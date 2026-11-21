#include "dominoes/search.hpp"

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <limits>
#include <map>
#include <stdexcept>
#include <utility>

namespace dominoes {

double evaluate_position(const Game& game, Seat perspective, std::optional<RoundResult> round) {
    if (const auto winner = game.winner()) {
        return *winner == perspective ? 1.0 : -1.0;
    }

    const auto opponent = perspective == Seat::first ? Seat::second : Seat::first;
    const int own_score = game.player(perspective).score;
    const int opposing_score = game.player(opponent).score;
    double lead = own_score - opposing_score;
    if (round && round->reason == RoundEnd::dominoes) {
        lead += game.player(perspective).hand.empty() ? 10.0 : -10.0;
    }
    const double runway = std::max(30.0, 150.0 - std::max(own_score, opposing_score));
    return std::tanh(lead / runway);
}

namespace {

struct Outcome {
    Game game;
    double weight;
};

// Outcomes in one group share the hands, board, and revealed draws. Only the remaining order varies.
using Outcomes = std::vector<Outcome>;

struct SearchExpired {};

struct SearchPass {
    std::optional<std::chrono::steady_clock::time_point> deadline;
    bool reached_cutoff = false;

    void check_deadline() const {
        if (deadline && std::chrono::steady_clock::now() >= *deadline) {
            throw SearchExpired{};
        }
    }
};

double weight_of(const Outcomes& outcomes) {
    double weight = 0.0;
    for (const auto& outcome : outcomes) {
        weight += outcome.weight;
    }
    return weight;
}

double score_action(const Outcomes& outcomes, const Action& action, Seat perspective, int remaining,
                    SearchPass& pass);

double score_continuation(const Outcomes& outcomes, Seat perspective, int remaining, SearchPass& pass) {
    const auto& game = outcomes.front().game;
    const bool maximize = game.active_seat() == perspective;
    double best = maximize ? -std::numeric_limits<double>::infinity()
                           : std::numeric_limits<double>::infinity();
    for (const auto& action : game.legal_actions()) {
        const double score = score_action(outcomes, action, perspective, remaining - 1, pass);
        best = maximize ? std::max(best, score) : std::min(best, score);
    }
    return best;
}

double score_draw(const Outcomes& outcomes, Seat perspective, int remaining, SearchPass& pass) {
    pass.check_deadline();
    std::map<std::size_t, Outcomes> by_draw;
    for (const auto& outcome : outcomes) {
        Outcome drawn = outcome;
        const Domino tile = drawn.game.boneyard().front();
        drawn.game.apply(Draw{}, DrawEvidence::omit).value();
        by_draw[tile.id()].push_back(std::move(drawn));
    }

    double score = 0.0;
    const double total_weight = weight_of(outcomes);
    for (const auto& [tile, branch] : by_draw) {
        const auto& game = branch.front().game;
        double branch_score;
        if (remaining == 0) {
            pass.reached_cutoff = true;
            branch_score = evaluate_position(game, perspective);
        } else if (!game.boneyard().empty() && game.legal_moves().empty()) {
            // A forced sequence of draws shares one search ply.
            branch_score = score_draw(branch, perspective, remaining, pass);
        } else {
            branch_score = score_continuation(branch, perspective, remaining, pass);
        }
        score += (weight_of(branch) / total_weight) * branch_score;
    }
    return score;
}

double score_action(const Outcomes& outcomes, const Action& action, Seat perspective, int remaining,
                    SearchPass& pass) {
    pass.check_deadline();
    if (std::holds_alternative<Draw>(action)) {
        return score_draw(outcomes, perspective, remaining, pass);
    }
    Outcomes next = outcomes;
    const auto result = next.front().game.apply(action).value();
    if (remaining == 0 || result.round) {
        pass.reached_cutoff |= !result.round;
        return evaluate_position(next.front().game, perspective, result.round);
    }
    for (std::size_t i = 1; i < next.size(); ++i) {
        next[i].game.apply(action).value();
    }
    return score_continuation(next, perspective, remaining, pass);
}

std::uint32_t hand_mask(std::span<const Domino> hand) {
    std::uint32_t mask = 0;
    for (auto tile : hand) {
        mask |= std::uint32_t{1} << tile.id();
    }
    return mask;
}

using HandOutcomes = std::map<std::uint32_t, Outcomes>;

std::vector<ScoredAction> score_root(const Game& position, Seat perspective, int depth,
                                    const HandOutcomes& groups, double total_weight, SearchPass& pass) {
    std::vector<ScoredAction> scores;
    for (const auto& action : position.legal_actions()) {
        double score = 0.0;
        for (const auto& [hand, outcomes] : groups) {
            score += (weight_of(outcomes) / total_weight) *
                     score_action(outcomes, action, perspective, depth - 1, pass);
        }
        scores.push_back({action, score});
    }
    return scores;
}

std::pair<HandOutcomes, double> prepare_samples(
    const Game& position, Seat perspective, int depth, std::span<const SearchSample> samples) {
    if (depth < 1 || position.active_seat() != perspective || samples.empty()) {
        throw std::invalid_argument("sampled search requires positive depth, an active seat and samples");
    }

    HandOutcomes groups;
    double total_weight = 0.0;
    for (const auto& sample : samples) {
        auto game = position.with_hidden_tiles(perspective, sample.opponent_hand, sample.draw_order);
        if (!game || !std::isfinite(sample.weight) || sample.weight <= 0.0) {
            throw std::invalid_argument("invalid hidden tiles or sample weight");
        }
        total_weight += sample.weight;
        groups[hand_mask(sample.opponent_hand)].push_back({std::move(*game), sample.weight});
    }
    if (!std::isfinite(total_weight)) {
        throw std::invalid_argument("sample weight sum must be finite");
    }
    return {std::move(groups), total_weight};
}

}  // namespace

std::vector<ScoredAction> score_fixed_actions(const Game& scenario, Seat perspective, int depth) {
    if (depth < 1) {
        throw std::invalid_argument("search depth must be positive");
    }
    if (scenario.phase() != Phase::playing) {
        return {};
    }
    SearchPass pass;
    return score_root(scenario, perspective, depth, {{0, {{scenario, 1.0}}}}, 1.0, pass);
}

std::vector<ScoredAction> score_sampled_actions(
    const Game& position, Seat perspective, int depth, std::span<const SearchSample> samples) {
    const auto [groups, total_weight] = prepare_samples(position, perspective, depth, samples);
    SearchPass pass;
    return score_root(position, perspective, depth, groups, total_weight, pass);
}

TimedSearchResult search_until(
    const Game& position, Seat perspective, int max_depth, std::span<const SearchSample> samples,
    std::chrono::steady_clock::time_point deadline) {
    const auto [groups, total_weight] = prepare_samples(position, perspective, max_depth, samples);
    TimedSearchResult last;
    try {
        for (int depth = 1; ; depth += std::min(2, max_depth - depth)) {
            SearchPass pass{deadline};
            pass.check_deadline();
            auto scores = score_root(position, perspective, depth, groups, total_weight, pass);
            pass.check_deadline();
            last = {std::move(scores), depth, !pass.reached_cutoff, false};
            if (last.reached_round_end || depth == max_depth) {
                return last;
            }
        }
    } catch (const SearchExpired&) {
        last.timed_out = true;
        return last;
    }
}

}  // namespace dominoes
