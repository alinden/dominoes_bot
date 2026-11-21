#include "dominoes/search.hpp"

#include <algorithm>
#include <atomic>
#include <cmath>
#include <cstdint>
#include <exception>
#include <limits>
#include <map>
#include <stdexcept>
#include <thread>
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

double score_no_draws(const Game& game, Seat perspective, int remaining, double alpha, double beta,
                      bool guaranteed_settlement, SearchPass& pass) {
    const bool maximize = game.active_seat() == perspective;
    double best = maximize ? -1.0 : 1.0;
    for (const auto& action : game.legal_actions()) {
        pass.check_deadline();
        Game next = game;
        const auto result = next.apply(action).value();
        double score;
        if (remaining == 1 || result.round) {
            pass.reached_cutoff |= !result.round;
            score = evaluate_position(next, perspective, result.round);
        } else {
            // With a closed window, this branch is visited to check settlement.
            score = score_no_draws(next, perspective, remaining - 1,
                                   alpha < beta ? alpha : -1.0, alpha < beta ? beta : 1.0,
                                   guaranteed_settlement, pass);
        }
        if (maximize) {
            best = std::max(best, score);
            alpha = std::max(alpha, best);
        } else {
            best = std::min(best, score);
            beta = std::min(beta, best);
        }
        // A pruned branch could hide a depth cutoff. Skip it only if the pass has
        // already seen a cutoff or the available depth guarantees settlement.
        if (alpha >= beta && (pass.reached_cutoff || guaranteed_settlement)) break;
    }
    return best;
}

double score_continuation(const Outcomes& outcomes, Seat perspective, int remaining, SearchPass& pass) {
    const auto& game = outcomes.front().game;
    if (game.boneyard().empty()) {
        // Each nonfinal pass is followed by a play, and a lock leaves at least
        // two tiles unplayed. A full window returns an exact value to averaging.
        const int tiles = static_cast<int>(Domino::tile_count - game.board().size());
        const bool guaranteed_settlement = remaining >= 2 * (tiles - 1);
        return score_no_draws(game, perspective, remaining, -1.0, 1.0, guaranteed_settlement, pass);
    }
    const bool maximize = game.active_seat() == perspective;
    double best = maximize ? -std::numeric_limits<double>::infinity()
                           : std::numeric_limits<double>::infinity();
    for (const auto& action : game.legal_actions()) {
        const double score = score_action(outcomes, action, perspective, remaining - 1, pass);
        best = maximize ? std::max(best, score) : std::min(best, score);
    }
    return best;
}

double finish_draw(std::span<const Outcome* const> outcomes, std::size_t count,
                   Seat perspective, int remaining, SearchPass& pass) {
    if (remaining == 0) {
        pass.reached_cutoff = true;
        return evaluate_position(outcomes.front()->game, perspective);
    }
    Outcomes drawn;
    drawn.reserve(outcomes.size());
    for (auto outcome : outcomes) {
        drawn.push_back(*outcome);
        drawn.back().game.apply_search_draws(count);
    }
    return score_continuation(drawn, perspective, remaining, pass);
}

double score_draw_prefix(std::span<const Outcome* const> outcomes, TileMask playable,
                         std::size_t offset, Seat perspective, int remaining, SearchPass& pass) {
    pass.check_deadline();
    const auto boneyard_size = outcomes.front()->game.boneyard().size();
    if (outcomes.size() == 1) {
        const auto& boneyard = outcomes.front()->game.boneyard();
        std::size_t count = offset + 1;
        while (remaining != 0 && count < boneyard_size && !(playable & tile_bit(boneyard[count - 1]))) {
            ++count;
        }
        return finish_draw(outcomes, count, perspective, remaining, pass);
    }

    struct Branch {
        std::vector<const Outcome*> outcomes;
        double weight = 0.0;
    };
    // Retain the ordered draw-prefix groups and their weighted sums without copying intermediate games.
    std::map<std::size_t, Branch> by_draw;
    double total_weight = 0.0;
    for (auto outcome : outcomes) {
        auto& branch = by_draw[outcome->game.boneyard()[offset].id()];
        branch.outcomes.push_back(outcome);
        branch.weight += outcome->weight;
        total_weight += outcome->weight;
    }

    const std::size_t count = offset + 1;
    double score = 0.0;
    for (const auto& [tile, branch] : by_draw) {
        const bool draw_again = remaining != 0 && count < boneyard_size && !(playable & (TileMask{1} << tile));
        const double branch_score = draw_again
            ? score_draw_prefix(branch.outcomes, playable, count, perspective, remaining, pass)
            : finish_draw(branch.outcomes, count, perspective, remaining, pass);
        score += (branch.weight / total_weight) * branch_score;
    }
    return score;
}

double score_draw(const Outcomes& outcomes, Seat perspective, int remaining, SearchPass& pass) {
    TileMask playable = 0;
    for (auto mask : outcomes.front().game.board().playable_masks()) {
        playable |= mask;
    }
    if (outcomes.size() == 1) {
        const Outcome* only = &outcomes.front();
        return score_draw_prefix({&only, 1}, playable, 0, perspective, remaining, pass);
    }
    std::vector<const Outcome*> pending;
    pending.reserve(outcomes.size());
    for (const auto& outcome : outcomes) {
        pending.push_back(&outcome);
    }
    return score_draw_prefix(pending, playable, 0, perspective, remaining, pass);
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

#if !defined(__EMSCRIPTEN__) || defined(__EMSCRIPTEN_PTHREADS__)
struct TaskScore {
    double score = 0.0;
    bool reached_cutoff = false;
};

std::vector<ScoredAction> score_root_parallel(
    const std::vector<Action>& actions, Seat perspective, int depth, const HandOutcomes& groups,
    double total_weight, SearchPass& pass, std::size_t workers) {
    std::vector<const Outcomes*> hands;
    std::vector<double> weights;
    for (const auto& [hand, outcomes] : groups) {
        hands.push_back(&outcomes);
        weights.push_back(weight_of(outcomes) / total_weight);
    }

    // Interleave moves across hand groups and let each idle worker claim the next task.
    std::vector<TaskScore> results(actions.size() * hands.size());
    const auto worker_count = std::min(workers, results.size());
    std::vector<std::exception_ptr> errors(worker_count);
    std::atomic<std::size_t> next_task{0};
    std::atomic<bool> stopped{false};
    const auto work = [&](std::size_t worker) {
        try {
            while (!stopped.load(std::memory_order_relaxed)) {
                const auto index = next_task.fetch_add(1, std::memory_order_relaxed);
                if (index >= results.size()) {
                    return;
                }
                const auto hand = index / actions.size();
                const auto action = index % actions.size();
                SearchPass task_pass{pass.deadline};
                const double score = score_action(*hands[hand], actions[action], perspective,
                                                  depth - 1, task_pass);
                results[index] = {score, task_pass.reached_cutoff};
            }
        } catch (...) {
            errors[worker] = std::current_exception();
            stopped.store(true, std::memory_order_relaxed);
        }
    };

    {
        std::vector<std::jthread> threads;
        threads.reserve(worker_count - 1);
        try {
            for (std::size_t worker = 1; worker < worker_count; ++worker) {
                threads.emplace_back(work, worker);
            }
        } catch (...) {
            stopped.store(true, std::memory_order_relaxed);
            throw;
        }
        work(0);
    } // All workers must leave a timed-out pass before the caller can use its fallback.

    for (const auto& error : errors) {
        if (error) std::rethrow_exception(error);
    }
    std::vector<ScoredAction> scores;
    scores.reserve(actions.size());
    for (std::size_t action = 0; action < actions.size(); ++action) {
        double score = 0.0;
        for (std::size_t hand = 0; hand < hands.size(); ++hand) {
            const auto& result = results[hand * actions.size() + action];
            score += weights[hand] * result.score;
            pass.reached_cutoff |= result.reached_cutoff;
        }
        scores.push_back({actions[action], score});
    }
    return scores;
}

#endif

std::vector<ScoredAction> score_root(const Game& position, Seat perspective, int depth,
                                    const HandOutcomes& groups, double total_weight, SearchPass& pass,
                                    std::size_t workers = 1) {
    const auto actions = position.legal_actions();
#if !defined(__EMSCRIPTEN__) || defined(__EMSCRIPTEN_PTHREADS__)
    if (workers > 1 && actions.size() * groups.size() > 1) {
        return score_root_parallel(actions, perspective, depth, groups, total_weight, pass, workers);
    }
#else
    (void)workers;
#endif
    std::vector<ScoredAction> scores;
    for (const auto& action : actions) {
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
    const Game& position, Seat perspective, int depth, std::span<const SearchSample> samples,
    std::size_t workers) {
    if (workers == 0) throw std::invalid_argument("worker count must be positive");
    const auto [groups, total_weight] = prepare_samples(position, perspective, depth, samples);
    SearchPass pass;
    return score_root(position, perspective, depth, groups, total_weight, pass, workers);
}

TimedSearchResult search_until(
    const Game& position, Seat perspective, int max_depth, std::span<const SearchSample> samples,
    std::chrono::steady_clock::time_point deadline, std::size_t workers) {
    if (workers == 0) throw std::invalid_argument("worker count must be positive");
    const auto [groups, total_weight] = prepare_samples(position, perspective, max_depth, samples);
    TimedSearchResult last;
    try {
        for (int depth = 1; ; depth += std::min(2, max_depth - depth)) {
            SearchPass pass{deadline};
            pass.check_deadline();
            auto scores = score_root(position, perspective, depth, groups, total_weight, pass, workers);
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

TimedSearchResult search_to_round_end(
    const Game& position, Seat perspective, std::span<const SearchSample> samples,
    std::chrono::steady_clock::time_point deadline, std::size_t workers) {
    if (workers == 0) throw std::invalid_argument("worker count must be positive");
    // A nonfinal pass must be followed by a play. A lock needs two final passes and
    // leaves at least two tiles unplayed. Each remaining draw step consumes a tile.
    const int depth = 2 * (static_cast<int>(Domino::tile_count - position.board().size()) - 1) +
                      static_cast<int>(position.boneyard().size());
    SearchPass pass{deadline};
    try {
        pass.check_deadline();
        const auto [groups, total_weight] = prepare_samples(position, perspective, depth, samples);
        auto scores = score_root(position, perspective, depth, groups, total_weight, pass, workers);
        pass.check_deadline();
        return {std::move(scores), depth, !pass.reached_cutoff, false};
    } catch (const SearchExpired&) {
        TimedSearchResult result;
        result.timed_out = true;
        return result;
    }
}

}  // namespace dominoes
