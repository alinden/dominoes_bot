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

int settlement_depth(const Game& position) {
    // Nonfinal passes precede plays, a lock leaves at least two tiles unplayed,
    // and each draw consumes a tile.
    return 2 * (static_cast<int>(Domino::tile_count - position.board().size()) - 1) +
           static_cast<int>(position.boneyard().size());
}

namespace {

struct Outcome {
    Game game;
    double weight;
};

// Outcomes in one group share the hands, board, and revealed draws. Only the remaining order varies.
using Outcomes = std::vector<Outcome>;

struct SearchExpired {};

struct Window {
    double alpha = -std::numeric_limits<double>::infinity();
    double beta = std::numeric_limits<double>::infinity();

    bool bounded() const { return std::isfinite(alpha) || std::isfinite(beta); }
};

struct SearchPass {
    std::optional<std::chrono::steady_clock::time_point> deadline;
    bool reached_cutoff = false;
    const MoveOrder* order = nullptr;
    bool prune_before_draws = false;
    bool measure_work = false;
    SearchWork work{};
    std::size_t order_min_boneyard = 0;
    std::span<const OrderingPhase> phases{};
    bool guaranteed_settlement = false;
    std::size_t leaf_tiles = 0;
    const MoveOrder* leaf_value = nullptr;

    bool may_prune() const { return guaranteed_settlement || reached_cutoff; }

    bool at_leaf(const Game& game) const {
        return leaf_value && leaf_tiles != 0 && game.board().size() >= leaf_tiles;
    }

    double evaluate_leaf(const Game& game, Seat mover, Seat perspective) {
        reached_cutoff = true;
        if (!leaf_value) return evaluate_position(game, perspective);
        const double value = (*leaf_value)(game, mover);
        return mover == perspective ? value : -value;
    }

    const MoveOrder* order_for(const Game& game) const {
        const auto draws = game.boneyard().size();
        if (phases.empty()) return draws >= order_min_boneyard ? order : nullptr;
        const auto placed = game.board().size();
        for (const auto& phase : phases) {
            if (phase.order && placed >= phase.first_placed && placed <= phase.last_placed &&
                draws >= phase.min_boneyard && draws <= phase.max_boneyard) {
                return &phase.order;
            }
        }
        return nullptr;
    }

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
                    SearchPass& pass, Window window = {});

double score_no_draws(const Game& game, Seat perspective, int remaining, double alpha, double beta,
                      bool guaranteed_settlement, SearchPass& pass) {
    const bool maximize = game.active_seat() == perspective;
    double best = maximize ? -1.0 : 1.0;
    const auto actions = game.legal_actions();
    const auto visit = [&](const Game& next, const std::optional<RoundResult>& round) {
        if (pass.measure_work) ++pass.work.nodes;
        pass.check_deadline();
        double score;
        if (round) {
            score = evaluate_position(next, perspective, round);
        } else if (remaining == 1 || pass.at_leaf(next)) {
            score = pass.evaluate_leaf(next, game.active_seat().value(), perspective);
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
        return alpha >= beta && (pass.reached_cutoff || guaranteed_settlement);
    };
    const auto* order = pass.order_for(game);
    if (order && remaining > 1 && actions.size() > 1 &&
        game.player(Seat::first).hand.size() + game.player(Seat::second).hand.size() >= 6) {
        struct Child {
            Game game;
            std::optional<RoundResult> round;
            double priority;
        };
        std::vector<Child> children;
        children.reserve(actions.size());
        const auto mover = game.active_seat().value();
        for (const auto& action : actions) {
            pass.check_deadline();
            Game next = game;
            const auto round = next.apply(action).value().round;
            const auto priority = round ? evaluate_position(next, mover, round) : (*order)(next, mover);
            children.push_back({std::move(next), round, priority});
        }
        std::ranges::stable_sort(children, std::ranges::greater{}, &Child::priority);
        for (const auto& child : children) {
            if (visit(child.game, child.round)) break;
        }
    } else {
        for (const auto& action : actions) {
            Game next = game;
            const auto round = next.apply(action).value().round;
            if (visit(next, round)) break;
        }
    }
    return best;
}

double score_continuation(const Outcomes& outcomes, Seat perspective, int remaining, SearchPass& pass,
                          Window window) {
    const auto& game = outcomes.front().game;
    if (game.boneyard().empty()) {
        // Each nonfinal pass is followed by a play, and a lock leaves at least
        // two tiles unplayed. The caller supplies a full window when it needs an exact value.
        const int tiles = static_cast<int>(Domino::tile_count - game.board().size());
        const bool guaranteed_settlement = remaining >= 2 * (tiles - 1);
        return score_no_draws(game, perspective, remaining, std::max(-1.0, window.alpha),
                              std::min(1.0, window.beta), guaranteed_settlement, pass);
    }
    const bool maximize = game.active_seat() == perspective;
    double best = maximize ? -std::numeric_limits<double>::infinity()
                           : std::numeric_limits<double>::infinity();
    auto actions = game.legal_actions();
    const auto* order = pass.order_for(game);
    if (pass.prune_before_draws && order && actions.size() > 1) {
        // Draw permutations in this group share all inputs to the orderer.
        std::vector<std::pair<Action, double>> ranked;
        ranked.reserve(actions.size());
        const auto mover = game.active_seat().value();
        for (const auto& action : actions) {
            pass.check_deadline();
            auto child = game;
            const auto round = child.apply(action).value().round;
            ranked.emplace_back(action, round ? evaluate_position(child, mover, round) : (*order)(child, mover));
        }
        std::ranges::stable_sort(ranked, std::ranges::greater{}, &std::pair<Action, double>::second);
        for (std::size_t i = 0; i < actions.size(); ++i) actions[i] = std::move(ranked[i].first);
    }
    for (const auto& action : actions) {
        // If reporting an exact settlement status still requires visiting a
        // skipped branch, search it without an already-closed window.
        const auto child_window = window.alpha < window.beta ? window : Window{};
        const double score = score_action(outcomes, action, perspective, remaining - 1, pass, child_window);
        best = maximize ? std::max(best, score) : std::min(best, score);
        if (pass.prune_before_draws) {
            if (maximize) window.alpha = std::max(window.alpha, best);
            else window.beta = std::min(window.beta, best);
            if (window.alpha >= window.beta && pass.may_prune()) break;
        }
    }
    return best;
}

double finish_draw(std::span<const Outcome* const> outcomes, std::size_t count,
                   Seat perspective, int remaining, SearchPass& pass, Window window) {
    if (remaining == 0) {
        const auto& game = outcomes.front()->game;
        const auto previous_player = game.active_seat() == Seat::first ? Seat::second : Seat::first;
        return pass.evaluate_leaf(game, previous_player, perspective);
    }
    Outcomes drawn;
    drawn.reserve(outcomes.size());
    for (auto outcome : outcomes) {
        drawn.push_back(*outcome);
        drawn.back().game.apply_search_draws(count);
    }
    return score_continuation(drawn, perspective, remaining, pass, window);
}

double score_draw_prefix(std::span<const Outcome* const> outcomes, TileMask playable,
                         std::size_t offset, Seat perspective, int remaining, SearchPass& pass, Window window) {
    pass.check_deadline();
    if (pass.measure_work) ++pass.work.nodes;
    const auto boneyard_size = outcomes.front()->game.boneyard().size();
    if (outcomes.size() == 1) {
        const auto& boneyard = outcomes.front()->game.boneyard();
        std::size_t count = offset + 1;
        while (remaining != 0 && count < boneyard_size && !(playable & tile_bit(boneyard[count - 1]))) {
            ++count;
        }
        return finish_draw(outcomes, count, perspective, remaining, pass, window);
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
    if (by_draw.size() == 1 && pass.prune_before_draws) {
        const auto& [tile, branch] = *by_draw.begin();
        const bool draw_again = remaining != 0 && count < boneyard_size && !(playable & (TileMask{1} << tile));
        return draw_again ? score_draw_prefix(branch.outcomes, playable, count, perspective, remaining, pass, window)
                          : finish_draw(branch.outcomes, count, perspective, remaining, pass, window);
    }
    double score = 0.0;
    double seen_weight = 0.0;
    auto branches_left = by_draw.size();
    const bool bounded = pass.prune_before_draws && window.bounded();
    for (const auto& [tile, branch] : by_draw) {
        const bool draw_again = remaining != 0 && count < boneyard_size && !(playable & (TileMask{1} << tile));
        const double branch_score = draw_again
            ? score_draw_prefix(branch.outcomes, playable, count, perspective, remaining, pass, {})
            : finish_draw(branch.outcomes, count, perspective, remaining, pass, {});
        score += (branch.weight / total_weight) * branch_score;
        if (bounded && --branches_left > 0) {
            seen_weight += branch.weight;
            // Each unsearched outcome is in [-1, 1]. Widen the interval
            // slightly to cover rounding in the weighted sum and remaining mass.
            const double radius = std::max(0.0, 1.0 - seen_weight / total_weight) + 1e-12;
            if (pass.may_prune() && score + radius <= window.alpha) {
                if (pass.measure_work) ++pass.work.draw_cutoffs;
                return score + radius;
            }
            if (pass.may_prune() && score - radius >= window.beta) {
                if (pass.measure_work) ++pass.work.draw_cutoffs;
                return score - radius;
            }
        }
    }
    return score;
}

double score_draw(const Outcomes& outcomes, Seat perspective, int remaining, SearchPass& pass, Window window) {
    TileMask playable = 0;
    for (auto mask : outcomes.front().game.board().playable_masks()) {
        playable |= mask;
    }
    if (outcomes.size() == 1) {
        const Outcome* only = &outcomes.front();
        return score_draw_prefix({&only, 1}, playable, 0, perspective, remaining, pass, window);
    }
    std::vector<const Outcome*> pending;
    pending.reserve(outcomes.size());
    for (const auto& outcome : outcomes) {
        pending.push_back(&outcome);
    }
    return score_draw_prefix(pending, playable, 0, perspective, remaining, pass, window);
}

double score_action(const Outcomes& outcomes, const Action& action, Seat perspective, int remaining,
                    SearchPass& pass, Window window) {
    pass.check_deadline();
    if (pass.measure_work) ++pass.work.nodes;
    if (std::holds_alternative<Draw>(action)) {
        return score_draw(outcomes, perspective, remaining, pass, window);
    }
    Outcomes next = outcomes;
    const auto result = next.front().game.apply(action).value();
    if (result.round) {
        return evaluate_position(next.front().game, perspective, result.round);
    }
    if (remaining == 0 || pass.at_leaf(next.front().game)) {
        return pass.evaluate_leaf(next.front().game, outcomes.front().game.active_seat().value(), perspective);
    }
    for (std::size_t i = 1; i < next.size(); ++i) {
        next[i].game.apply(action).value();
    }
    return score_continuation(next, perspective, remaining, pass, window);
}

std::uint32_t hand_mask(std::span<const Domino> hand) {
    std::uint32_t mask = 0;
    for (auto tile : hand) {
        mask |= std::uint32_t{1} << tile.id();
    }
    return mask;
}

using HandOutcomes = std::map<std::uint32_t, Outcomes>;

struct TaskScore {
    double score = 0.0;
    bool reached_cutoff = false;
    SearchWork work{};
};

std::vector<TaskScore> run_root_tasks(
    const std::vector<Action>& actions, Seat perspective, int depth, std::span<const Outcomes* const> hands,
    const SearchPass& pass, std::size_t workers) {
    // Interleave moves across hand groups and let each idle worker claim the next task.
    std::vector<TaskScore> results(actions.size() * hands.size());
    if (results.empty()) return results;
    const auto evaluate = [&](std::size_t index) {
        const auto hand = index / actions.size();
        const auto action = index % actions.size();
        auto task_pass = pass;
        const double score = score_action(*hands[hand], actions[action], perspective, depth - 1, task_pass);
        results[index] = {score, task_pass.reached_cutoff, task_pass.work};
    };
#if !defined(__EMSCRIPTEN__) || defined(__EMSCRIPTEN_PTHREADS__)
    const auto worker_count = std::min(workers, results.size());
    if (worker_count == 1) {
        for (std::size_t index = 0; index < results.size(); ++index) evaluate(index);
        return results;
    }
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
                evaluate(index);
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
#else
    (void)workers;
    for (std::size_t index = 0; index < results.size(); ++index) evaluate(index);
#endif
    return results;
}

#if !defined(__EMSCRIPTEN__) || defined(__EMSCRIPTEN_PTHREADS__)
std::vector<ScoredAction> score_root_parallel(
    const std::vector<Action>& actions, Seat perspective, int depth, const HandOutcomes& groups,
    double total_weight, SearchPass& pass, std::size_t workers) {
    std::vector<const Outcomes*> hands;
    std::vector<double> weights;
    for (const auto& [hand, outcomes] : groups) {
        hands.push_back(&outcomes);
        weights.push_back(weight_of(outcomes) / total_weight);
    }
    const auto results = run_root_tasks(actions, perspective, depth, hands, pass, workers);
    std::vector<ScoredAction> scores;
    scores.reserve(actions.size());
    for (std::size_t action = 0; action < actions.size(); ++action) {
        double score = 0.0;
        for (std::size_t hand = 0; hand < hands.size(); ++hand) {
            const auto& result = results[hand * actions.size() + action];
            score += weights[hand] * result.score;
            pass.reached_cutoff |= result.reached_cutoff;
            pass.work.nodes += result.work.nodes;
            pass.work.draw_cutoffs += result.work.draw_cutoffs;
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

SearchPass configured_pass(std::optional<std::chrono::steady_clock::time_point> deadline,
                           const SettlementSearchOptions& options, bool guaranteed_settlement) {
    SearchPass pass{deadline};
    if (options.order) pass.order = &options.order;
    pass.prune_before_draws = options.prune_before_draws;
    pass.measure_work = options.measure_work;
    pass.order_min_boneyard = options.order_min_boneyard;
    pass.phases = options.phases;
    pass.guaranteed_settlement = guaranteed_settlement;
    pass.leaf_tiles = options.leaf_tiles;
    if (options.leaf_value) pass.leaf_value = &options.leaf_value;
    return pass;
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
    std::size_t workers, const SettlementSearchOptions& options) {
    if (workers == 0) throw std::invalid_argument("worker count must be positive");
    const auto [groups, total_weight] = prepare_samples(position, perspective, depth, samples);
    auto pass = configured_pass(std::nullopt, options, depth >= settlement_depth(position));
    return score_root(position, perspective, depth, groups, total_weight, pass, workers);
}

TimedSearchResult search_until(
    const Game& position, Seat perspective, int max_depth, std::span<const SearchSample> samples,
    std::chrono::steady_clock::time_point deadline, std::size_t workers, const SettlementSearchOptions& options) {
    if (workers == 0) throw std::invalid_argument("worker count must be positive");
    const auto [groups, total_weight] = prepare_samples(position, perspective, max_depth, samples);
    TimedSearchResult last;
    try {
        for (int depth = 1; ; depth += std::min(2, max_depth - depth)) {
            auto pass = configured_pass(deadline, options, depth >= settlement_depth(position));
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
    std::chrono::steady_clock::time_point deadline, std::size_t workers, const SettlementSearchOptions& options) {
    if (workers == 0) throw std::invalid_argument("worker count must be positive");
    const int depth = settlement_depth(position);
    auto pass = configured_pass(deadline, options, true);
    try {
        pass.check_deadline();
        const auto [groups, total_weight] = prepare_samples(position, perspective, depth, samples);
        auto scores = score_root(position, perspective, depth, groups, total_weight, pass, workers);
        pass.check_deadline();
        return {std::move(scores), depth, !pass.reached_cutoff, false, pass.work};
    } catch (const SearchExpired&) {
        TimedSearchResult result;
        result.timed_out = true;
        result.work = pass.work;
        return result;
    }
}

std::vector<TimedSearchResult> search_hands_to_round_end(
    const Game& position, Seat perspective, std::span<const std::vector<SearchSample>> hand_samples,
    std::chrono::steady_clock::time_point deadline, std::size_t workers, const SettlementSearchOptions& options) {
    if (workers == 0 || hand_samples.empty()) throw std::invalid_argument("hand batch requires workers and samples");
    const auto depth = settlement_depth(position);
    const auto pass = configured_pass(deadline, options, true);
    try {
        std::vector<Outcomes> groups;
        groups.reserve(hand_samples.size());
        for (const auto& samples : hand_samples) {
            pass.check_deadline();
            auto prepared = prepare_samples(position, perspective, depth, samples);
            if (prepared.first.size() != 1) throw std::invalid_argument("expected one opponent hand per sample group");
            groups.push_back(std::move(prepared.first.begin()->second));
        }
        std::vector<const Outcomes*> hands;
        for (const auto& group : groups) hands.push_back(&group);
        const auto actions = position.legal_actions();
        const auto tasks = run_root_tasks(actions, perspective, depth, hands, pass, workers);
        pass.check_deadline();
        std::vector<TimedSearchResult> results;
        for (std::size_t hand = 0; hand < hands.size(); ++hand) {
            TimedSearchResult result{{}, depth, true};
            for (std::size_t action = 0; action < actions.size(); ++action) {
                const auto& task = tasks[hand * actions.size() + action];
                result.scores.push_back({actions[action], task.score});
                result.reached_round_end &= !task.reached_cutoff;
                result.work.nodes += task.work.nodes;
                result.work.draw_cutoffs += task.work.draw_cutoffs;
            }
            results.push_back(std::move(result));
        }
        return results;
    } catch (const SearchExpired&) {
        return {};
    }
}

}  // namespace dominoes
