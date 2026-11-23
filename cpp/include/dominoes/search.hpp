#pragma once

#include "dominoes/game.hpp"

#include <chrono>
#include <functional>

namespace dominoes {

struct ScoredAction {
    Action action;
    double score;
};

struct SearchSample {
    std::vector<Domino> opponent_hand;
    std::vector<Domino> draw_order;
    double weight;
};

struct SearchWork {
    std::size_t nodes = 0;
    std::size_t draw_cutoffs = 0;
};

struct TimedSearchResult {
    std::vector<ScoredAction> scores;
    int depth = 0;
    bool reached_round_end = false;
    bool timed_out = false;
    SearchWork work{};
};

double evaluate_position(const Game& game, Seat perspective,
                         std::optional<RoundResult> round = std::nullopt);

// Both hands and the ordered boneyard belong to one fixed scenario.
std::vector<ScoredAction> score_fixed_actions(const Game& scenario, Seat perspective, int depth);

// Ranks child positions from the mover's perspective.
using MoveOrder = std::function<double(const Game&, Seat)>;

struct OrderingPhase {
    MoveOrder order;
    std::size_t first_placed;
    std::size_t last_placed;
    std::size_t min_boneyard;
    std::size_t max_boneyard;
};

struct SettlementSearchOptions {
    MoveOrder order;
    bool prune_before_draws = true;
    bool measure_work = false;
    std::size_t order_min_boneyard = 0;
    // If phases are supplied, unmatched positions keep the original move order.
    std::span<const OrderingPhase> phases{};
    // Evaluate nonterminal children when a move reaches this board size.
    // Zero uses the evaluator only at the depth limit.
    std::size_t leaf_tiles = 0;
    MoveOrder leaf_value{};
};

int settlement_depth(const Game& position);

// Score one root action across weighted hidden hands and scripted draw orders.
// The perspective must be the active seat. Weights are relative and strictly positive.
std::vector<ScoredAction> score_sampled_actions(
    const Game& position, Seat perspective, int depth, std::span<const SearchSample> samples,
    std::size_t workers = 1, const SettlementSearchOptions& options = {{}, false});

TimedSearchResult search_until(
    const Game& position, Seat perspective, int max_depth, std::span<const SearchSample> samples,
    std::chrono::steady_clock::time_point deadline, std::size_t workers = 1,
    const SettlementSearchOptions& options = {{}, false});

// One depth pass bounded by the remaining round, with no partial scores on timeout.
TimedSearchResult search_to_round_end(
    const Game& position, Seat perspective, std::span<const SearchSample> samples,
    std::chrono::steady_clock::time_point deadline, std::size_t workers = 1,
    const SettlementSearchOptions& options = {});

// Each sample group represents one opponent hand. Results preserve group and
// legal move order. A timeout discards the whole batch and returns no results.
std::vector<TimedSearchResult> search_hands_to_round_end(
    const Game& position, Seat perspective, std::span<const std::vector<SearchSample>> hand_samples,
    std::chrono::steady_clock::time_point deadline, std::size_t workers,
    const SettlementSearchOptions& options = {});

}  // namespace dominoes
