#pragma once

#include "dominoes/game.hpp"

#include <chrono>

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

struct TimedSearchResult {
    std::vector<ScoredAction> scores;
    int depth = 0;
    bool reached_round_end = false;
    bool timed_out = false;
};

double evaluate_position(const Game& game, Seat perspective,
                         std::optional<RoundResult> round = std::nullopt);

// Both hands and the ordered boneyard belong to one fixed scenario.
std::vector<ScoredAction> score_fixed_actions(const Game& scenario, Seat perspective, int depth);

// Score one root action across weighted hidden hands and scripted draw orders.
// The perspective must be the active seat. Weights are relative and strictly positive.
std::vector<ScoredAction> score_sampled_actions(
    const Game& position, Seat perspective, int depth, std::span<const SearchSample> samples);

TimedSearchResult search_until(
    const Game& position, Seat perspective, int max_depth, std::span<const SearchSample> samples,
    std::chrono::steady_clock::time_point deadline);

}  // namespace dominoes
