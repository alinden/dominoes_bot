#pragma once

#include "dominoes/search.hpp"

#include <cstdint>
#include <iosfwd>
#include <random>

namespace dominoes {

struct InferenceReport {
    std::size_t candidates = 0;
    std::size_t good = 0;
    std::size_t bad = 0;
    std::size_t forced = 0;
    bool too_many_hands = false;
    bool timed_out = false;
    bool incomplete = false;
    bool updated = false;
};

struct DecisionSamples {
    std::vector<SearchSample> worlds;
    bool all_hands = false;
    bool all_draw_orders = false;
};

void log_inference(std::ostream& log, const InferenceReport& report);

class HandCandidates {
public:
    // Initialize from a fresh deal before the first tile is played.
    HandCandidates(Seat observer, std::span<const Domino> own_hand, std::size_t opponent_size,
                   bool bayesian = false);

    // The position is immediately after the observed action, within the same round.
    void observe(const Game& position, const TurnResult& result);
    InferenceReport observe_with_inference(
        const Game& before, const Game& after, const TurnResult& result, std::mt19937& random,
        std::chrono::steady_clock::time_point deadline, std::size_t workers,
        const SettlementSearchOptions* neural_options = nullptr, std::size_t neural_hand_limit = 1000);
    std::vector<SearchSample> sample(const Game& position, std::size_t count,
                                     std::mt19937& random) const;
    DecisionSamples sample_for_search(const Game& position, std::size_t breadth,
                                      std::mt19937& random) const;
    void log_estimates(std::ostream& log, std::span<const Domino> actual_hand) const;
    std::vector<std::vector<Domino>> feasible_hands() const;
    std::vector<std::vector<Domino>> sample_distinct_hands(
        std::span<const Domino> included, std::size_t limit, std::mt19937& random) const;

    static constexpr std::size_t search_samples = 16;

private:
    struct Candidate {
        TileMask hand;
        double weight = 1.0;
    };

    std::discrete_distribution<std::size_t> weighted_picker() const;
    InferenceReport infer(const Game& before, Move observed, std::mt19937& random,
                          std::chrono::steady_clock::time_point deadline, std::size_t workers,
                          const SettlementSearchOptions* neural_options, std::size_t neural_hand_limit);
    Seat observer_;
    bool bayesian_;
    std::vector<Candidate> hands_;
};

}  // namespace dominoes
