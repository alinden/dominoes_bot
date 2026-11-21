#pragma once

#include "dominoes/search.hpp"

#include <cstdint>
#include <random>

namespace dominoes {

class HandCandidates {
public:
    // Initialize from a fresh deal before the first tile is played.
    HandCandidates(Seat observer, std::span<const Domino> own_hand, std::size_t opponent_size);

    // The position is immediately after the observed action, within the same round.
    void observe(const Game& position, const TurnResult& result);
    std::vector<SearchSample> sample(const Game& position, std::size_t count,
                                     std::mt19937& random) const;
    std::vector<ScoredAction> score_moves(const Game& position, int depth, std::mt19937& random) const;

    static constexpr std::size_t search_samples = 16;

private:
    Seat observer_;
    std::vector<std::uint32_t> hands_;
};

}  // namespace dominoes
