#pragma once

#include "dominoes/search.hpp"
#include "dominoes/value_order.hpp"

namespace dominoes {

struct NeuralSearch {
    ValueOrder opening;
    ValueOrder midgame;
    ValueOrder late;

    std::array<OrderingPhase, 3> phases() const {
        return {{{std::cref(opening), 0, 5, 0, 14},
                 {std::cref(midgame), 6, 12, 8, 14},
                 {std::cref(late), 13, 27, 4, 6}}};
    }

    SettlementSearchOptions options(std::span<const OrderingPhase> ordering) const {
        SettlementSearchOptions result{{}, true, false, 0, ordering};
        result.leaf_value = [this](const Game& leaf, Seat mover) {
            const auto tiles = leaf.board().size();
            const auto& model = tiles < 6 ? opening : tiles < 13 ? midgame : late;
            return model(leaf, mover);
        };
        return result;
    }

    static int max_depth(const Game& position) {
        return position.board().size() < 6 ? 15 : settlement_depth(position);
    }
};

}  // namespace dominoes
