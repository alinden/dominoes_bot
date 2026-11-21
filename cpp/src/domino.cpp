#include "dominoes/domino.hpp"

#include <algorithm>
#include <stdexcept>

namespace dominoes {

Domino::Domino(int first, int second)
    : high_(std::max(first, second)), low_(std::min(first, second)) {
    if (low_ < 0 || high_ > max_pip) {
        throw std::invalid_argument("domino pips must be between 0 and 6");
    }
}

const std::array<Domino, Domino::tile_count>& Domino::all() {
    static const std::array<Domino, tile_count> tiles{{
        {0, 0},
        {1, 0}, {1, 1},
        {2, 0}, {2, 1}, {2, 2},
        {3, 0}, {3, 1}, {3, 2}, {3, 3},
        {4, 0}, {4, 1}, {4, 2}, {4, 3}, {4, 4},
        {5, 0}, {5, 1}, {5, 2}, {5, 3}, {5, 4}, {5, 5},
        {6, 0}, {6, 1}, {6, 2}, {6, 3}, {6, 4}, {6, 5}, {6, 6},
    }};
    return tiles;
}

PlayedDomino::PlayedDomino(Domino tile, int exposed_pip)
    : tile_(tile), exposed_pip_(exposed_pip) {
    if (exposed_pip_ != tile_.high() && exposed_pip_ != tile_.low()) {
        throw std::invalid_argument("exposed pip must be an end of the domino");
    }
}

int PlayedDomino::score() const {
    return tile_.is_double() ? tile_.pip_total() : exposed_pip_;
}

std::optional<PlayedDomino> PlayedDomino::connect(Domino next) const {
    if (exposed_pip_ == next.high()) {
        return PlayedDomino(next, next.low());
    }
    if (exposed_pip_ == next.low()) {
        return PlayedDomino(next, next.high());
    }
    return std::nullopt;
}

PlayedDomino PlayedDomino::reversed() const {
    return PlayedDomino(tile_, tile_.pip_total() - exposed_pip_);
}

}  // namespace dominoes
