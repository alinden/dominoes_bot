#include "dominoes/domino.hpp"

#include <iostream>
#include <stdexcept>

using dominoes::Domino;
using dominoes::PlayedDomino;

namespace {

void check(bool condition, const char* message) {
    if (!condition) {
        throw std::runtime_error(message);
    }
}

void check_tile_set() {
    const auto& tiles = Domino::all();
    std::size_t index = 0;
    for (int high = 0; high <= 6; ++high) {
        for (int low = 0; low <= high; ++low) {
            const Domino tile = tiles.at(index);
            check(tile == Domino(low, high), "tile set must use Scala order");
            check(tile.high() == high && tile.low() == low, "tile pips must be canonical");
            check(tile.id() == index, "tile ID must match the Scala mask index");
            check(tile.is_double() == (high == low), "only matching pips form a double");
            ++index;
        }
    }
    check(index == tiles.size(), "double-six must have 28 tiles");
}

void check_orientation() {
    const PlayedDomino single(Domino(6, 3), 3);
    const PlayedDomino double_four(Domino(4, 4), 4);
    check(single.score() == 3, "a non-double scores its exposed pip");
    check(double_four.score() == 8, "a double scores both pips");
    check(single.connect(Domino(5, 3)) == PlayedDomino(Domino(5, 3), 5),
          "placement can match the lower pip");
    check(single.connect(Domino(3, 1)) == PlayedDomino(Domino(3, 1), 1),
          "placement can match the higher pip");
    check(!single.connect(Domino(6, 2)), "placement must match the exposed pip");
    check(double_four.connect(Domino(6, 4)) == PlayedDomino(Domino(6, 4), 6),
          "a double connects using one pip");
    check(!double_four.connect(Domino(6, 2)), "the double's score is separate from its pip");
    const auto reversed = single.reversed();
    check(reversed.tile() == single.tile(), "reversing preserves tile identity");
    check(reversed.exposed_pip() == 6 && single.exposed_pip() == 3,
          "reversing exposes the other pip and preserves the original");
    check(reversed.reversed() == single, "reversing twice restores orientation");
    check(double_four.reversed() == double_four, "a double preserves its orientation");
}

}  // namespace

int main() {
    try {
        check_tile_set();
        check_orientation();
    } catch (const std::exception& error) {
        std::cerr << error.what() << '\n';
        return 1;
    }
}
