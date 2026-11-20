#pragma once

#include "dominoes/domino.hpp"

#include <array>
#include <optional>
#include <vector>

namespace dominoes {

enum class Direction { left, right, up, down };
enum class BoardShape { empty, snake, spinner_only, tee, cross };

class Board {
public:
    BoardShape shape() const;
    int score() const;
    std::size_t size() const;
    bool contains(Domino tile) const;

    const std::optional<PlayedDomino>& spinner() const { return spinner_; }
    // Chains run from the exposed end toward the spinner. A snake is one left chain.
    const std::vector<PlayedDomino>& chain(Direction direction) const;

    [[nodiscard]] std::optional<Board> place(Domino tile, Direction direction) const;

private:
    std::optional<Board> extend_snake(Domino tile, Direction direction) const;
    std::optional<Board> extend_arm(Domino tile, Direction direction) const;

    std::optional<PlayedDomino> spinner_;
    std::array<std::vector<PlayedDomino>, 4> chains_;
};

}  // namespace dominoes
