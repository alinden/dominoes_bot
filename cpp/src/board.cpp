#include "dominoes/board.hpp"

#include <algorithm>

namespace dominoes {

const std::vector<PlayedDomino>& Board::chain(Direction direction) const {
    return chains_.at(static_cast<std::size_t>(direction));
}

BoardShape Board::shape() const {
    if (!spinner_) {
        return chain(Direction::left).empty() ? BoardShape::empty : BoardShape::snake;
    }
    if (chain(Direction::left).empty()) {
        return BoardShape::spinner_only;
    }
    return chain(Direction::right).empty() ? BoardShape::tee : BoardShape::cross;
}

int Board::score() const {
    const auto& left = chain(Direction::left);
    switch (shape()) {
    case BoardShape::empty:
        return 0;
    case BoardShape::snake:
        return left.front().score() + left.back().reversed().score();
    case BoardShape::spinner_only:
        return spinner_->score();
    case BoardShape::tee:
        return spinner_->score() + left.front().score();
    case BoardShape::cross:
        int total = 0;
        for (const auto& arm : chains_) {
            if (!arm.empty()) {
                total += arm.front().score();
            }
        }
        return total;
    }
    return 0;
}

std::size_t Board::size() const {
    std::size_t count = spinner_.has_value();
    for (const auto& arm : chains_) {
        count += arm.size();
    }
    return count;
}

bool Board::contains(Domino tile) const {
    if (spinner_ && spinner_->tile() == tile) {
        return true;
    }
    for (const auto& arm : chains_) {
        if (std::ranges::any_of(arm, [tile](const PlayedDomino& played) {
                return played.tile() == tile;
            })) {
            return true;
        }
    }
    return false;
}

std::optional<Board> Board::place(Domino tile, Direction direction) const {
    if (contains(tile)) {
        return std::nullopt;
    }

    switch (shape()) {
    case BoardShape::empty: {
        Board next;
        if (tile.is_double()) {
            next.spinner_.emplace(tile, tile.high());
        } else {
            next.chains_.front().emplace_back(tile, tile.high());
        }
        return next;
    }
    case BoardShape::snake:
        return extend_snake(tile, direction);
    case BoardShape::spinner_only:
        return extend_arm(tile, Direction::left);
    case BoardShape::tee:
        if (direction != Direction::left && direction != Direction::right) {
            return std::nullopt;
        }
        return extend_arm(tile, direction);
    case BoardShape::cross:
        return extend_arm(tile, direction);
    }
    return std::nullopt;
}

std::optional<Board> Board::extend_snake(Domino tile, Direction direction) const {
    if (direction != Direction::left && direction != Direction::right) {
        return std::nullopt;
    }
    const auto& snake = chain(Direction::left);
    const bool at_left = direction == Direction::left;
    const auto end = at_left ? snake.front() : snake.back().reversed();
    const auto played = end.connect(tile);
    if (!played) {
        return std::nullopt;
    }

    Board next = *this;
    auto& left = next.chains_.front();
    if (tile.is_double()) {
        next.spinner_ = played;
        if (at_left) {
            // The old left end is now next to the spinner, so the far end faces out.
            std::ranges::reverse(left);
            for (auto& placed : left) {
                placed = placed.reversed();
            }
        }
    } else if (at_left) {
        left.insert(left.begin(), *played);
    } else {
        left.push_back(played->reversed());
    }
    return next;
}

std::optional<Board> Board::extend_arm(Domino tile, Direction direction) const {
    const auto& arm = chain(direction);
    const auto& end = arm.empty() ? *spinner_ : arm.front();
    const auto played = end.connect(tile);
    if (!played) {
        return std::nullopt;
    }

    Board next = *this;
    auto& next_arm = next.chains_.at(static_cast<std::size_t>(direction));
    next_arm.insert(next_arm.begin(), *played);
    return next;
}

}  // namespace dominoes
