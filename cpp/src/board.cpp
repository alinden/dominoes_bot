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

std::optional<Board::Placement> Board::resolve_placement(Domino tile, Direction direction) const {
    if (contains(tile)) {
        return std::nullopt;
    }

    const auto current_shape = shape();
    if (current_shape == BoardShape::empty) {
        return Placement{current_shape, Direction::left, PlayedDomino(tile, tile.high())};
    }
    if ((current_shape == BoardShape::snake || current_shape == BoardShape::tee) &&
        direction != Direction::left && direction != Direction::right) {
        return std::nullopt;
    }
    if (current_shape == BoardShape::spinner_only) {
        direction = Direction::left;
    }

    const auto& arm = chain(direction);
    const auto end = current_shape == BoardShape::snake && direction == Direction::right
                         ? chain(Direction::left).back().reversed()
                         : arm.empty() ? *spinner_ : arm.front();
    if (auto played = end.connect(tile)) {
        return Placement{current_shape, direction, *played};
    }
    return std::nullopt;
}

bool Board::can_place(Domino tile, Direction direction) const {
    return resolve_placement(tile, direction).has_value();
}

std::optional<Board> Board::place(Domino tile, Direction direction) const {
    const auto placement = resolve_placement(tile, direction);
    if (!placement) {
        return std::nullopt;
    }

    Board next = *this;
    const auto [shape, target, played] = *placement;
    auto& arm = next.chains_.at(static_cast<std::size_t>(target));
    if (shape == BoardShape::empty && tile.is_double()) {
        next.spinner_ = played;
    } else if (shape == BoardShape::snake && tile.is_double()) {
        next.spinner_ = played;
        if (target == Direction::left) {
            // The old left end is now next to the spinner, so the far end faces out.
            std::ranges::reverse(arm);
            for (auto& placed : arm) {
                placed = placed.reversed();
            }
        }
    } else if (shape == BoardShape::snake && target == Direction::right) {
        next.chains_.front().push_back(played.reversed());
    } else {
        arm.insert(arm.begin(), played);
    }
    return next;
}

}  // namespace dominoes
