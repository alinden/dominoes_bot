#include "dominoes/board.hpp"

#include <iostream>
#include <stdexcept>

using namespace dominoes;

namespace {

void check(bool condition, const char* message) {
    if (!condition) {
        throw std::runtime_error(message);
    }
}

void check_spinner_arms() {
    const Board empty;
    check(empty.shape() == BoardShape::empty && empty.score() == 0 && empty.size() == 0,
          "the empty board has no tiles or points");

    const Board spinner = empty.place({5, 5}, Direction::up).value();
    check(spinner.shape() == BoardShape::spinner_only && spinner.score() == 10,
          "an opening double becomes the spinner and scores both pips");
    for (auto direction : {Direction::left, Direction::right, Direction::up, Direction::down}) {
        const Board tee = spinner.place({5, 2}, direction).value();
        check(tee.shape() == BoardShape::tee && tee.chain(Direction::left).size() == 1,
              "every first arm placement goes to the left chain");
        check(!spinner.place({6, 3}, direction), "a tile must match the spinner");
    }

    const Board tee = spinner.place({5, 2}, Direction::left).value();
    check(tee.score() == 12, "a tee scores the spinner and its left end");
    check(!tee.place({5, 4}, Direction::up) && !tee.place({5, 4}, Direction::down),
          "vertical arms require a cross");
    const Board cross = tee.place({5, 3}, Direction::right).value();
    check(cross.shape() == BoardShape::cross && cross.score() == 5 && cross.size() == 3,
          "with two arms the spinner stops contributing to the score");
    const Board three_arms = cross.place({5, 4}, Direction::up).value();
    check(three_arms.score() == 9, "the upper exposed pip contributes to the total");
    const Board four_arms = three_arms.place({5, 1}, Direction::down).value();
    check(four_arms.score() == 10, "all four exposed ends contribute to the total");
    check(!four_arms.place({5, 0}, Direction::down), "a filled arm connects at its outer end");
    const Board end_double = four_arms.place({1, 1}, Direction::down).value();
    check(end_double.score() == 11 && end_double.spinner() == spinner.spinner(),
          "an exposed double counts twice and keeps the original spinner");
    check(!cross.place({5, 3}, Direction::up), "a tile already on the board cannot be reused");
    check(spinner.contains({5, 5}) && !spinner.contains({5, 2}) && tee.contains({5, 2}),
          "placement preserves the original board");
}

void check_snake() {
    const Board first = Board{}.place({6, 4}, Direction::down).value();
    check(first.shape() == BoardShape::snake && first.score() == 10,
          "the first non-double exposes both ends");
    check(!first.place({4, 3}, Direction::up) && !first.place({4, 3}, Direction::down),
          "a snake only has left and right ends");
    check(!first.place({5, 2}, Direction::right), "the right end rejects mismatched tiles");
    const Board second = first.place({6, 2}, Direction::left).value();
    check(second.score() == 6, "the left extension updates the exposed left pip");
    const Board third = second.place({4, 3}, Direction::right).value();
    check(third.score() == 5 && third.chain(Direction::right).empty(),
          "the right extension updates the far end of the same chain");
    check(third.chain(Direction::left) == std::vector<PlayedDomino>{
              {{6, 2}, 2}, {{6, 4}, 6}, {{4, 3}, 4}},
          "the snake stores its tiles oriented toward the left end");
}

void check_attached_spinner() {
    const Board first = Board{}.place({6, 4}, Direction::left).value();
    const Board snake = first.place({4, 3}, Direction::right).value();
    const Board left_spinner = snake.place({6, 6}, Direction::left).value();
    check(left_spinner.chain(Direction::left) == std::vector<PlayedDomino>{
              {{4, 3}, 3}, {{6, 4}, 4}},
          "attaching a spinner on the left reverses both order and orientation");
    check(left_spinner.shape() == BoardShape::tee && left_spinner.score() == 15,
          "the tee scores the new spinner and the old right end");

    const Board right_spinner = snake.place({3, 3}, Direction::right).value();
    check(right_spinner.chain(Direction::left) == snake.chain(Direction::left),
          "attaching a spinner on the right preserves the chain");
    check(right_spinner.shape() == BoardShape::tee && right_spinner.score() == 12,
          "the tee scores the new spinner and the old left end");
    check(!snake.place({5, 5}, Direction::left) && !snake.place({5, 5}, Direction::right),
          "a spinner must match the end it attaches to");
    check(snake.shape() == BoardShape::snake && snake.score() == 9,
          "spinner placement preserves the original snake");
}

}  // namespace

int main() {
    try {
        check_spinner_arms();
        check_snake();
        check_attached_spinner();
    } catch (const std::exception& error) {
        std::cerr << error.what() << '\n';
        return 1;
    }
}
