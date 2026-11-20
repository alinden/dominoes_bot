#include "dominoes/greedy.hpp"

#include <tuple>

namespace dominoes {

std::optional<Action> choose_greedy_action(const Game& game, std::mt19937& random) {
    const auto actions = game.legal_actions();
    if (actions.empty()) {
        return std::nullopt;
    }

    const Move* best = nullptr;
    std::tuple<int, Direction, std::size_t> best_rank;
    for (const auto& action : actions) {
        if (const auto* move = std::get_if<Move>(&action)) {
            const int score = game.board().place(move->tile, move->direction)->score();
            // Scala reverses the stable score sort over direction and tile enumeration.
            const auto rank = std::tuple{score, move->direction, move->tile.id()};
            if (score % 5 == 0 && (!best || rank > best_rank)) {
                best = move;
                best_rank = rank;
            }
        }
    }
    if (best) {
        return *best;
    }
    std::uniform_int_distribution<std::size_t> index(0, actions.size() - 1);
    return actions[index(random)];
}

}  // namespace dominoes
