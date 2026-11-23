#pragma once

#include "dominoes/game.hpp"

#include <random>

namespace dominoes {

std::optional<Action> choose_greedy_action(const Game& game, std::mt19937& random);

}  // namespace dominoes
