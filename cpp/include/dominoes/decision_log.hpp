#pragma once

#include "dominoes/search.hpp"

#include <iosfwd>

namespace dominoes {

void log_action(std::ostream& log, const Action& action);
void log_search_scores(std::ostream& log, int depth, std::size_t samples,
                       std::vector<ScoredAction>& scores);

}  // namespace dominoes
