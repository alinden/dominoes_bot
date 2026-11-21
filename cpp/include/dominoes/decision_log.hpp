#pragma once

#include "dominoes/hand_candidates.hpp"

#include <iosfwd>

namespace dominoes {

void log_action(std::ostream& log, const Action& action);
void log_search_scores(std::ostream& log, int depth, const DecisionSamples& samples,
                       std::vector<ScoredAction>& scores);

}  // namespace dominoes
