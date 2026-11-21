#include "dominoes/decision_log.hpp"

#include <algorithm>
#include <iomanip>
#include <ostream>
#include <string_view>

namespace dominoes {

void log_action(std::ostream& log, const Action& action) {
    constexpr std::array<std::string_view, 4> directions{"left", "right", "up", "down"};
    if (const auto* move = std::get_if<Move>(&action)) {
        log << "Move [" << move->tile.high() << ',' << move->tile.low() << "] "
            << directions.at(static_cast<std::size_t>(move->direction));
    } else {
        log << (std::holds_alternative<Draw>(action) ? "Draw" : "Pass");
    }
}

void log_search_scores(std::ostream& log, int depth, const DecisionSamples& samples,
                       std::vector<ScoredAction>& scores) {
    log << "Depth: " << depth << ", Samples: " << samples.worlds.size() << '\n';
    if (samples.all_hands) {
        log << "Hand coverage: all feasible hands";
        if (samples.all_draw_orders) log << " and draw orders";
        log << ".\n";
    }
    std::ranges::stable_sort(scores, std::ranges::greater{}, &ScoredAction::score);
    for (const auto& scored : scores) {
        log_action(log, scored.action);
        log << "  mean=" << std::fixed << std::setprecision(6) << scored.score << '\n';
    }
}

}  // namespace dominoes
