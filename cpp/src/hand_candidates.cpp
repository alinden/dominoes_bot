#include "dominoes/hand_candidates.hpp"

#include <algorithm>
#include <bit>
#include <iomanip>
#include <ostream>
#include <sstream>
#include <stdexcept>

namespace dominoes {
namespace {

using Mask = std::uint32_t;
constexpr Mask all_tiles = (Mask{1} << Domino::tile_count) - 1;

Mask bit(Domino tile) {
    return Mask{1} << tile.id();
}

Mask mask(std::span<const Domino> tiles) {
    Mask result = 0;
    for (auto tile : tiles) {
        result |= bit(tile);
    }
    return result;
}

void enumerate(Mask available, std::size_t count, Mask selected, std::vector<Mask>& hands) {
    if (count == 0) {
        hands.push_back(selected);
        return;
    }
    while (static_cast<std::size_t>(std::popcount(available)) >= count) {
        const Mask first = Mask{1} << std::countr_zero(available);
        available ^= first;
        enumerate(available, count - 1, selected | first, hands);
    }
}

Mask unknown_tiles(const Game& position, Seat observer) {
    Mask unknown = all_tiles & ~mask(position.player(observer).hand);
    for (auto tile : Domino::all()) {
        if (position.board().contains(tile)) {
            unknown &= ~bit(tile);
        }
    }
    return unknown;
}

std::vector<Domino> tiles_from(Mask tiles) {
    std::vector<Domino> result;
    result.reserve(std::popcount(tiles));
    while (tiles != 0) {
        result.push_back(Domino::all()[std::countr_zero(tiles)]);
        tiles &= tiles - 1;
    }
    return result;
}

}  // namespace

HandCandidates::HandCandidates(Seat observer, std::span<const Domino> own_hand,
                               std::size_t opponent_size, bool bayesian)
    : observer_(observer), bayesian_(bayesian) {
    std::vector<Mask> possible;
    enumerate(all_tiles & ~mask(own_hand), opponent_size, 0, possible);
    hands_.reserve(possible.size());
    for (auto hand : possible) hands_.push_back({hand});
}

void HandCandidates::observe(const Game& position, const TurnResult& result) {
    if (!result.actor || result.round) {
        return;
    }
    const bool own_action = result.actor == observer_;
    if (result.placement) {
        const auto tile = result.placement->tile;
        if (std::holds_alternative<PlayHighestDouble>(result.action)) {
            Mask higher_doubles = 0;
            for (int pip = tile.high() + 1; pip <= Domino::max_pip; ++pip) {
                higher_doubles |= bit(Domino(pip, pip));
            }
            std::erase_if(hands_, [higher_doubles](const Candidate& candidate) {
                return candidate.hand & higher_doubles;
            });
        }
        if (!own_action) {
            std::erase_if(hands_, [tile](const Candidate& candidate) {
                return !(candidate.hand & bit(tile));
            });
            for (auto& candidate : hands_) {
                candidate.hand &= ~bit(tile);
            }
        }
        return;
    }

    if (own_action) {
        if (std::holds_alternative<Draw>(result.action)) {
            const Mask drawn = bit(position.player(observer_).hand.front());
            std::erase_if(hands_, [drawn](const Candidate& candidate) { return candidate.hand & drawn; });
        }
        return;
    }

    const bool is_draw = std::holds_alternative<Draw>(result.action);
    const Mask playable = is_draw ? mask(result.pre_draw_playable_tiles)
                                  : mask(position.playable_tiles());
    std::erase_if(hands_, [playable](const Candidate& candidate) { return candidate.hand & playable; });
    if (is_draw) {
        const Mask unknown = unknown_tiles(position, observer_);
        std::vector<Candidate> expanded;
        for (const auto& candidate : hands_) {
            for (Mask choices = unknown & ~candidate.hand; choices != 0; choices &= choices - 1) {
                expanded.push_back({candidate.hand | (Mask{1} << std::countr_zero(choices)),
                                    candidate.weight});
            }
        }
        std::ranges::sort(expanded, {}, &Candidate::hand);
        std::size_t unique = 0;
        for (const auto candidate : expanded) {
            if (unique == 0 || expanded[unique - 1].hand != candidate.hand) {
                expanded[unique++] = candidate;
            } else if (bayesian_) {
                // Each compatible pre-draw hand has the same number of possible drawn tiles.
                expanded[unique - 1].weight += candidate.weight;
            }
        }
        expanded.resize(unique);
        hands_ = std::move(expanded);
    }
}

std::discrete_distribution<std::size_t> HandCandidates::weighted_picker() const {
    std::vector<double> weights;
    weights.reserve(hands_.size());
    for (const auto& candidate : hands_) weights.push_back(candidate.weight);
    return {weights.begin(), weights.end()};
}

void HandCandidates::log_estimates(std::ostream& log, std::span<const Domino> actual_hand) const {
    log << "Opponent hand estimates:\n";
    if (hands_.empty()) {
        log << "No possible hands\n\n";
        return;
    }

    const auto higher_rank = [](const Candidate& first, const Candidate& second) {
        if (first.weight != second.weight) return first.weight > second.weight;
        return first.hand < second.hand;
    };
    const auto actual = std::ranges::find(hands_, mask(actual_hand), &Candidate::hand);
    double total_weight = 0.0;
    std::size_t actual_rank = actual == hands_.end() ? 0 : 1;
    std::array<double, Domino::tile_count> tile_weights{};
    for (const auto& candidate : hands_) {
        total_weight += candidate.weight;
        if (actual != hands_.end() && higher_rank(candidate, *actual)) ++actual_rank;
        for (Mask tiles = candidate.hand; tiles != 0; tiles &= tiles - 1) {
            tile_weights[std::countr_zero(tiles)] += candidate.weight;
        }
    }

    std::vector<Domino> ranked_tiles;
    for (auto tile : Domino::all()) {
        if (tile_weights[tile.id()] > 0.0) ranked_tiles.push_back(tile);
    }
    std::ranges::stable_sort(ranked_tiles, std::ranges::greater{},
                             [&](Domino tile) { return tile_weights[tile.id()]; });
    std::array<Candidate, 5> top_hands{};
    const auto top_end = std::partial_sort_copy(hands_.begin(), hands_.end(), top_hands.begin(),
                                                top_hands.end(), higher_rank);

    std::ostringstream summary;
    const auto print_tile = [&](Domino tile) { summary << '[' << tile.high() << ',' << tile.low() << ']'; };
    const auto print_probability = [&](double probability) { summary << 100.0 * probability << '%'; };
    const auto print_hand = [&](std::span<const Domino> tiles) {
        bool first = true;
        for (auto tile : tiles) {
            if (!first) summary << ' ';
            print_tile(tile);
            first = false;
        }
    };
    const auto print_tile_probability = [&](Domino tile) {
        summary << "  ";
        print_tile(tile);
        summary << "  ";
        print_probability(tile_weights[tile.id()] / total_weight);
        summary << '\n';
    };

    constexpr std::size_t tile_limit = 5;
    const auto visible = std::min(tile_limit, ranked_tiles.size());
    const double uniform = 1.0 / hands_.size();
    summary << std::fixed << std::setprecision(3) << hands_.size()
            << " possible hands\nUniform hand probability: ";
    print_probability(uniform);
    summary << "\n\nMost likely tiles:\n";
    for (std::size_t i = 0; i < visible; ++i) print_tile_probability(ranked_tiles[i]);
    summary << "\nLeast likely tiles:\n";
    for (std::size_t i = 0; i < visible; ++i) print_tile_probability(ranked_tiles[ranked_tiles.size() - 1 - i]);
    summary << "\nMost likely hands:\n";
    for (auto candidate = top_hands.begin(); candidate != top_end; ++candidate) {
        summary << "  ";
        print_probability(candidate->weight / total_weight);
        summary << "  ";
        print_hand(tiles_from(candidate->hand));
        summary << '\n';
    }

    const double probability = actual == hands_.end() ? 0.0 : actual->weight / total_weight;
    summary << "\nActual hand:\n  ";
    print_hand(actual_hand);
    summary << "\n  Probability: ";
    print_probability(probability);
    summary << "\n  Relative to uniform: " << std::setprecision(2) << probability / uniform
            << "x\n  Rank: " << actual_rank << " / " << hands_.size() << "\n\n";
    log << summary.str();
}

std::vector<std::vector<Domino>> HandCandidates::feasible_hands() const {
    std::vector<std::vector<Domino>> result;
    result.reserve(hands_.size());
    for (const auto& candidate : hands_) result.push_back(tiles_from(candidate.hand));
    return result;
}

std::vector<std::vector<Domino>> HandCandidates::sample_distinct_hands(
    std::span<const Domino> included, std::size_t limit, std::mt19937& random) const {
    if (limit == 0) return {};
    const auto actual = std::ranges::find(hands_, mask(included), &Candidate::hand);
    if (actual == hands_.end()) throw std::logic_error("Hard constraints excluded the dealt hand");
    std::vector<std::size_t> indices{static_cast<std::size_t>(actual - hands_.begin())};
    const auto wanted = std::min(limit, hands_.size());
    std::uniform_int_distribution<std::size_t> pick(0, hands_.size() - 1);
    if (wanted == hands_.size()) {
        for (std::size_t i = 0; i < hands_.size(); ++i) {
            if (i != indices.front()) indices.push_back(i);
        }
    } else {
        while (indices.size() < wanted) {
            const auto next = pick(random);
            if (std::ranges::find(indices, next) == indices.end()) indices.push_back(next);
        }
    }
    std::vector<std::vector<Domino>> selected;
    for (auto index : indices) selected.push_back(tiles_from(hands_[index].hand));
    return selected;
}

std::vector<SearchSample> HandCandidates::sample(const Game& position, std::size_t count,
                                                 std::mt19937& random) const {
    if (hands_.empty()) {
        return {};
    }
    const Mask unknown = unknown_tiles(position, observer_);
    std::uniform_int_distribution<std::size_t> pick(0, hands_.size() - 1);
    auto weighted = bayesian_ ? weighted_picker() : std::discrete_distribution<std::size_t>{};
    std::vector<SearchSample> result;
    result.reserve(count);
    while (result.size() < count) {
        const Mask hand = hands_[bayesian_ ? weighted(random) : pick(random)].hand;
        auto draw_order = tiles_from(unknown & ~hand);
        std::shuffle(draw_order.begin(), draw_order.end(), random);
        result.push_back({tiles_from(hand), std::move(draw_order), 1.0});
    }
    return result;
}

InferenceReport HandCandidates::observe_with_inference(
    const Game& before, const Game& after, const TurnResult& result, std::mt19937& random,
    std::chrono::steady_clock::time_point deadline, std::size_t workers) {
    observe(after, result);
    if (bayesian_ && !result.round && result.actor && result.actor != observer_) {
        if (const auto* move = std::get_if<Move>(&result.action)) {
            return infer(before, *move, random, deadline, workers);
        }
    }
    return {};
}

InferenceReport HandCandidates::infer(const Game& before, Move observed, std::mt19937& random,
                                    std::chrono::steady_clock::time_point deadline, std::size_t workers) {
    const Seat opponent = observer_ == Seat::first ? Seat::second : Seat::first;
    InferenceReport report;
    report.candidates = hands_.size();
    if (hands_.size() < 2 || before.active_seat() != opponent) return report;

    if (hands_.size() > 300) {
        report.too_many_hands = true;
        return report;
    }
    constexpr std::size_t breadth = 10;
    const auto original_unknown = unknown_tiles(before, observer_);
    const auto own_size = before.player(observer_).hand.size();
    const auto canonical = before.canonical_move(observed);
    std::vector<std::size_t> bad_hands;

    for (std::size_t index = 0; index < hands_.size(); ++index) {
        if (std::chrono::steady_clock::now() >= deadline) {
            report.timed_out = true;
            return report;
        }
        const auto remaining = hands_[index].hand;
        const auto full_hand = remaining | bit(observed.tile);
        auto hypothetical = before.with_hidden_tiles(observer_, tiles_from(full_hand),
                                                      tiles_from(original_unknown & ~full_hand)).value();
        if (hypothetical.legal_actions().size() == 1) {
            ++report.forced;
            continue;
        }

        // Model the opponent's viewpoint. Only our hand size is known to this search.
        auto unknown = tiles_from(unknown_tiles(hypothetical, opponent));
        std::vector<SearchSample> worlds;
        worlds.reserve(breadth);
        for (std::size_t i = 0; i < breadth; ++i) {
            std::shuffle(unknown.begin(), unknown.end(), random);
            const auto middle = unknown.begin() + own_size;
            worlds.push_back({{unknown.begin(), middle}, {middle, unknown.end()}, 1.0});
        }
        const auto search = search_to_round_end(hypothetical, opponent, worlds, deadline, workers);
        if (!search.reached_round_end) {
            report.timed_out = search.timed_out;
            report.incomplete = !search.timed_out;
            return report;
        }
        const auto observed_score = std::ranges::find_if(search.scores, [&](const ScoredAction& scored) {
            const auto* move = std::get_if<Move>(&scored.action);
            return move && *move == canonical;
        });
        if (observed_score == search.scores.end()) {
            throw std::logic_error("observed move is missing from the hypothetical legal actions");
        }
        const double best = std::ranges::max_element(search.scores, {}, &ScoredAction::score)->score;
        constexpr double near_best = 0.025;
        const bool plausible = best - observed_score->score <= near_best;
        if (plausible) {
            ++report.good;
        } else {
            ++report.bad;
            bad_hands.push_back(index);
        }
    }

    if (std::chrono::steady_clock::now() >= deadline) {
        report.timed_out = true;
        return report;
    }
    if (bad_hands.empty() || bad_hands.size() == hands_.size()) return report;
    for (auto index : bad_hands) hands_[index].weight *= 0.2;
    report.updated = true;
    return report;
}

void log_inference(std::ostream& log, const InferenceReport& report) {
    if (report.candidates < 2) return;
    if (report.too_many_hands) {
        log << "Bayes skipped: " << report.candidates << " feasible hands. Kept hard evidence.\n" << std::flush;
        return;
    }
    log << "Bayes hand update: " << report.candidates
        << " candidates, " << report.good << " good, " << report.bad << " bad, "
        << report.forced << " forced. ";
    if (report.timed_out) log << "Deadline reached. Kept hard evidence.";
    else if (report.incomplete) log << "Search did not settle all sampled branches. Kept hard evidence.";
    else if (report.updated) log << "Updated weights.";
    else log << "Weights unchanged.";
    log << '\n' << std::flush;
}

DecisionSamples HandCandidates::sample_for_search(const Game& position, std::size_t breadth,
                                                   std::mt19937& random) const {
    constexpr std::size_t exhaustive_hand_limit = 32;
    if (hands_.empty() || hands_.size() > exhaustive_hand_limit) {
        return {sample(position, breadth, random)};
    }

    constexpr std::array<std::size_t, 4> permutations{1, 1, 2, 6};
    const auto boneyard_size = position.boneyard().size();
    const bool all_draw_orders = boneyard_size < permutations.size();
    const auto total_scenarios = all_draw_orders ? hands_.size() * permutations[boneyard_size]
                                                : std::max(breadth, hands_.size());
    DecisionSamples result{{}, true, all_draw_orders};
    result.worlds.reserve(total_scenarios);
    const Mask unknown = unknown_tiles(position, observer_);
    const auto draws_per_hand = total_scenarios / hands_.size();
    const auto extra_draws = total_scenarios % hands_.size();
    for (std::size_t index = 0; index < hands_.size(); ++index) {
        const auto& candidate = hands_[index];
        const auto hand = tiles_from(candidate.hand);
        auto draw_order = tiles_from(unknown & ~candidate.hand);
        const auto draw_count = draws_per_hand + (index < extra_draws);
        const double weight = candidate.weight / draw_count;
        for (std::size_t i = 0; i < draw_count; ++i) {
            if (!all_draw_orders) std::shuffle(draw_order.begin(), draw_order.end(), random);
            result.worlds.push_back({hand, draw_order, weight});
            if (all_draw_orders) std::ranges::next_permutation(draw_order, {}, &Domino::id);
        }
    }
    return result;
}

}  // namespace dominoes
