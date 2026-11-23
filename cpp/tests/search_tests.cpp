#include "dominoes/search.hpp"
#include "dominoes/hand_candidates.hpp"

#include <algorithm>
#include <cmath>
#include <fstream>
#include <iostream>
#include <map>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>

using namespace dominoes;

namespace {

void expect_score(double actual, double expected, const char* message) {
    if (!(std::abs(actual - expected) < 1e-12)) {
        throw std::runtime_error(message);
    }
}

void exchange_tiles(SearchSample& sample, Domino held, Domino in_boneyard) {
    const auto hand = std::ranges::find(sample.opponent_hand, held);
    const auto draw = std::ranges::find(sample.draw_order, in_boneyard);
    if (hand == sample.opponent_hand.end() || draw == sample.draw_order.end()) {
        throw std::runtime_error("invalid exchange in search fixture");
    }
    std::iter_swap(hand, draw);
}

void draw_at(SearchSample& sample, Domino tile, std::size_t index) {
    const auto found = std::ranges::find(sample.draw_order, tile);
    if (found == sample.draw_order.end()) {
        throw std::runtime_error("scripted draw is missing from the boneyard");
    }
    std::iter_swap(sample.draw_order.begin() + index, found);
}

void check_sampled_hands(const Game& game) {
    const SearchSample first{game.player(Seat::first).hand, game.boneyard(), 1.0};
    auto second = first;
    second.weight = 3.0;
    exchange_tiles(second, {6, 2}, {2, 2});

    const std::array samples{first, second};
    const auto scores = score_sampled_actions(game, Seat::second, 2, samples);
    const auto parallel = score_sampled_actions(game, Seat::second, 2, samples, 4);
    if (scores.size() != 2 || std::get<Move>(scores.front().action) != Move{{6, 3}, Direction::left}) {
        throw std::runtime_error("sampled search did not preserve the root moves");
    }
    expect_score(scores.front().score, 0.25 * std::tanh(10.0 / 135) + 0.75 * std::tanh(15.0 / 135),
                 "a counterplay available in one hand must receive only that hand's weight");
    expect_score(scores.back().score, 0.0, "the second root move scores zero against both hands");
    if (parallel.size() != scores.size()) {
        throw std::runtime_error("parallel search must return all root moves");
    }
    for (std::size_t i = 0; i < scores.size(); ++i) {
        if (std::get<Move>(parallel[i].action) != std::get<Move>(scores[i].action) ||
            parallel[i].score != scores[i].score) {
            throw std::runtime_error("parallel search must retain move order and exact scores");
        }
    }

    const auto expired = search_until(game, Seat::second, 68, samples, std::chrono::steady_clock::now());
    if (!expired.timed_out || expired.depth != 0 || !expired.scores.empty()) {
        throw std::runtime_error("an expired deadline must discard the unfinished search");
    }
    const auto deadline = std::chrono::steady_clock::now() + std::chrono::seconds(5);
    const auto timed = search_until(game, Seat::second, 2, samples, deadline, 4);
    if (timed.timed_out || timed.depth != 2 || timed.reached_round_end ||
        timed.scores.size() != scores.size()) {
        throw std::runtime_error("timed search should complete the requested shallow pass");
    }
    expect_score(timed.scores.front().score, scores.front().score,
                 "timed search must reuse the supplied samples");

    int ordered_nodes = 0;
    const std::array phases{OrderingPhase{[&](const Game& child, Seat mover) {
        ++ordered_nodes;
        return evaluate_position(child, mover);
    }, 0, 12, 8, 14}};
    const SettlementSearchOptions ordered{{}, true, false, 0, phases};
    const auto plain = search_until(game, Seat::second, 6, samples, deadline);
    const auto ranked = search_until(game, Seat::second, 6, samples, deadline, 1, ordered);
    if (ordered_nodes == 0 || plain.timed_out || ranked.timed_out ||
        plain.depth != ranked.depth || plain.reached_round_end != ranked.reached_round_end ||
        plain.scores.size() != ranked.scores.size()) {
        throw std::runtime_error("bounded move ordering must preserve completion and depth");
    }
    for (std::size_t i = 0; i < plain.scores.size(); ++i) {
        expect_score(ranked.scores[i].score, plain.scores[i].score,
                     "bounded move ordering must preserve weighted move values");
    }

    auto invalid = second;
    invalid.opponent_hand.front() = {6, 6};
    try {
        score_sampled_actions(game, Seat::second, 2, std::span{&invalid, 1});
    } catch (const std::invalid_argument&) {
        return;
    }
    throw std::runtime_error("a sample cannot reuse a visible tile");
}

void check_sampled_draws(const Game& game) {
    SearchSample blocked{game.player(Seat::first).hand, game.boneyard(), 1.0};
    exchange_tiles(blocked, {6, 2}, {2, 2});
    exchange_tiles(blocked, {4, 3}, {5, 1});
    auto drawing = game.with_hidden_tiles(Seat::second, blocked.opponent_hand, blocked.draw_order).value();
    drawing.apply(Move{{6, 3}, Direction::left}).value();

    SearchSample first{drawing.player(Seat::second).hand, drawing.boneyard(), 1.0};
    auto second = first;
    auto third = first;
    second.weight = 2.0;
    draw_at(first, {2, 1}, 0); // Miss, then draw a tile that scores five.
    draw_at(first, {6, 2}, 1);
    draw_at(second, {2, 1}, 0); // Same miss, then a playable tile that scores zero.
    draw_at(second, {6, 5}, 1);
    draw_at(third, {6, 5}, 0); // The same zero-point tile on the first draw.
    const std::array samples{first, second, third};
    const auto scores = score_sampled_actions(drawing, Seat::first, 2, samples);
    if (scores.size() != 1 || !std::holds_alternative<Draw>(scores.front().action)) {
        throw std::runtime_error("a blocked hand must draw");
    }
    expect_score(scores.front().score, -0.25 * std::tanh(10.0 / 135) - 0.75 * std::tanh(15.0 / 135),
                 "draw branches must average their weighted replies and repeated draws share a ply");
    expect_score(score_sampled_actions(drawing, Seat::first, 1, samples).front().score,
                 -std::tanh(15.0 / 135), "a draw at the cutoff does not score a follow-up placement");
}

void check_public_draw_evidence() {
    auto tiles = Domino::all();
    std::swap(tiles[7], tiles[20]); // Seat two opens with [5,5].
    Game game;
    if (!game.deal(tiles)) throw std::runtime_error("invalid deal for draw evidence");
    HandCandidates candidates(Seat::second, game.player(Seat::second).hand, 7);
    candidates.observe(game, game.apply(PlayHighestDouble{}).value());

    std::mt19937 random(120);
    const auto opening_samples = candidates.sample(game, 128, random);
    if (opening_samples.empty() || std::ranges::any_of(opening_samples, [](const auto& sample) {
            return std::ranges::find(sample.opponent_hand, Domino(6, 6)) != sample.opponent_hand.end();
        })) {
        throw std::runtime_error("a higher double must still be in the boneyard after opening");
    }
    for (int draw = 1; draw <= 2; ++draw) {
        const auto event = game.apply(Draw{}).value();
        candidates.observe(game, event);
        const auto samples = candidates.sample(game, 128, random);
        bool saw_playable_draw = false;
        for (const auto& sample : samples) {
            const auto playable = std::ranges::count_if(sample.opponent_hand, [&](Domino tile) {
                return std::ranges::find(event.pre_draw_playable_tiles, tile) !=
                       event.pre_draw_playable_tiles.end();
            });
            if (playable > 1 || sample.opponent_hand.size() != 7U + draw ||
                !game.with_hidden_tiles(Seat::second, sample.opponent_hand, sample.draw_order)) {
                throw std::runtime_error("sample contradicts the opening or public draw history");
            }
            saw_playable_draw |= playable == 1;
        }
        if (!saw_playable_draw) {
            throw std::runtime_error("the unknown drawn tile must be allowed to match the board");
        }
    }

    candidates.observe(game, game.apply(Move{{5, 0}, Direction::left}).value());
    const auto samples = candidates.sample(game, 16, random);
    if (samples.empty() || std::ranges::any_of(samples, [&](const auto& sample) {
            return sample.opponent_hand.size() != 8 ||
                   !game.with_hidden_tiles(Seat::second, sample.opponent_hand, sample.draw_order);
        })) {
        throw std::runtime_error("the opponent's played tile must leave each candidate hand");
    }
}

void check_bayesian_opening(const Game& game) {
    // Seat one has already played its opening double. Start the model before that public play.
    auto original_hand = game.player(Seat::first).hand;
    original_hand.push_back({6, 6});
    HandCandidates prior(Seat::first, original_hand, 7, true);
    Game after = game;
    const auto result = after.apply(Move{{6, 1}, Direction::down}).value();
    std::mt19937 random(11);
    const auto report = prior.observe_with_inference(
        game, after, result, random, std::chrono::steady_clock::now() + std::chrono::seconds(15), 1);
    if (!report.too_many_hands || report.candidates != 38760 ||
        report.updated || report.good + report.bad + report.forced != 0) {
        throw std::runtime_error("opening inference should keep only the hard update");
    }
    const auto samples = prior.sample(after, 64, random);
    if (samples.size() != 64 || std::ranges::any_of(samples, [&](const auto& sample) {
            return sample.opponent_hand.size() != 6 ||
                   !after.with_hidden_tiles(Seat::first, sample.opponent_hand, sample.draw_order);
        })) {
        throw std::runtime_error("Bayesian inference must preserve hard constraints on its samples");
    }

    std::ostringstream log;
    prior.log_estimates(log, after.player(Seat::second).hand);
    const auto summary = log.str();
    if (!summary.starts_with("Opponent hand estimates:\n38760 possible hands\n"
                             "Uniform hand probability: 0.003%\n") ||
        summary.find("Most likely tiles:\n  [1,1]  30.000%") == std::string::npos ||
        summary.find("Least likely tiles:\n") == std::string::npos ||
        summary.find("Most likely hands:\n") == std::string::npos ||
        summary.find("Actual hand:\n  [6,3] [5,5] [5,4] [5,2] [5,0] [4,4]\n"
                     "  Probability: 0.003%\n  Relative to uniform: 1.00x\n") == std::string::npos ||
        summary.find("Rank: 0 / 38760") != std::string::npos) {
        throw std::runtime_error("hand estimate log must report probabilities and the actual hand's rank");
    }
}

void check_direct_bayesian_update() {
    std::mt19937 random(97);
    bool checked_sampled_orders = false;
    bool checked_all_orders = false;
    for (int attempt = 0; attempt < 12; ++attempt) {
        auto tiles = Domino::all();
        std::shuffle(tiles.begin(), tiles.end(), random);
        Game game;
        if (!game.deal(tiles)) throw std::runtime_error("invalid direct-inference fixture deal");
        std::array candidates{
            HandCandidates(Seat::first, game.player(Seat::first).hand, 7, true),
            HandCandidates(Seat::second, game.player(Seat::second).hand, 7, true),
        };
        const auto opening = game.apply(PlayHighestDouble{}).value();
        for (auto& model : candidates) model.observe(game, opening);
        while (const auto actor = game.active_seat()) {
            const auto actions = game.legal_actions();
            const auto& action = actions[random() % actions.size()];
            Game before = game;
            const auto event = game.apply(action).value();
            if (event.round) break;
            const Seat observer = *actor == Seat::first ? Seat::second : Seat::first;
            auto& model = candidates[static_cast<std::size_t>(observer)];
            const auto own = static_cast<std::size_t>(*actor);
            candidates[own].observe(game, event);
            const auto boneyard_size = game.boneyard().size();
            const auto hand_size = game.player(*actor).hand.size();
            const bool check_orders = !checked_all_orders && boneyard_size >= 2 && boneyard_size <= 3 && hand_size <= 3;
            const bool check_hands = !checked_sampled_orders && boneyard_size > 3 && hand_size == 1;
            if (event.placement && (check_orders || check_hands)) {
                auto expired = model;
                const auto report = model.observe_with_inference(
                    before, game, event, random, std::chrono::steady_clock::now() + std::chrono::seconds(15), 1);
                if (report.candidates > 1) {
                    if (report.too_many_hands || report.timed_out || report.incomplete ||
                        report.good + report.bad + report.forced != report.candidates) {
                        throw std::runtime_error("late inference should finish every feasible hand through settlement");
                    }
                    auto hard_only = expired;
                    hard_only.observe(game, event);
                    const auto timeout = expired.observe_with_inference(
                        before, game, event, random, std::chrono::steady_clock::now(), 1);
                    std::ostringstream expired_log, hard_log;
                    expired.log_estimates(expired_log, game.player(*actor).hand);
                    hard_only.log_estimates(hard_log, game.player(*actor).hand);
                    if (!timeout.timed_out || timeout.updated || timeout.candidates != report.candidates ||
                        expired_log.str() != hard_log.str()) {
                        throw std::runtime_error("expired inference must retain only the completed hard update");
                    }
                    std::mt19937 sampling(11);
                    const auto coverage = model.sample_for_search(game, 1, sampling);
                    constexpr std::array<std::size_t, 4> permutations{1, 1, 2, 6};
                    const bool all_orders = boneyard_size < permutations.size();
                    const auto orders = all_orders ? permutations[boneyard_size] : 1;
                    std::map<TileMask, std::set<std::vector<std::size_t>>> sequences;
                    double weight = 0.0;
                    double actual_weight = 0.0;
                    const auto hand_mask = [](const std::vector<Domino>& hand) {
                        TileMask mask = 0;
                        for (auto tile : hand) mask |= tile_bit(tile);
                        return mask;
                    };
                    const auto actual = hand_mask(game.player(*actor).hand);
                    for (const auto& world : coverage.worlds) {
                        const auto hand = hand_mask(world.opponent_hand);
                        std::vector<std::size_t> order;
                        for (auto tile : world.draw_order) order.push_back(tile.id());
                        sequences[hand].insert(std::move(order));
                        weight += world.weight;
                        if (hand == actual) actual_weight += world.weight;
                    }
                    if (!coverage.all_hands || coverage.all_draw_orders != all_orders ||
                        sequences.size() != report.candidates ||
                        coverage.worlds.size() != report.candidates * orders ||
                        !sequences.contains(actual) ||
                        std::ranges::any_of(sequences, [orders](const auto& entry) {
                            return entry.second.size() != orders;
                        })) {
                        throw std::runtime_error("late search must cover all feasible hands and small draw orders");
                    }
                    std::ostringstream log;
                    model.log_estimates(log, game.player(*actor).hand);
                    const auto summary = log.str();
                    const auto start = summary.find("  Probability: ") + std::string_view("  Probability: ").size();
                    const double logged = std::stod(summary.substr(start)) / 100.0;
                    if (std::abs(actual_weight / weight - logged) > 0.00000501) {
                        throw std::runtime_error("late search must retain posterior hand probabilities");
                    }
                    checked_sampled_orders |= !all_orders;
                    checked_all_orders |= all_orders;
                    if (checked_sampled_orders && checked_all_orders) return;
                }
            } else {
                model.observe(game, event);
            }
        }
    }
    throw std::runtime_error("the fixture did not cover both late-game draw regimes");
}

}  // namespace

int main(int argc, char* argv[]) {
    try {
        if (argc != 2) throw std::runtime_error("supply the fixed search fixture path");
        std::ifstream fixture(argv[1]);
        std::string deal_line;
        if (!std::getline(fixture, deal_line)) throw std::runtime_error("missing deal");
        std::istringstream ids(deal_line);
        std::vector<Domino> tiles;
        std::size_t id;
        while (ids >> id) tiles.push_back(Domino::all().at(id));

        Game game;
        if (!game.deal(tiles) || !game.apply(PlayHighestDouble{}) ||
            game.active_seat() != Seat::second) {
            throw std::runtime_error("could not set up the fixed position");
        }

        for (int depth : {1, 2}) {
            const auto scores = score_fixed_actions(game, Seat::second, depth);
            if (scores.size() != 2) throw std::runtime_error("expected two root moves");
            for (const auto& scored : scores) {
                int expected_depth;
                std::size_t tile_id;
                double lead;
                double runway;
                if (!(fixture >> expected_depth >> tile_id >> lead >> runway) ||
                    expected_depth != depth ||
                    scored.action.index() != 0 ||
                    std::get<Move>(scored.action) != Move{Domino::all().at(tile_id), Direction::left} ||
                    std::abs(scored.score - std::tanh(lead / runway)) > 1e-12) {
                    throw std::runtime_error("root move or score mismatch at depth " +
                                             std::to_string(depth));
                }
            }
        }
        check_sampled_hands(game);
        check_sampled_draws(game);
        check_public_draw_evidence();
        check_bayesian_opening(game);
        check_direct_bayesian_update();
    } catch (const std::exception& error) {
        std::cerr << error.what() << '\n';
        return 1;
    }
}
