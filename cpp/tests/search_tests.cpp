#include "dominoes/search.hpp"
#include "dominoes/hand_candidates.hpp"

#include <algorithm>
#include <cmath>
#include <fstream>
#include <iostream>
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
    if (scores.size() != 2 || std::get<Move>(scores.front().action) != Move{{6, 3}, Direction::left}) {
        throw std::runtime_error("sampled search did not preserve the root moves");
    }
    expect_score(scores.front().score, 0.25 * std::tanh(10.0 / 135) + 0.75 * std::tanh(15.0 / 135),
                 "a counterplay available in one hand must receive only that hand's weight");
    expect_score(scores.back().score, 0.0, "the second root move scores zero against both hands");

    const auto expired = search_until(game, Seat::second, 68, samples, std::chrono::steady_clock::now());
    if (!expired.timed_out || expired.depth != 0 || !expired.scores.empty()) {
        throw std::runtime_error("an expired deadline must discard the unfinished search");
    }
    const auto deadline = std::chrono::steady_clock::now() + std::chrono::seconds(5);
    const auto timed = search_until(game, Seat::second, 2, samples, deadline);
    if (timed.timed_out || timed.depth != 2 || timed.reached_round_end ||
        timed.scores.size() != scores.size()) {
        throw std::runtime_error("timed search should complete the requested shallow pass");
    }
    expect_score(timed.scores.front().score, scores.front().score,
                 "timed search must reuse the supplied samples");

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
    second.weight = 3.0;
    draw_at(first, {2, 1}, 0); // Miss, then draw a tile that scores five.
    draw_at(first, {6, 2}, 1);
    draw_at(second, {6, 5}, 0); // Playable, but scores zero.
    const std::array samples{first, second};
    const auto scores = score_sampled_actions(drawing, Seat::first, 2, samples);
    if (scores.size() != 1 || !std::holds_alternative<Draw>(scores.front().action)) {
        throw std::runtime_error("a blocked hand must draw");
    }
    expect_score(scores.front().score, -0.25 * std::tanh(10.0 / 135) - 0.75 * std::tanh(15.0 / 135),
                 "draw branches must average their weighted replies and repeated draws share a ply");
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
    } catch (const std::exception& error) {
        std::cerr << error.what() << '\n';
        return 1;
    }
}
