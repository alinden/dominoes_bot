#include "dominoes/game.hpp"

#include <algorithm>
#include <cmath>
#include <iostream>
#include <numeric>
#include <random>
#include <stdexcept>

using namespace dominoes;

namespace {

void check(bool condition, const char* message) {
    if (!condition) {
        throw std::runtime_error(message);
    }
}

Seat opponent(Seat seat) {
    return seat == Seat::first ? Seat::second : Seat::first;
}

void check_opening_and_draws() {
    auto tiles = Domino::all();
    std::stable_partition(tiles.begin(), tiles.end(), [](Domino tile) {
        return !tile.is_double();
    });
    Game game;
    check(game.deal(tiles), "accept a complete deal without doubles in the hands");
    check(!game.apply(Draw{}), "ordinary play waits for the opening double");
    check(!game.apply(PlayHighestDouble{}).value().actor, "no double requests a redeal");
    check(game.phase() == Phase::awaiting_deal, "the caller supplies the next deal");

    tiles = Domino::all();
    std::swap(tiles[7], tiles[20]); // Seat two has [5,5], the highest dealt double.
    auto invalid = tiles;
    invalid.back() = invalid.front();
    check(!game.deal(invalid) && game.phase() == Phase::awaiting_deal,
          "a duplicate tile rejects the deal without starting a round");
    check(game.deal(tiles), "accept the replacement deal");
    const auto opening = game.apply(PlayHighestDouble{}).value();
    check(opening.actor == Seat::second && opening.placement->tile == Domino(5, 5),
          "the highest double opens, including when held by seat two");
    check(opening.play_points == 10 && game.active_seat() == Seat::first,
          "the opener scores and the other seat takes the turn");
    check(!game.apply(Pass{}) && !game.apply(Move{{5, 0}, Direction::left}),
          "reject passing with a boneyard and playing a tile outside the hand");

    for (const auto expected : {Domino(4, 4), Domino(5, 0)}) {
        check(game.legal_actions().size() == 1 &&
                  std::holds_alternative<Draw>(game.legal_actions().front()),
              "draw is the only action until a tile fits");
        const auto hand_size = game.player(Seat::first).hand.size();
        const auto draw = game.apply(Draw{}).value();
        check(draw.pre_draw_hand_size == hand_size && draw.pre_draw_playable_tiles.size() == 6,
              "draw evidence describes the position before drawing");
        check(game.active_seat() == Seat::first && game.player(Seat::first).hand.front() == expected,
              "each draw prepends the next tile and preserves the turn");
    }
    check(game.legal_moves().size() == 4 && game.legal_actions().size() == 1,
          "the spinner accepts all four directions and canonicalizes to one");
    check(!game.apply(Draw{}), "drawing is illegal when a placement is available");
    check(game.apply(Move{{5, 0}, Direction::down}).has_value(),
          "a legal noncanonical direction can still be played");
}

int remaining_pips(const Game& game, Seat seat) {
    const auto& hand = game.player(seat).hand;
    return std::accumulate(hand.begin(), hand.end(), 0,
                           [](int total, Domino tile) { return total + tile.pip_total(); });
}

void check_complete_matches() {
    std::mt19937 random(12345);
    bool saw_lock = false;
    bool saw_dominoes = false;
    bool saw_mid_round_threshold = false;
    for (int match = 0; match < 20; ++match) {
        Game game;
        std::optional<Seat> next_opener;
        bool previous_pass = false;
        for (int turn = 0; turn < 2000 && !game.winner(); ++turn) {
            if (game.phase() == Phase::awaiting_deal) {
                auto tiles = Domino::all();
                for (std::size_t size = tiles.size(); size > 1; --size) {
                    std::swap(tiles[size - 1], tiles[random() % size]);
                }
                check(game.deal(tiles), "accept a shuffled full deal");
                check(game.active_seat() == next_opener, "the round result selects the next opener");
                previous_pass = false;
            }
            const auto actions = game.legal_actions();
            check(!actions.empty(), "every live turn has an action");
            const auto& action = actions[random() % actions.size()];
            const bool pass = std::holds_alternative<Pass>(action);
            const auto result = game.apply(action).value();
            const int first = game.player(Seat::first).score;
            const int second = game.player(Seat::second).score;
            if (result.round) {
                const bool locked = result.round->reason == RoundEnd::locked;
                saw_lock |= locked;
                saw_dominoes |= !locked;
                check(locked == (pass && previous_pass), "only consecutive passes lock a round");
                const auto actor = *result.actor;
                const int opponent_pips = remaining_pips(game, opponent(actor));
                const int difference = opponent_pips - remaining_pips(game, actor);
                const int pips = locked ? std::abs(difference) : opponent_pips;
                const int bonus = static_cast<int>(std::round(pips / 5.0)) * 5;
                const auto recipient = locked && difference < 0 ? opponent(actor) : actor;
                check(result.round->bonus == bonus &&
                          result.round->bonus_recipient == (bonus ? std::optional{recipient} : std::nullopt),
                      "round settlement awards the rounded bonus to the correct seat");
                const bool finished = std::max(first, second) >= 150 && first != second;
                check(game.phase() == (finished ? Phase::match_complete : Phase::awaiting_deal),
                      "a settled score of at least 150 and a lead wins the match");
                next_opener = locked ? std::nullopt : std::optional{actor};
            } else {
                check(!(pass && previous_pass), "the second consecutive pass must end the round");
                saw_mid_round_threshold |= std::max(first, second) >= 150;
                check(!game.winner(), "crossing 150 during play does not end the match");
            }
            check(game.board().size() + game.boneyard().size() + game.player(Seat::first).hand.size() +
                      game.player(Seat::second).hand.size() == Domino::tile_count,
                  "the engine accounts for all 28 tiles");
            previous_pass = pass;
        }
        check(game.winner().has_value(), "the headless runner completes a match");
        check(!game.deal(Domino::all()) && !game.apply(Pass{}), "a completed match rejects actions");
    }
    check(saw_lock && saw_dominoes && saw_mid_round_threshold,
          "replays exercise both round endings and the mid-round scoring threshold");
}

}  // namespace

int main() {
    try {
        check_opening_and_draws();
        check_complete_matches();
    } catch (const std::exception& error) {
        std::cerr << error.what() << '\n';
        return 1;
    }
}
