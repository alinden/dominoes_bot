#include "dominoes/greedy.hpp"
#include "dominoes/hand_candidates.hpp"

#include <emscripten/emscripten.h>

#include <algorithm>
#include <chrono>
#include <sstream>
#include <stdexcept>
#include <string>

namespace {

using namespace dominoes;

constexpr Seat human = Seat::first;
constexpr Seat robot = Seat::second;
constexpr int draw = 4;
constexpr int pass = 5;
constexpr const char* directions[]{"left", "right", "up", "down"};

struct Session {
    Game game;
    std::mt19937 random;
    std::mt19937 inference_random;
    std::optional<HandCandidates> candidates;
    std::optional<Game> before;
    std::optional<TurnResult> pending;
    std::string notice;
    int depth;
    int breadth;
    int search_seconds;
    int inference_seconds;
    bool search;

    Session(unsigned seed, bool search, int depth, int breadth, int search_seconds, int inference_seconds)
        : random(seed), inference_random(seed ^ 0x9e3779b9U), depth(depth), breadth(breadth),
          search_seconds(search_seconds), inference_seconds(inference_seconds), search(search) {}

    void deal() {
        auto tiles = Domino::all();
        std::shuffle(tiles.begin(), tiles.end(), random);
        if (!game.deal(tiles)) throw std::logic_error("Engine rejected the deal");
        if (search) candidates.emplace(robot, game.player(robot).hand, game.player(human).hand.size(),
                                       inference_seconds > 0);
    }

    void announce(const TurnResult& result) {
        if (!result.actor) {
            notice = "Nobody had a double. Redealing.";
            return;
        }
        const auto name = [](Seat seat) { return seat == human ? "human" : "robot"; };
        std::ostringstream out;
        out << name(*result.actor) << ": ";
        if (result.placement) {
            const auto& move = *result.placement;
            out << "Play [" << move.tile.high() << ',' << move.tile.low() << ']';
            if (std::holds_alternative<PlayHighestDouble>(result.action)) out << " to start the round";
            else out << ' ' << directions[static_cast<int>(move.direction)];
        } else {
            out << (std::holds_alternative<Draw>(result.action) ? "Draw" : "Pass");
        }
        if (result.round) out << (result.round->reason == RoundEnd::locked ? "\\nLocked round." : "\\nDominoes!");
        for (auto seat : {human, robot}) {
            int points = result.actor == seat ? result.play_points : 0;
            if (result.round && result.round->bonus_recipient == seat) points += result.round->bonus;
            if (points > 0) out << "\\n" << name(seat) << ": score " << points << " points";
        }
        notice = out.str();
    }

    bool apply(const Action& action) {
        const bool infer = candidates && inference_seconds > 0 && game.active_seat() == human &&
                           std::holds_alternative<Move>(action);
        if (infer) before = game;
        const auto result = game.apply(action);
        if (!result) {
            before.reset();
            return false;
        }
        announce(*result);
        if (infer) pending = result;
        else if (candidates) candidates->observe(game, *result);
        return true;
    }

    Action choose() {
        if (candidates) {
            const auto legal = game.legal_actions();
            if (legal.size() == 1) return legal.front();
            const auto deadline = std::chrono::steady_clock::now() + std::chrono::seconds(search_seconds);
            const auto samples = candidates->sample_for_search(game, breadth, random);
            if (!samples.worlds.empty()) {
                const auto scores = search_seconds == 0
                    ? score_sampled_actions(game, robot, depth, samples.worlds)
                    : search_until(game, robot, depth, samples.worlds, deadline).scores;
                if (!scores.empty()) return std::ranges::max_element(scores, {}, &ScoredAction::score)->action;
            }
        }
        return choose_greedy_action(game, random).value();
    }

    void advance() {
        if (pending) {
            const auto deadline = std::chrono::steady_clock::now() + std::chrono::seconds(inference_seconds);
            candidates->observe_with_inference(*before, game, *pending, inference_random, deadline, 1);
            before.reset();
            pending.reset();
        } else if (game.phase() == Phase::awaiting_deal) {
            deal();
        } else if (game.phase() == Phase::awaiting_highest_double) {
            apply(PlayHighestDouble{});
        } else if (game.phase() == Phase::playing && game.active_seat() == robot) {
            apply(choose());
        }
    }
};

std::optional<Session> session;
std::string response;

template <typename Range, typename Write>
void write_array(std::ostream& out, const Range& values, Write write) {
    out << '[';
    bool first = true;
    for (const auto& value : values) {
        if (!first) out << ',';
        first = false;
        write(value);
    }
    out << ']';
}

int seat_id(std::optional<Seat> seat) {
    return seat ? static_cast<int>(*seat) : -1;
}

}  // namespace

extern "C" {

EMSCRIPTEN_KEEPALIVE void start_game(unsigned seed, int search, int depth, int breadth,
                                    int search_seconds, int inference_seconds) {
    session.emplace(seed, search != 0, depth, breadth, search_seconds, inference_seconds);
    session->deal();
}

EMSCRIPTEN_KEEPALIVE void advance_game() {
    session.value().advance();
}

EMSCRIPTEN_KEEPALIVE int play_action(int tile, int direction) {
    auto& current = session.value();
    if (current.pending || current.game.active_seat() != human) return 0;
    if (direction == draw) return current.apply(Draw{});
    if (direction == pass) return current.apply(Pass{});
    if (tile < 0 || tile >= static_cast<int>(Domino::tile_count) || direction < 0 || direction >= draw) return 0;
    return current.apply(Move{Domino::all()[tile], static_cast<Direction>(direction)});
}

EMSCRIPTEN_KEEPALIVE const char* game_state(int show_bot_hand) {
    const auto& current = session.value();
    const auto& game = current.game;
    const auto& board = game.board();
    std::ostringstream out;
    out << "{\"phase\":" << static_cast<int>(game.phase())
        << ",\"active\":" << seat_id(game.active_seat())
        << ",\"winner\":" << seat_id(game.winner())
        << ",\"pendingInference\":" << (current.pending ? "true" : "false")
        << ",\"notice\":\"" << current.notice << "\""
        << ",\"humanScore\":" << game.player(human).score
        << ",\"botScore\":" << game.player(robot).score << ",\"humanHand\":";
    write_array(out, game.player(human).hand, [&](Domino tile) { out << tile.id(); });
    out << ",\"botHand\":";
    write_array(out, game.player(robot).hand, [&](Domino tile) {
        if (show_bot_hand) out << tile.id();
        else out << -1;
    });
    out << ",\"boneyard\":" << game.boneyard().size()
        << ",\"spinner\":" << (board.spinner() ? static_cast<int>(board.spinner()->tile().id()) : -1)
        << ",\"boardScore\":" << board.score() << ",\"chains\":[";
    for (int i = 0; i < draw; ++i) {
        if (i) out << ',';
        write_array(out, board.chain(static_cast<Direction>(i)), [&](const auto& tile) {
            out << '[' << tile.exposed_pip() << ',' << tile.reversed().exposed_pip() << ']';
        });
    }
    out << "],\"actions\":";
    const auto actions = game.active_seat() == human && !current.pending ? game.legal_actions() : std::vector<Action>{};
    write_array(out, actions, [&](const Action& action) {
        if (const auto* move = std::get_if<Move>(&action)) {
            out << '[' << move->tile.id() << ',' << static_cast<int>(move->direction) << ']';
        } else {
            out << "[-1," << (std::holds_alternative<Draw>(action) ? draw : pass) << ']';
        }
    });
    out << '}';
    response = out.str();
    return response.c_str();
}

}  // extern "C"
