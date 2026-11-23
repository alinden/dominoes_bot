#include "dominoes/greedy.hpp"
#include "dominoes/hand_candidates.hpp"
#include "dominoes/neural_search.hpp"

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
constexpr int inference_seconds = 15;
constexpr const char* directions[]{"left", "right", "up", "down"};
std::optional<NeuralSearch> neural_models;

struct Session {
    Game game;
    std::mt19937 random;
    std::mt19937 inference_random;
    std::optional<HandCandidates> candidates;
    std::optional<Game> before;
    std::optional<TurnResult> pending;
    std::string notice;
    std::optional<Domino> last_played;
    int bonefish_draws = 0;
    std::array<int, 2> last_points{};
    bool round_ended = false;
    bool fast;

    Session(unsigned seed, bool fast) : random(seed), inference_random(seed ^ 0x9e3779b9U), fast(fast) {}

    void deal() {
        last_played.reset();
        round_ended = false;
        bonefish_draws = 0;
        last_points = {};
        auto tiles = Domino::all();
        std::shuffle(tiles.begin(), tiles.end(), random);
        if (!game.deal(tiles)) throw std::logic_error("Engine rejected the deal");
        candidates.emplace(robot, game.player(robot).hand, game.player(human).hand.size(), true);
    }

    void announce(const TurnResult& result) {
        if (result.placement) last_played = result.placement->tile;
        round_ended = result.round.has_value();
        last_points = {};
        if (!result.actor) {
            notice = "Nobody had a double. Redealing.";
            return;
        }
        const auto name = [](Seat seat) { return seat == human ? "You" : "Bonefish"; };
        std::ostringstream out;
        const bool drew = std::holds_alternative<Draw>(result.action);
        if (result.actor == robot) {
            if (drew) ++bonefish_draws;
            if (bonefish_draws > 0) {
                out << "Bonefish drew " << bonefish_draws << (bonefish_draws == 1 ? " tile" : " tiles");
                if (drew) {
                    notice = out.str();
                    return;
                }
                out << "\\n";
                bonefish_draws = 0;
            }
        }
        out << name(*result.actor) << ' ';
        if (result.placement) {
            const auto& move = *result.placement;
            out << "played [" << move.tile.high() << ',' << move.tile.low() << ']';
            if (std::holds_alternative<PlayHighestDouble>(result.action)) out << " to start the round";
            else out << ' ' << directions[static_cast<int>(move.direction)];
        } else {
            out << (drew ? "drew a tile" : "passed");
        }
        if (result.round) out << (result.round->reason == RoundEnd::locked ? "\\nLocked round." : "\\nDominoes!");
        for (auto seat : {human, robot}) {
            int points = result.actor == seat ? result.play_points : 0;
            if (result.round && result.round->bonus_recipient == seat) points += result.round->bonus;
            last_points[static_cast<int>(seat)] = points;
            if (points > 0) out << "\\n" << name(seat) << " scored " << points << " points";
        }
        notice = out.str();
    }

    bool apply(const Action& action) {
        const bool infer = candidates && game.active_seat() == human &&
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
            const auto deadline = std::chrono::steady_clock::now() + std::chrono::seconds(fast ? 2 : 10);
            const auto samples = candidates->sample_for_search(game, 16, random);
            if (!samples.worlds.empty()) {
                const auto& model = neural_models.value();
                const auto phases = model.phases();
                const int full_depth = NeuralSearch::max_depth(game);
                const int max_depth = fast ? std::min(9, full_depth) : full_depth;
                const auto scores =
                    search_until(game, robot, max_depth, samples.worlds, deadline, 1, model.options(phases)).scores;
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

EMSCRIPTEN_KEEPALIVE int load_neural_models(const char* opening_weights, const char* midgame_weights,
                                           const char* late_weights) {
    try {
        std::istringstream opening(opening_weights);
        std::istringstream midgame(midgame_weights);
        std::istringstream late(late_weights);
        NeuralSearch models{ValueOrder(opening), ValueOrder(midgame), ValueOrder(late)};
        session.reset();
        neural_models.emplace(std::move(models));
        return 1;
    } catch (const std::exception&) {
        return 0;
    }
}

EMSCRIPTEN_KEEPALIVE void start_game(unsigned seed, int fast) {
    session.emplace(seed, fast != 0);
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

EMSCRIPTEN_KEEPALIVE const char* game_state() {
    const auto& current = session.value();
    const auto& game = current.game;
    const auto& board = game.board();
    std::ostringstream out;
    out << "{\"phase\":" << static_cast<int>(game.phase())
        << ",\"active\":" << seat_id(game.active_seat())
        << ",\"winner\":" << seat_id(game.winner())
        << ",\"pendingInference\":" << (current.pending ? "true" : "false")
        << ",\"roundEnded\":" << (current.round_ended ? "true" : "false")
        << ",\"notice\":\"" << current.notice << "\""
        << ",\"humanScore\":" << game.player(human).score
        << ",\"botScore\":" << game.player(robot).score
        << ",\"humanPoints\":" << current.last_points[static_cast<int>(human)]
        << ",\"botPoints\":" << current.last_points[static_cast<int>(robot)] << ",\"humanHand\":";
    write_array(out, game.player(human).hand, [&](Domino tile) { out << tile.id(); });
    out << ",\"botHand\":";
    write_array(out, game.player(robot).hand, [&](Domino tile) {
        out << (current.round_ended ? static_cast<int>(tile.id()) : -1);
    });
    out << ",\"boneyard\":" << game.boneyard().size()
        << ",\"lastPlayed\":" << (current.last_played ? static_cast<int>(current.last_played->id()) : -1)
        << ",\"spinner\":" << (board.spinner() ? static_cast<int>(board.spinner()->tile().id()) : -1)
        << ",\"chains\":[";
    for (int i = 0; i < draw; ++i) {
        if (i) out << ',';
        write_array(out, board.chain(static_cast<Direction>(i)), [&](const auto& tile) {
            out << '[' << tile.exposed_pip() << ',' << tile.reversed().exposed_pip() << ']';
        });
    }
    out << "],\"actions\":";
    const bool human_turn = game.active_seat() == human && !current.pending;
    const auto actions = human_turn ? game.legal_actions() : std::vector<Action>{};
    write_array(out, actions, [&](const Action& action) {
        if (const auto* move = std::get_if<Move>(&action)) {
            out << '[' << move->tile.id() << ',' << static_cast<int>(move->direction) << ']';
        } else {
            out << "[-1," << (std::holds_alternative<Draw>(action) ? draw : pass) << ']';
        }
    });
    out << ",\"boardMoves\":";
    // The search deduplicates equivalent moves, but players can use any physical end.
    auto moves = human_turn ? game.legal_moves() : std::vector<Move>{};
    if (board.shape() == BoardShape::empty || board.shape() == BoardShape::spinner_only) {
        std::erase_if(moves, [](const Move& move) { return move.direction != Direction::left; });
    }
    write_array(out, moves, [&](const Move& move) {
        out << '[' << move.tile.id() << ',' << static_cast<int>(move.direction) << ']';
    });
    out << '}';
    response = out.str();
    return response.c_str();
}

}  // extern "C"
