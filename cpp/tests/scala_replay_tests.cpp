#include "dominoes/game.hpp"

#include <fstream>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>

using namespace dominoes;

namespace {

constexpr std::string_view directions = "LRUD";

template <typename Range, typename Format>
std::string join(const Range& range, Format format) {
    std::string text;
    for (const auto& item : range) {
        if (!text.empty()) {
            text += ',';
        }
        text += format(item);
    }
    return text;
}

std::string tile_code(Domino tile) { return std::to_string(tile.id()); }

std::string move_code(const Move& move) {
    return directions.at(static_cast<std::size_t>(move.direction)) + tile_code(move.tile);
}

std::string action_code(const Action& action) {
    if (const auto* move = std::get_if<Move>(&action)) {
        return move_code(*move);
    }
    if (std::holds_alternative<Draw>(action)) {
        return "D";
    }
    return std::holds_alternative<Pass>(action) ? "P" : "H";
}

std::string seat_code(std::optional<Seat> seat) {
    return seat ? std::to_string(static_cast<int>(*seat)) : "-";
}

std::string played_code(const PlayedDomino& placed) {
    return tile_code(placed.tile()) + ':' + std::to_string(placed.exposed_pip());
}

std::string_view phase_code(Phase phase) {
    switch (phase) {
    case Phase::awaiting_deal: return "deal";
    case Phase::awaiting_highest_double: return "opening";
    case Phase::playing: return "play";
    case Phase::match_complete: return "done";
    }
    throw std::logic_error("unknown phase");
}

std::string position(const Game& game, bool locked) {
    const auto& board = game.board();
    std::ostringstream snapshot;
    snapshot << "S " << phase_code(game.phase()) << ' ' << seat_code(game.active_seat())
             << ' ' << seat_code(game.winner()) << ' ' << game.player(Seat::first).score
             << ':' << game.player(Seat::second).score << ' ' << board.score() << ' ';
    if (board.spinner()) {
        snapshot << played_code(*board.spinner());
    }
    for (std::size_t i = 0; i < directions.size(); ++i) {
        snapshot << '|' << join(board.chain(static_cast<Direction>(i)), played_code);
    }
    // Scala clears locked hands after scoring. C++ retains them until the next deal.
    snapshot << ' ' << (locked ? "" : join(game.player(Seat::first).hand, tile_code))
             << '|' << (locked ? "" : join(game.player(Seat::second).hand, tile_code))
             << '|' << join(game.boneyard(), tile_code) << ' '
             << join(game.legal_moves(), move_code) << '|' << join(game.legal_actions(), action_code);
    return snapshot.str();
}

std::string event(const TurnResult& result) {
    std::ostringstream line;
    line << "E " << seat_code(result.actor) << ' '
         << (result.placement ? move_code(*result.placement) : "-") << ' ' << result.play_points << ' ';
    if (result.round) {
        line << (result.round->reason == RoundEnd::locked ? "lock" : "out") << ':'
             << seat_code(result.round->bonus_recipient) << ':' << result.round->bonus;
    } else {
        line << "-:-:0";
    }
    if (result.pre_draw_hand_size) {
        line << ' ' << *result.pre_draw_hand_size << ':' << join(result.pre_draw_playable_tiles, tile_code);
    } else {
        line << " -";
    }
    return line.str();
}

Action read_action(const std::string& code) {
    if (code == "H") return PlayHighestDouble{};
    if (code == "D") return Draw{};
    if (code == "P") return Pass{};
    const auto direction = directions.find(code.at(0));
    if (direction == std::string_view::npos) {
        throw std::runtime_error("invalid move direction");
    }
    return Move{Domino::all().at(std::stoul(code.substr(1))), static_cast<Direction>(direction)};
}

class Trace {
public:
    explicit Trace(const char* path) : input_(path) {
        if (!input_) throw std::runtime_error("could not open Scala replay fixture");
    }

    bool read(std::string& line) {
        ++line_number_;
        return static_cast<bool>(std::getline(input_, line));
    }

    void expect(const std::string& actual) {
        std::string expected;
        if (!read(expected) || actual != expected) {
            fail("expected " + expected + "\nactual   " + actual);
        }
    }

    [[noreturn]] void fail(const std::string& message) const {
        throw std::runtime_error("fixture line " + std::to_string(line_number_) + ": " + message);
    }

private:
    std::ifstream input_;
    unsigned line_number_ = 0;
};

void replay(Trace& trace) {
    Game game;
    std::string line;
    while (trace.read(line)) {
        if (line == "END") {
            if (!game.winner()) trace.fail("replay ended before match completion");
            return;
        }
        if (line.starts_with("DEAL ")) {
            std::istringstream ids(line.substr(5));
            std::vector<Domino> tiles;
            std::string id;
            while (std::getline(ids, id, ',')) {
                tiles.push_back(Domino::all().at(std::stoul(id)));
            }
            if (!game.deal(tiles)) trace.fail("C++ rejected the scripted deal");
            trace.expect(position(game, false));
        } else if (line.starts_with("ACT ")) {
            const auto result = game.apply(read_action(line.substr(4)));
            if (!result) trace.fail("C++ rejected " + line);
            trace.expect(event(*result));
            trace.expect(position(game, result->round && result->round->reason == RoundEnd::locked));
        } else {
            trace.fail("expected a deal, action, or end marker");
        }
    }
    trace.fail("missing end marker");
}

}  // namespace

int main(int argc, char* argv[]) {
    try {
        if (argc != 2) throw std::runtime_error("supply the Scala replay fixture path");
        Trace trace(argv[1]);
        std::string line;
        int matches = 0;
        while (trace.read(line)) {
            if (!line.starts_with("MATCH ")) trace.fail("expected a match marker");
            replay(trace);
            ++matches;
        }
        if (matches == 0) trace.fail("no matches in fixture");
    } catch (const std::exception& error) {
        std::cerr << error.what() << '\n';
        return 1;
    }
}
