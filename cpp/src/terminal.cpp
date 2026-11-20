#include "dominoes/greedy.hpp"

#include <algorithm>
#include <charconv>
#include <cctype>
#include <chrono>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <thread>

namespace {

using namespace dominoes;
using namespace std::chrono_literals;

constexpr std::array<std::string_view, 4> direction_names{"left", "right", "up", "down"};

std::string_view name(Seat seat) {
    return seat == Seat::first ? "human" : "robot";
}

void print_tile(int first, int second) {
    std::cout << '[' << first << ',' << second << ']';
}

void print_tile(Domino tile) {
    print_tile(tile.high(), tile.low());
}

void print_board(const Game& game, bool show_bot_hand) {
    std::cout << "\n----------------------------------\n";
    for (auto seat : {Seat::first, Seat::second}) {
        const auto& player = game.player(seat);
        std::cout << name(seat) << (game.active_seat() == seat ? "*" : "")
                  << '(' << player.score << "):";
        for (auto tile : player.hand) {
            std::cout << ' ';
            if (seat == Seat::first || show_bot_hand) {
                print_tile(tile);
            } else {
                std::cout << "[x,x]";
            }
        }
        std::cout << '\n';
    }
    std::cout << "boneyard:";
    for (std::size_t i = 0; i < game.boneyard().size(); ++i) {
        std::cout << " [x,x]";
    }
    const auto& board = game.board();
    std::cout << "\ntiles: " << board.size() + game.boneyard().size() +
        game.player(Seat::first).hand.size() + game.player(Seat::second).hand.size();
    std::cout << "\nspinner: ";
    if (board.spinner()) {
        print_tile(board.spinner()->tile());
    } else {
        std::cout << "None";
    }
    for (std::size_t i = 0; i < direction_names.size(); ++i) {
        std::cout << '\n' << direction_names[i] << ':';
        std::string_view separator = " ";
        for (const auto& placed : board.chain(static_cast<Direction>(i))) {
            std::cout << separator;
            print_tile(placed.exposed_pip(), placed.reversed().exposed_pip());
            separator = " - ";
        }
    }
    std::cout << "\nboard score: " << board.score() << '\n';
}

void print_moves_help(const std::vector<Domino>& hand) {
    std::cout << "Your turn\nTiles:\n";
    for (std::size_t i = 0; i < hand.size(); ++i) {
        std::cout << i << ": ";
        print_tile(hand[i]);
        std::cout << '\n';
    }
    std::cout << "To move your 3rd tile left, type 'm 2 l'\n"
                 "Directions: l (left), r (right), u (up), d (down)\n"
                 "To draw, type 'd'\nTo pass, type 'p'\n";
}

std::optional<Action> parse_move(std::istringstream& input, const std::vector<Domino>& hand) {
    std::string index_text;
    std::string direction_text;
    if (!(input >> index_text >> direction_text)) {
        return std::nullopt;
    }
    std::size_t index = 0;
    const auto* end = index_text.data() + index_text.size();
    const auto [parsed, error] = std::from_chars(index_text.data(), end, index);
    const auto direction = std::string_view("lrud").find(static_cast<char>(
        std::tolower(static_cast<unsigned char>(direction_text.front()))));
    if (error != std::errc{} || parsed != end || index >= hand.size() ||
        direction == std::string_view::npos) {
        return std::nullopt;
    }
    return Move{hand[index], static_cast<Direction>(direction)};
}

std::optional<TurnResult> play_human(Game& game) {
    const auto& hand = game.player(Seat::first).hand;
    std::string line;
    while (std::cout << "Your move (h for help): " && std::getline(std::cin, line)) {
        std::istringstream input(line);
        std::string command;
        input >> command;
        if (command == "h") {
            print_moves_help(hand);
            continue;
        }
        std::optional<Action> action;
        if (command == "m") {
            action = parse_move(input, hand);
        } else if (command == "d") {
            action = Draw{};
        } else if (command == "p") {
            action = Pass{};
        }
        if (action) {
            if (auto result = game.apply(*action)) {
                return result;
            }
            std::cout << "Illegal action.\n";
        } else {
            std::cout << "Invalid command. Type 'h' for help.\n";
        }
    }
    std::cout << '\n';
    return std::nullopt;
}

void announce(const TurnResult& result) {
    if (!result.actor) {
        std::cout << "Nobody had a double. Redealing.\n";
        return;
    }
    std::cout << name(*result.actor) << ": ";
    if (result.placement) {
        std::cout << "Play ";
        print_tile(result.placement->tile);
        if (std::holds_alternative<PlayHighestDouble>(result.action)) {
            std::cout << " to start the round";
        } else {
            std::cout << ' ' << direction_names.at(static_cast<std::size_t>(result.placement->direction));
        }
    } else {
        std::cout << (std::holds_alternative<Draw>(result.action) ? "Draw" : "Pass");
    }
    std::cout << '\n';
    if (result.round) {
        std::cout << (result.round->reason == RoundEnd::locked ? "Locked round.\n" : "Dominoes!\n");
    }
    for (auto seat : {Seat::first, Seat::second}) {
        int points = result.actor == seat ? result.play_points : 0;
        if (result.round && result.round->bonus_recipient == seat) {
            points += result.round->bonus;
        }
        if (points > 0) {
            std::cout << name(seat) << ": score " << points << " points\n";
        }
    }
}

void show_robot_thinking() {
    std::cout << std::flush;
    for (auto dots : {".", "..", "..."}) {
        std::this_thread::sleep_for(500ms);
        std::cout << dots << std::endl;
    }
}

int play_match(bool show_bot_hand) {
    Game game;
    std::mt19937 random(std::random_device{}());
    std::cout << "New Game!\n";
    while (!game.winner()) {
        switch (game.phase()) {
        case Phase::awaiting_deal: {
            auto tiles = Domino::all();
            std::shuffle(tiles.begin(), tiles.end(), random);
            if (!game.deal(tiles)) {
                throw std::logic_error("engine rejected a complete deal");
            }
            break;
        }
        case Phase::awaiting_highest_double:
            print_board(game, show_bot_hand);
            std::cout << "\nFirst Turn, playing the highest double." << std::endl;
            std::this_thread::sleep_for(2s);
            announce(game.apply(PlayHighestDouble{}).value());
            break;
        case Phase::playing:
            print_board(game, show_bot_hand);
            if (game.active_seat() == Seat::first) {
                const auto result = play_human(game);
                if (!result) {
                    return 0;
                }
                announce(*result);
            } else {
                show_robot_thinking();
                announce(game.apply(choose_greedy_action(game, random).value()).value());
            }
            break;
        case Phase::match_complete:
            break;
        }
    }
    print_board(game, show_bot_hand);
    std::cout << name(*game.winner()) << " wins the match.\n";
    return 0;
}

}  // namespace

int main(int argc, char* argv[]) {
    bool show_bot_hand = false;
    for (int i = 1; i < argc; ++i) {
        const std::string_view argument = argv[i];
        if (argument == "--show-bot-hand") {
            show_bot_hand = true;
        } else if (argument == "--help" || argument == "-h") {
            std::cout << "Usage: dominoes [--show-bot-hand]\n";
            return 0;
        } else {
            std::cerr << "Unknown option: " << argument << '\n';
            return 1;
        }
    }
    return play_match(show_bot_hand);
}
