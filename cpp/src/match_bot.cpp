#include "dominoes/decision_log.hpp"
#include "dominoes/greedy.hpp"
#include "dominoes/hand_candidates.hpp"
#include "dominoes/neural_search.hpp"

#include <algorithm>
#include <fstream>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>

namespace {

using namespace dominoes;
constexpr std::string_view directions = "LRUD";

Action read_action(std::istream& command) {
    char code;
    if (!(command >> code)) throw std::runtime_error("missing action");
    if (code == 'H') return PlayHighestDouble{};
    if (code == 'W') return Draw{};
    if (code == 'P') return Pass{};
    std::size_t id;
    const auto direction = directions.find(code);
    if (direction == std::string_view::npos || !(command >> id)) {
        throw std::runtime_error("invalid move");
    }
    return Move{Domino::all().at(id), static_cast<Direction>(direction)};
}

void write_action(const Action& action) {
    if (const auto* move = std::get_if<Move>(&action)) {
        std::cout << directions.at(static_cast<std::size_t>(move->direction)) << ' ' << move->tile.id();
    } else {
        std::cout << (std::holds_alternative<Draw>(action) ? 'W' : 'P');
    }
    std::cout << std::endl;
}

void run(Seat bot, int depth, int search_seconds, std::size_t breadth,
         std::size_t workers, int inference_seconds, unsigned long seed, std::ostream& log,
         const std::optional<NeuralSearch>& neural) {
    std::mt19937 random(seed);
    std::mt19937 inference_random(seed ^ 0x9e3779b9UL);
    Game game;
    std::optional<HandCandidates> candidates;
    const Seat opponent = bot == Seat::first ? Seat::second : Seat::first;
    const auto phases = neural ? neural->phases() : std::array<OrderingPhase, 3>{};
    const auto options = neural ? neural->options(phases) : SettlementSearchOptions{{}, false};
    std::string line;
    while (std::getline(std::cin, line)) {
        std::istringstream command(line);
        std::string kind;
        command >> kind;
        if (kind == "DEAL") {
            std::vector<Domino> tiles;
            std::size_t id;
            while (command >> id) tiles.push_back(Domino::all().at(id));
            if (!game.deal(tiles)) throw std::runtime_error("invalid deal");
            candidates.emplace(bot, game.player(bot).hand, game.player(opponent).hand.size(),
                               inference_seconds > 0);
        } else if (kind == "ACT") {
            const auto action = read_action(command);
            std::optional<Game> before;
            if (inference_seconds > 0 && game.active_seat() == opponent && std::holds_alternative<Move>(action)) {
                before = game;
            }
            const auto result = game.apply(action);
            if (!result || !candidates) throw std::runtime_error("illegal action in match replay");
            if (before) {
                const auto deadline = std::chrono::steady_clock::now() + std::chrono::seconds(inference_seconds);
                const auto evidence = candidates->observe_with_inference(
                    *before, game, *result, inference_random, deadline, workers);
                log_inference(log, evidence);
            } else {
                candidates->observe(game, *result);
            }
        } else if (kind == "CHOOSE") {
            if (game.active_seat() != bot || !candidates) throw std::runtime_error("bot is out of turn");
            int turn;
            if (!(command >> turn)) throw std::runtime_error("missing turn number");
            log << "TURN " << turn << '\n' << std::flush;
            candidates->log_estimates(log, game.player(opponent).hand);
            const auto legal = game.legal_actions();
            TimedSearchResult search;
            DecisionSamples samples;
            if (legal.size() > 1) {
                const auto start = std::chrono::steady_clock::now();
                const auto deadline = start + std::chrono::seconds(search_seconds);
                const int max_depth = neural ? NeuralSearch::max_depth(game) : depth;
                samples = candidates->sample_for_search(game, breadth, random);
                if (!samples.worlds.empty() && search_seconds > 0) {
                    search = search_until(game, bot, max_depth, samples.worlds, deadline, workers, options);
                    log << "Depth ceiling: " << max_depth << ", completed: " << search.depth << '\n';
                    if (search.timed_out) log << "Search deadline reached.\n";
                    if (search.reached_round_end) log << "All sampled branches reached round settlement.\n";
                } else if (!samples.worlds.empty()) {
                    search.scores = score_sampled_actions(game, bot, max_depth, samples.worlds, workers, options);
                    search.depth = max_depth;
                }
                log << "Move search seconds: "
                    << std::chrono::duration<double>(std::chrono::steady_clock::now() - start).count() << '\n';
            }
            auto& scores = search.scores;
            const auto chosen = legal.size() == 1 ? legal.front()
                : scores.empty() ? choose_greedy_action(game, random).value()
                                 : std::ranges::max_element(scores, {}, &ScoredAction::score)->action;
            if (legal.size() == 1) log << "Forced action.\n";
            else if (scores.empty()) log << "No search pass completed. Using greedy play.\n";
            else log_search_scores(log, search.depth, samples, scores);
            log << "FINAL DECISION: ";
            log_action(log, chosen);
            log << "\n\n" << std::flush;
            write_action(chosen);
            continue;
        } else {
            throw std::runtime_error("unknown match command: " + line);
        }
        std::cout << "OK " << game.player(Seat::first).score << ' ' << game.player(Seat::second).score
                  << std::endl;
    }
}

}  // namespace

int main(int argc, char* argv[]) {
    try {
        if (argc < 5 || (argc > 9 && argc != 12)) {
            throw std::runtime_error("Usage: dominoes_match_bot SEAT DEPTH SEED LOG [SEARCH_SECONDS [BREADTH [WORKERS [INFERENCE_SECONDS [OPENING MIDGAME LATE]]]]]");
        }
        const int seat = std::stoi(argv[1]);
        const int depth = std::stoi(argv[2]);
        const int search_seconds = argc >= 6 ? std::stoi(argv[5]) : 0;
        const int breadth = argc >= 7 ? std::stoi(argv[6]) : HandCandidates::search_samples;
        const int workers = argc >= 8 ? std::stoi(argv[7]) : 1;
        const int inference_seconds = argc >= 9 ? std::stoi(argv[8]) : 0;
        if (seat < 0 || seat > 1 || depth < 1 || search_seconds < 0 || breadth < 1 ||
            workers < 1 || inference_seconds < 0) {
            throw std::runtime_error("invalid seat, depth, timing budget, breadth, or worker count");
        }
        const auto seed = std::stoul(argv[3]);
        std::optional<NeuralSearch> neural;
        if (argc == 12) {
            neural.emplace(NeuralSearch{ValueOrder(argv[9]), ValueOrder(argv[10]), ValueOrder(argv[11])});
        }
        std::ofstream log(argv[4], std::ios::app);
        if (!log) throw std::runtime_error("could not open decision log");
        log << "GAME | seed " << argv[3] << " | C++ seat " << seat + 1 << '\n' << std::flush;
        run(static_cast<Seat>(seat), depth, search_seconds, static_cast<std::size_t>(breadth),
            static_cast<std::size_t>(workers), inference_seconds, seed, log, neural);
    } catch (const std::exception& error) {
        std::cerr << "C++ match bot: " << error.what() << '\n';
        return 1;
    }
}
