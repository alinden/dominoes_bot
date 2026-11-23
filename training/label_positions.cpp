#include "dominoes/hand_candidates.hpp"
#include "dominoes/greedy.hpp"
#include "dominoes/value_order.hpp"

#include <algorithm>
#include <array>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <map>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string_view>
#include <tuple>

using namespace dominoes;
using Clock = std::chrono::steady_clock;

namespace {

Seat opponent(Seat seat) {
    return seat == Seat::first ? Seat::second : Seat::first;
}

struct Position {
    Game game;
    std::vector<std::vector<Domino>> hands;
    int seed;
    int game_number;
    int turn;
    double gap;
    std::string_view category = "replay";
};

using PositionKey = std::tuple<int, int, int>;

std::set<PositionKey> timed_out_positions(const char* path) {
    std::ifstream log(path);
    if (!log) throw std::runtime_error("Could not open retry log");
    std::set<PositionKey> result;
    for (std::string line; std::getline(log, line);) {
        if (!line.ends_with(" | did not settle within budget")) continue;
        std::istringstream entry(line);
        int seed, game, turn;
        char first_slash, second_slash;
        if (!(entry >> seed >> first_slash >> game >> second_slash >> turn) ||
            first_slash != '/' || second_slash != '/') {
            throw std::runtime_error("Invalid timeout entry in retry log");
        }
        result.emplace(seed, game, turn);
    }
    return result;
}

std::map<int, std::vector<Position>> read_positions(bool dealt_only = false) {
    std::map<int, std::vector<Position>> positions;
    int pair, seed, bayes_seat, turns, deal_count;
    while (std::cin >> pair >> seed >> bayes_seat >> turns >> deal_count) {
        std::vector<std::array<Domino, Domino::tile_count>> deals(deal_count, Domino::all());
        for (auto& deal : deals) {
            for (auto& tile : deal) {
                int id;
                if (!(std::cin >> id)) throw std::runtime_error("Truncated deal");
                tile = Domino::all().at(id);
            }
        }
        Game game;
        std::array<std::optional<HandCandidates>, 2> belief;
        std::size_t deal_index = 0;
        for (int turn = 1; turn <= turns; ++turn) {
            if (game.phase() == Phase::awaiting_deal) {
                if (!game.deal(deals.at(deal_index++))) throw std::runtime_error("Invalid deal");
                if (!dealt_only) {
                    for (auto seat : {Seat::first, Seat::second}) {
                        belief[static_cast<int>(seat)].emplace(seat, game.player(seat).hand,
                                                               game.player(opponent(seat)).hand.size());
                    }
                }
            }
            char verb;
            int tile, actor, candidate_count;
            double gap;
            if (!(std::cin >> verb >> tile >> actor >> candidate_count >> gap)) {
                throw std::runtime_error("Truncated action");
            }
            Action action = PlayHighestDouble{};
            if (verb == 'W') action = Draw{};
            else if (verb == 'P') action = Pass{};
            else if (verb != 'H') {
                const auto direction = std::string_view("LRUD").find(verb);
                if (direction == std::string_view::npos) throw std::runtime_error("Unknown action");
                action = Move{Domino::all().at(tile), static_cast<Direction>(direction)};
            }

            if (verb != 'H' && game.active_seat() != static_cast<Seat>(actor - 1)) {
                throw std::runtime_error("Replay turn disagrees with the scoreboard");
            }
            if (candidate_count > 0 && dealt_only) {
                positions[seed].push_back({game, {}, seed, bayes_seat, turn, gap});
            } else if (candidate_count > 0) {
                auto hands = belief[actor - 1]->feasible_hands();
                auto actual = game.player(opponent(static_cast<Seat>(actor - 1))).hand;
                std::ranges::sort(actual, {}, &Domino::id);
                if (hands.size() != static_cast<std::size_t>(candidate_count) ||
                    std::ranges::find(hands, actual) == hands.end() || game.legal_actions().size() < 2) {
                    throw std::runtime_error("Replay disagrees with the decision log");
                }
                positions[seed].push_back({game, std::move(hands), seed, bayes_seat, turn, gap});
            }
            const auto result = game.apply(action);
            if (!result) throw std::runtime_error("Illegal replay action at turn " + std::to_string(turn));
            for (auto& model : belief) {
                if (model) model->observe(game, *result);
            }
        }
        if (!game.winner()) throw std::runtime_error("Replay did not finish");
    }
    return positions;
}

std::vector<SearchSample> worlds_for(const Game& game, Seat seat, const std::vector<Domino>& hand,
                                     std::size_t count, bool exhaustive, std::mt19937& random) {
    auto order = game.boneyard();
    for (auto tile : game.player(opponent(seat)).hand) order.push_back(tile);
    std::erase_if(order, [&](Domino tile) { return std::ranges::find(hand, tile) != hand.end(); });
    std::ranges::sort(order, {}, &Domino::id);
    std::vector<SearchSample> worlds;
    worlds.reserve(count);
    if (exhaustive) {
        do {
            worlds.push_back({hand, order, 1.0});
        } while (std::ranges::next_permutation(order, {}, &Domino::id).found);
    } else {
        std::set<std::vector<std::size_t>> seen;
        while (worlds.size() < count) {
            std::shuffle(order.begin(), order.end(), random);
            std::vector<std::size_t> ids;
            for (auto tile : order) ids.push_back(tile.id());
            if (seen.insert(std::move(ids)).second) worlds.push_back({hand, order, 1.0});
        }
    }
    return worlds;
}

void write_hand(std::ostream& out, const std::vector<Domino>& hand) {
    out << '[';
    for (std::size_t i = 0; i < hand.size(); ++i) out << (i ? "," : "") << hand[i].id();
    out << ']';
}

void write_state(std::ostream& out, const Game& game) {
    const auto seat = game.active_seat();
    const auto& board = game.board();
    out << "{\"active\":" << (seat ? static_cast<int>(*seat) : -1)
        << ",\"scores\":[" << game.player(Seat::first).score << ',' << game.player(Seat::second).score
        << "],\"firstHand\":";
    write_hand(out, game.player(Seat::first).hand);
    out << ",\"secondHand\":";
    write_hand(out, game.player(Seat::second).hand);
    out << ",\"boneyardSize\":" << game.boneyard().size()
        << ",\"spinner\":" << (board.spinner() ? static_cast<int>(board.spinner()->tile().id()) : -1)
        << ",\"chains\":[";
    for (int direction = 0; direction < 4; ++direction) {
        if (direction) out << ',';
        out << '[';
        bool first = true;
        for (const auto& tile : board.chain(static_cast<Direction>(direction))) {
            if (!first) out << ',';
            out << '[' << tile.exposed_pip() << ',' << tile.reversed().exposed_pip() << ']';
            first = false;
        }
        out << ']';
    }
    out << "]}";
}

bool label(const Position& position, std::size_t breadth, int seconds, std::mt19937& random,
           const SettlementSearchOptions& options = {}, std::vector<double>* benchmark_scores = nullptr,
           bool shared_queue = false) {
    const auto& game = position.game;
    const auto perspective = game.active_seat().value();
    const auto deadline = Clock::now() + std::chrono::seconds(seconds);
    std::size_t order_count = 1;
    for (std::size_t n = 2; n <= game.boneyard().size() && order_count <= breadth; ++n) order_count *= n;
    auto hands = position.hands;
    std::shuffle(hands.begin(), hands.end(), random);
    if (hands.size() > breadth) hands.resize(breadth);
    std::ostringstream rows;
    rows << std::setprecision(17);
    std::size_t total_worlds = 0;
    std::vector<std::vector<SearchSample>> hand_worlds;
    for (std::size_t i = 0; i < hands.size(); ++i) {
        const std::size_t count = std::min(order_count, breadth / hands.size() + (i < breadth % hands.size()));
        hand_worlds.push_back(worlds_for(game, perspective, hands[i], count, count == order_count, random));
        total_worlds += hand_worlds.back().size();
    }
    std::vector<TimedSearchResult> batch;
    if (shared_queue) {
        batch = search_hands_to_round_end(game, perspective, hand_worlds, deadline, 4, options);
        if (batch.empty()) return false;
    }
    for (std::size_t i = 0; i < hands.size(); ++i) {
        const auto& worlds = hand_worlds[i];
        const auto search = shared_queue ? std::move(batch[i])
                                         : search_to_round_end(game, perspective, worlds, deadline, 4, options);
        if (search.scores.empty() || (!search.reached_round_end && !options.leaf_value)) {
            return false;
        }
        if (benchmark_scores) {
            for (const auto& scored : search.scores) benchmark_scores->push_back(scored.score);
            continue;
        }
        const auto parent = game.with_hidden_tiles(perspective, hands[i], worlds.front().draw_order).value();
        for (const auto& [action, score] : search.scores) {
            const auto& move = std::get<Move>(action);
            auto after = parent;
            const auto event = after.apply(action).value();
            rows << "{\"seed\":" << position.seed << ",\"game\":" << position.game_number
                 << ",\"turn\":" << position.turn << ",\"perspective\":" << static_cast<int>(perspective)
                 << ",\"category\":\"" << position.category << "\",\"opponentHand\":";
            write_hand(rows, hands[i]);
            rows << ",\"tile\":" << move.tile.id() << ",\"direction\":" << static_cast<int>(move.direction)
                 << ",\"target\":" << score << ",\"settled\":" << (search.reached_round_end ? "true" : "false")
                 << ",\"depthBound\":" << search.depth;
            if (options.leaf_value) rows << ",\"leafTiles\":" << options.leaf_tiles;
            rows << ",\"drawOrders\":" << worlds.size() << ",\"allDrawOrders\":" << (worlds.size() == order_count ? "true" : "false")
                 << ",\"moveEndsRound\":" << (event.round ? "true" : "false") << ",\"before\":";
            write_state(rows, parent);
            rows << ",\"after\":";
            write_state(rows, after);
            rows << "}\n";
        }
    }
    if (benchmark_scores) return true;
    std::cout << rows.str() << std::flush;
    std::cerr << position.seed << '/' << position.game_number << '/' << position.turn << " | "
              << position.category << " | " << hands.size() << " hands, " << total_worlds << " worlds | "
              << std::chrono::duration<double>(Clock::now() - deadline + std::chrono::seconds(seconds)).count()
              << " s\n";
    return true;
}

enum class Category { lock, domino, jam, ordinary };
constexpr std::array<std::string_view, 4> categories{"lock", "near_domino", "jam", "ordinary"};

// Board identity deliberately ignores the hands, scores, and deal seed.
std::string board_key(const Board& board) {
    std::string key;
    key += static_cast<char>('A' + (board.spinner() ? board.spinner()->tile().id() : Domino::tile_count));
    for (int direction = 0; direction < 4; ++direction) {
        for (const auto& tile : board.chain(static_cast<Direction>(direction))) {
            key += static_cast<char>('A' + tile.exposed_pip() * 7 + tile.reversed().exposed_pip());
        }
        key += static_cast<char>('A' + 49);
    }
    return key;
}

Action rollout_action(const Game& game, const std::vector<Action>& legal, int policy, std::mt19937& random) {
    if (legal.size() == 1) return legal.front();
    if (std::bernoulli_distribution(policy == 0 ? .05 : .30)(random)) {
        return legal[std::uniform_int_distribution<std::size_t>(0, legal.size() - 1)(random)];
    }
    if (policy != 2) return choose_greedy_action(game, random).value();
    // A shedding policy supplies trajectories in which players unload high-pip tiles.
    return *std::ranges::max_element(legal, {}, [](const Action& action) {
        return std::get<Move>(action).tile.pip_total();
    });
}

bool can_jam(const Game& game, const std::vector<Action>& legal) {
    const auto jams = std::ranges::count_if(legal, [&](const Action& action) {
        auto next = game;
        const auto result = next.apply(action, DrawEvidence::omit).value();
        return !result.round && next.legal_moves().empty();
    });
    return jams > 0 && static_cast<std::size_t>(jams) < legal.size();
}

std::vector<std::vector<Domino>> select_hands(
    const Game& game, Seat seat, const HandCandidates* belief, std::size_t max_hands, std::mt19937& random) {
    auto dealt = game.player(opponent(seat)).hand;
    std::ranges::sort(dealt, {}, &Domino::id);
    if (!belief || game.boneyard().empty()) return {std::move(dealt)};

    return belief->sample_distinct_hands(dealt, max_hands, random);
}

using BenchmarkOrder = std::pair<std::string_view, SettlementSearchOptions>;

struct BandPolicy {
    std::string_view name;
    SettlementSearchOptions options;
    std::optional<SettlementSearchOptions> from_nine_placed{};
    bool shared_queue = false;

    const SettlementSearchOptions& for_board(std::size_t placed) const {
        return placed >= 9 && from_nine_placed ? *from_nine_placed : options;
    }
};

struct GenerationBand {
    std::vector<BandPolicy> policies;
    std::size_t early_order = 0;
    std::size_t late_order = 0;
    bool benchmark = false;
    std::size_t first_placed = 6;
    std::size_t last_placed = 12;
    const char* excluded_boards = nullptr;
    bool compare_leaf_values = false;
    bool distinct_decisions = false;
};

void benchmark_generated(const Position& position, int breadth, int seconds, std::mt19937& random,
                         const GenerationBand& band, std::mt19937& order_random) {
    auto sample_random = random;
    random.discard(1);
    std::vector<std::size_t> order;
    for (std::size_t i = 0; i < band.policies.size(); ++i) order.push_back(i);
    if (!band.compare_leaf_values) std::shuffle(order.begin(), order.end(), order_random);
    std::vector<double> reference;
    for (const auto i : order) {
        auto trial_random = sample_random;
        const auto& policy = band.policies[i];
        std::vector<double> scores;
        const auto start = Clock::now();
        const bool settled = label(position, breadth, seconds, trial_random,
                                   policy.for_board(position.game.board().size()), &scores, policy.shared_queue);
        const auto elapsed = std::chrono::duration<double, std::milli>(Clock::now() - start).count();
        if (settled && !band.compare_leaf_values) {
            if (reference.empty()) reference = scores;
            if (!std::ranges::equal(scores, reference, [](double a, double b) { return std::abs(a - b) <= 1e-12; })) {
                throw std::runtime_error("Ordering changed generated settlement labels");
            }
        }
        std::cout << position.seed << ',' << position.turn << ',' << position.game.board().size() << ','
                  << position.game.boneyard().size() << ',' << position.hands.size() << ','
                  << policy.name << ',' << settled << ',' << elapsed;
        if (band.compare_leaf_values) {
            std::cout << ',' << position.game.legal_actions().size() << ',' << std::setprecision(17);
            for (std::size_t j = 0; j < scores.size(); ++j) {
                if (j) std::cout << ';';
                std::cout << scores[j];
            }
        }
        std::cout << '\n' << std::flush;
    }
}

void generate_positions(int limit, int draws_per_hand, int seconds, int first_seed, int max_hands,
                        const GenerationBand* band = nullptr) {
    const auto first_early = band ? band->first_placed : 13;
    const auto last_early = band ? band->last_placed : Domino::tile_count;
    std::vector<int> quotas{limit / 10, limit / 5, limit / 4, limit - limit / 10 - limit / 5 - limit / 4};
    if (band) {
        const int span = static_cast<int>(last_early - first_early + 1);
        quotas.assign(span, limit / span);
        for (int i = 0; i < limit % span; ++i) ++quotas[i];
    }
    std::vector<int> counts(quotas.size());
    std::set<std::string> seen;
    if (band && band->excluded_boards) {
        std::ifstream file(band->excluded_boards);
        if (!file) throw std::runtime_error("Could not open board exclusions");
        for (std::string key; std::getline(file, key);) {
            if (!key.empty()) seen.insert(std::move(key));
        }
    }
    std::mt19937 label_random(first_seed);
    std::mt19937 order_random(first_seed ^ 613);
    if (band && band->benchmark) {
        std::cout << "seed,turn,board_tiles,boneyard,hands,order,"
                  << (band->compare_leaf_values ? "completed,milliseconds,moves,scores" : "settled,milliseconds")
                  << '\n';
    }
    int accepted = 0;
    int timeouts = 0;
    for (int seed = first_seed; seed - first_seed < 20000 && accepted < limit; ++seed) {
        Game game;
        std::mt19937 random(seed);
        std::vector<std::pair<Position, Category>> round;
        std::array<std::optional<HandCandidates>, 2> beliefs;
        int from_seed = 0;
        for (int turn = 1; !game.winner() && accepted < limit && from_seed < 8; ++turn) {
            if (game.phase() == Phase::awaiting_deal) {
                auto tiles = Domino::all();
                std::shuffle(tiles.begin(), tiles.end(), random);
                if (!game.deal(tiles)) throw std::logic_error("Rejected generated deal");
                if (max_hands > 1) {
                    for (auto seat : {Seat::first, Seat::second}) {
                        beliefs[static_cast<int>(seat)].emplace(
                            seat, game.player(seat).hand, game.player(opponent(seat)).hand.size());
                    }
                }
            }
            const auto legal = game.legal_actions();
            const auto seat = game.active_seat();
            const auto placed = game.board().size();
            const bool eligible = band ? placed >= first_early && placed <= last_early
                                       : game.boneyard().size() <= 6 && placed >= 13;
            if (seat && legal.size() > 1 && eligible &&
                (!band || counts[placed - first_early] < quotas[placed - first_early])) {
                const auto size = game.player(*seat).hand.size();
                const auto category = size <= 2 ? Category::domino
                    : can_jam(game, legal) ? Category::jam : Category::ordinary;
                const auto& belief = beliefs[static_cast<int>(*seat)];
                auto hands = select_hands(game, *seat, belief ? &*belief : nullptr, max_hands, label_random);
                if (!band || !band->distinct_decisions || hands.size() == static_cast<std::size_t>(max_hands)) {
                    round.emplace_back(Position{game, std::move(hands), seed, 1, turn, 0}, category);
                }
            }
            const auto action = rollout_action(game, legal, (seed + static_cast<int>(seat.value_or(Seat::first))) % 3,
                                               random);
            const auto result = game.apply(action, max_hands > 1 ? DrawEvidence::collect : DrawEvidence::omit).value();
            for (auto& belief : beliefs) {
                if (belief) belief->observe(game, result);
            }
            if (!result.round) continue;
            std::shuffle(round.begin(), round.end(), random);
            for (auto& [position, category] : round) {
                if (result.round->reason == RoundEnd::locked && position.game.boneyard().empty()) {
                    category = Category::lock;
                }
                const auto placed = position.game.board().size();
                const auto index = band ? placed - first_early : static_cast<std::size_t>(category);
                if (counts[index] == quotas[index]) continue;
                auto key = board_key(position.game.board());
                if (band && band->distinct_decisions) {
                    const auto mover = position.game.active_seat().value();
                    const auto& player = position.game.player(mover);
                    TileMask hand = 0;
                    for (auto tile : player.hand) hand |= TileMask{1} << tile.id();
                    key += '/' + std::to_string(hand) + '/' + std::to_string(player.score)
                         + '/' + std::to_string(position.game.player(opponent(mover)).score)
                         + '/' + std::to_string(position.game.boneyard().size());
                }
                if (seen.contains(key)) continue;
                position.category = categories[static_cast<std::size_t>(category)];
                const auto breadth = draws_per_hand * position.hands.size();
                const auto* policy = band ? &band->policies[placed <= 8 ? band->early_order : band->late_order] : nullptr;
                if (band && band->benchmark) {
                    benchmark_generated(position, breadth, seconds, label_random, *band, order_random);
                } else if (!label(position, breadth, seconds, label_random,
                                  policy ? policy->for_board(placed) : SettlementSearchOptions{},
                                  nullptr, policy && policy->shared_queue)) {
                    ++timeouts;
                    std::cerr << position.seed << '/' << position.game_number << '/' << position.turn
                              << " | " << placed << " tiles placed | did not settle within budget\n";
                    continue;
                }
                seen.insert(std::move(key));
                ++counts[index];
                ++accepted;
                if (++from_seed == 8 || accepted == limit) break;
            }
            round.clear();
        }
    }
    if (band && band->benchmark) {
        std::cerr << "Benchmarked " << accepted << " of " << limit << " distinct boards\n";
    } else {
        std::cerr << "Labeled " << accepted << " of " << limit
                  << (band && band->distinct_decisions ? " distinct decisions; " : " distinct boards; ")
                  << timeouts << " timeouts\n";
    }
    for (std::size_t i = 0; i < counts.size(); ++i) {
        if (band) std::cerr << (i + first_early) << " tiles placed";
        else std::cerr << categories[i];
        std::cerr << ": " << counts[i] << '\n';
    }
    if (accepted != limit) throw std::runtime_error("Could not meet the diversity quotas");
}

void benchmark_ordering(int per_group, std::size_t breadth, int seconds, char** checkpoints,
                        std::string_view mode, std::size_t first_placement, std::size_t minimum_boneyard) {
    const bool early = mode != "benchmark";
    const bool include_medium = mode == "benchmark-early-medium";
    const bool compare_hybrids = mode == "benchmark-early-wide" || include_medium;
    const ValueOrder linear(checkpoints[0]);
    const ValueOrder small(checkpoints[1]);
    const ValueOrder wide(checkpoints[2]);
    std::optional<ValueOrder> medium;
    if (include_medium) medium.emplace(checkpoints[3]);
    using BenchmarkOrder = std::pair<std::string_view, SettlementSearchOptions>;
    const BenchmarkOrder pruned{"pruned", {{}, true, true}};
    const BenchmarkOrder small_order{"1x32", {std::cref(small), true, true}};
    const auto suffix = "-" + std::to_string(minimum_boneyard) + "plus";
    const auto small_hybrid = "1x32" + suffix;
    const auto wide_hybrid = "2x128" + suffix;
    const auto medium_hybrid = "2x64" + suffix;
    std::vector<BenchmarkOrder> orders = early
        ? std::vector<BenchmarkOrder>{
              pruned, {small_hybrid, {std::cref(small), true, true, minimum_boneyard}}, small_order}
        : std::vector<BenchmarkOrder>{
              {"current", {{}, false, true}}, pruned,
              {"score", {[](const Game& game, Seat seat) { return evaluate_position(game, seat); }, true, true}},
              {"linear", {std::cref(linear), true, true}}, small_order,
              {"2x128", {std::cref(wide), true, true}}};
    const auto original_order_count = orders.size();
    if (compare_hybrids) orders.push_back({wide_hybrid, {std::cref(wide), true, true, minimum_boneyard}});
    if (medium) orders.push_back({medium_hybrid, {std::cref(*medium), true, true, minimum_boneyard}});
    auto by_seed = read_positions(true);
    std::array<std::vector<const Position*>, 4> buckets;
    for (const auto& [seed, positions] : by_seed) {
        for (const auto& position : positions) {
            const auto draws = position.game.boneyard().size();
            const auto placed = position.game.board().size();
            if (early && (draws < minimum_boneyard ||
                          (placed != first_placement - 1 && placed != first_placement))) continue;
            const auto bucket = early ? placed - (first_placement - 1)
                                      : draws == 0 ? 0 : draws <= 3 ? 1 : draws <= 6 ? 2 : 3;
            buckets[bucket].push_back(&position);
        }
    }
    std::mt19937 random(651);
    std::mt19937 medium_order_random(640);
    std::cout << "seed,game,turn,bucket,boneyard,board_tiles,worlds,order,settled,milliseconds,nodes,draw_cutoffs\n";
    for (std::size_t bucket = 0; bucket < buckets.size(); ++bucket) {
        auto& positions = buckets[bucket];
        std::shuffle(positions.begin(), positions.end(), random);
        if (!early && bucket == 0) {
            // Give deterministic endgames with some search left room in the benchmark.
            std::ranges::stable_sort(positions, {}, [](const Position* position) {
                return position->game.board().size() > 21;
            });
        } else if (!early && bucket == 3) {
            std::ranges::stable_sort(positions, {}, [](const Position* position) {
                return position->game.board().size();
            });
        }
        if (positions.size() > static_cast<std::size_t>(per_group)) positions.resize(per_group);
        for (const auto* position : positions) {
            const auto& game = position->game;
            const auto mover = game.active_seat().value();
            const auto& hand = game.player(opponent(mover)).hand;
            std::size_t permutations = 1;
            for (std::size_t n = 2; n <= game.boneyard().size() && permutations <= breadth; ++n) {
                permutations *= n;
            }
            const auto count = std::min<std::size_t>(breadth, permutations);
            auto worlds = worlds_for(game, mover, hand, count, count == permutations, random);
            std::vector<ScoredAction> reference;
            std::vector<std::size_t> run_order;
            for (std::size_t index = 0; index < original_order_count; ++index) run_order.push_back(index);
            std::shuffle(run_order.begin(), run_order.end(), random);
            // Keep draw and position sampling identical to benchmark-early.
            if (compare_hybrids) {
                run_order = run_order.front() == 1
                    ? std::vector<std::size_t>{1, 3} : std::vector<std::size_t>{3, 1};
                if (include_medium) {
                    const auto slot = std::uniform_int_distribution<int>(0, 2)(medium_order_random);
                    run_order.insert(run_order.begin() + slot, 4);
                }
            }
            for (auto index : run_order) {
                const auto& [name, options] = orders[index];
                const auto start = Clock::now();
                const auto result = search_to_round_end(game, mover, worlds, start + std::chrono::seconds(seconds),
                                                        1, options);
                const auto ms = std::chrono::duration<double, std::milli>(Clock::now() - start).count();
                if (result.reached_round_end) {
                    if (reference.empty()) reference = result.scores;
                    for (std::size_t i = 0; i < reference.size(); ++i) {
                        const auto& expected = reference[i];
                        const auto& actual = result.scores.at(i);
                        if (std::get<Move>(actual.action) != std::get<Move>(expected.action) ||
                            std::abs(actual.score - expected.score) > 1e-12) {
                            throw std::runtime_error("Move order changed a settled value");
                        }
                    }
                }
                std::cout << position->seed << ',' << position->game_number << ',' << position->turn << ','
                          << bucket << ',' << game.boneyard().size() << ',' << game.board().size() << ','
                          << worlds.size() << ',' << name << ',' << result.reached_round_end << ',' << ms << ','
                          << result.work.nodes << ',' << result.work.draw_cutoffs << '\n'
                          << std::flush;
            }
        }
    }
}

}  // namespace

int main(int argc, char** argv) {
    try {
        const std::string_view mode = argc > 1 ? argv[1] : "";
        if ((mode == "benchmark-leaves" && argc == 8) ||
            (mode == "generate-leaves" && (argc == 8 || argc == 10))) {
            const int count = std::stoi(argv[2]);
            const int draws = std::stoi(argv[3]);
            const int seconds = std::stoi(argv[4]);
            const int seed = std::stoi(argv[5]);
            const int hands = std::stoi(argv[6]);
            if (count < 1 || draws < 1 || seconds < 1 || seed < 0 || seed > 2000000000 || hands < 1) {
                throw std::runtime_error("Invalid leaf benchmark arguments");
            }
            const ValueOrder model(argv[7]);
            const SettlementSearchOptions exact{std::cref(model), true, false, 8};
            GenerationBand band;
            if (mode == "generate-leaves") {
                auto approximate = exact;
                approximate.leaf_tiles = 9;
                approximate.leaf_value = std::cref(model);
                band.policies.push_back({"leaf-9", approximate, {}, true});
                const int first = argc == 10 ? std::stoi(argv[8]) : 0;
                const int last = argc == 10 ? std::stoi(argv[9]) : 3;
                if (first < 0 || first > last || last >= static_cast<int>(approximate.leaf_tiles)) {
                    throw std::runtime_error("Board size range must precede the leaf cutoff");
                }
                band.first_placed = first;
                band.last_placed = last;
                band.distinct_decisions = true;
                generate_positions(count, draws, seconds, seed, hands, &band);
                return 0;
            }
            constexpr std::array<std::pair<std::size_t, std::string_view>, 3> cutoffs{{
                {6, "leaf-6"}, {9, "leaf-9"}, {12, "leaf-12"},
            }};
            for (const auto& [tiles, name] : cutoffs) {
                auto approximate = exact;
                approximate.leaf_tiles = tiles;
                approximate.leaf_value = std::cref(model);
                band.policies.push_back({name, approximate, {}, true});
            }
            band.policies.push_back({"exact", exact, {}, true});
            band.first_placed = 3;
            band.last_placed = 3;
            band.benchmark = true;
            band.compare_leaf_values = true;
            generate_positions(count, draws, seconds, seed, hands, &band);
            return 0;
        }
        if ((mode == "benchmark-band" && (argc == 10 || argc == 12 || argc == 13)) ||
            (mode == "benchmark-phases" && argc == 10) ||
            (mode == "benchmark-opening" && argc == 10) ||
            (mode == "benchmark-queue" && argc == 10) ||
            (mode == "generate-phases" && argc == 11) ||
            (mode == "generate-band" && (argc == 12 || argc == 14 || argc == 15))) {
            const int count = std::stoi(argv[2]);
            const int draws = std::stoi(argv[3]);
            const int seconds = std::stoi(argv[4]);
            const int seed = std::stoi(argv[5]);
            const int hands = std::stoi(argv[6]);
            if (count < 1 || draws < 1 || seconds < 1 || seed < 0 || seed > 2000000000 || hands < 1) {
                throw std::runtime_error("Invalid band generation arguments");
            }
            const ValueOrder first(argv[7]);
            const ValueOrder medium(argv[8]);
            const ValueOrder wide(argv[9]);
            if (mode == "benchmark-opening") {
                const OrderingPhase opening{std::cref(wide), 0, 5, 0, 14};
                const OrderingPhase midgame{std::cref(first), 6, 12, 8, 14};
                const OrderingPhase endgame{std::cref(medium), 13, 27, 4, 6};
                const std::array previous{midgame, endgame};
                const std::array with_opening{opening, midgame, endgame};
                const GenerationBand band{{
                    {"two-phase", {{}, true, false, 0, previous}, {}, true},
                    {"three-phase", {{}, true, false, 0, with_opening}, {}, true},
                }, 0, 0, true, 3, 3};
                generate_positions(count, draws, seconds, seed, hands, &band);
                return 0;
            }
            if (mode == "benchmark-phases" || mode == "benchmark-queue" || mode == "generate-phases") {
                const OrderingPhase early{std::cref(first), 6, 12, 8, 14};
                const std::array specialist{early};
                const std::array late_4_to_6{early, OrderingPhase{std::cref(medium), 13, 27, 4, 6}};
                const std::array late_8_plus{early, OrderingPhase{std::cref(medium), 13, 27, 8, 14}};
                const SettlementSearchOptions phase_order{{}, true, false, 0, late_4_to_6};
                GenerationBand band{{
                    {"production", {std::cref(wide), true, false, 8},
                                   SettlementSearchOptions{std::cref(medium), true, false, 8}},
                    {"specialist", {{}, true, false, 0, specialist}},
                    {"late-4-6", phase_order},
                    {"late-8-plus", {{}, true, false, 0, late_8_plus}},
                }, 0, 0, true};
                if (mode == "benchmark-queue") {
                    band.policies = {{"sequential-hands", phase_order},
                                     {"shared-queue", phase_order, {}, true}};
                } else if (mode == "generate-phases") {
                    band.policies = {{"shared-queue", phase_order, {}, true}};
                    band.benchmark = false;
                    band.excluded_boards = argv[10];
                }
                generate_positions(count, draws, seconds, seed, hands, &band);
                return 0;
            }
            GenerationBand band{{
                {"original", {}},
                {"1x32", {std::cref(first), true, false, 8}},
                {"2x64", {std::cref(medium), true, false, 8}},
                {"2x128", {std::cref(wide), true, false, 8}},
            }};
            band.benchmark = mode == "benchmark-band";
            if ((band.benchmark && argc >= 12) || (!band.benchmark && argc >= 14)) {
                const int range_arg = band.benchmark ? 10 : 12;
                const int first = std::stoi(argv[range_arg]);
                const int last = std::stoi(argv[range_arg + 1]);
                if (first < 0 || first > last || last >= static_cast<int>(Domino::tile_count)) {
                    throw std::runtime_error("Invalid board size range");
                }
                band.first_placed = first;
                band.last_placed = last;
                if (band.benchmark && argc == 13) {
                    const std::string_view name = argv[12];
                    std::erase_if(band.policies, [&](const auto& policy) { return policy.name != name; });
                    if (band.policies.empty()) throw std::runtime_error("Unknown band ordering model");
                }
            }
            if (!band.benchmark) {
                const auto find_policy = [&](std::string_view name) {
                    const auto policy = std::ranges::find(band.policies, name, &BandPolicy::name);
                    if (policy == band.policies.end()) throw std::runtime_error("Unknown band ordering model");
                    return static_cast<std::size_t>(policy - band.policies.begin());
                };
                band.early_order = find_policy(argv[10]);
                band.late_order = find_policy(argv[11]);
                if (argc == 15) {
                    if (std::string_view(argv[14]) != "--shared-queue") {
                        throw std::runtime_error("Unknown band generation option");
                    }
                    band.policies[band.early_order].shared_queue = true;
                    band.policies[band.late_order].shared_queue = true;
                }
            }
            generate_positions(count, draws, seconds, seed, hands, &band);
            return 0;
        }
        if ((argc == 8 && mode == "benchmark") ||
            ((argc == 9 || argc == 11) && mode == "benchmark-early-medium") ||
            ((argc == 8 || argc == 10) && (mode == "benchmark-early" || mode == "benchmark-early-wide"))) {
            const int positions = std::stoi(argv[2]);
            const int breadth = std::stoi(argv[3]);
            const int seconds = std::stoi(argv[4]);
            const int first_optional = mode == "benchmark-early-medium" ? 9 : 8;
            const int placement = argc > first_optional ? std::stoi(argv[first_optional]) : 3;
            const int minimum_boneyard = argc > first_optional ? std::stoi(argv[first_optional + 1]) : 7;
            if (positions < 1 || breadth < 1 || seconds < 1 || placement < 1 || placement >= 28 ||
                minimum_boneyard < 0 || minimum_boneyard > 14) {
                throw std::runtime_error("Invalid benchmark arguments");
            }
            benchmark_ordering(positions, breadth, seconds, argv + 5, mode, placement, minimum_boneyard);
            return 0;
        }
        if ((argc == 6 || argc == 7) && std::string_view(argv[1]) == "generate") {
            const int count = std::stoi(argv[2]);
            const int breadth = std::stoi(argv[3]);
            const int seconds = std::stoi(argv[4]);
            const int first_seed = std::stoi(argv[5]);
            const int max_hands = argc == 7 ? std::stoi(argv[6]) : 1;
            if (count < 1 || breadth < 1 || seconds < 1 || first_seed < 0 || first_seed > 2000000000 ||
                max_hands < 1) {
                throw std::runtime_error("Invalid generation arguments");
            }
            generate_positions(count, breadth, seconds, first_seed, max_hands);
            return 0;
        }
        if (argc != 4 && argc != 5) {
            throw std::runtime_error("Usage: label_positions POSITIONS|all BREADTH SECONDS_PER_POSITION [RETRY_LOG]");
        }
        const bool all = std::string_view(argv[1]) == "all";
        const int limit = all ? 0 : std::stoi(argv[1]);
        const int breadth = std::stoi(argv[2]);
        const int seconds = std::stoi(argv[3]);
        if ((!all && limit < 1) || breadth < 1 || seconds < 1) throw std::runtime_error("Arguments must be positive");
        auto by_seed = read_positions();
        if (argc == 5) {
            const auto retry = timed_out_positions(argv[4]);
            for (auto& [seed, positions] : by_seed) {
                std::erase_if(positions, [&](const Position& position) {
                    return !retry.contains({seed, position.game_number, position.turn});
                });
            }
        }
        std::mt19937 random(613);
        std::size_t eligible = 0;
        for (auto& [seed, positions] : by_seed) {
            eligible += positions.size();
            std::shuffle(positions.begin(), positions.end(), random);
            std::ranges::stable_sort(positions, {}, [](const Position& position) {
                return std::pair{position.game.boneyard().size() > 5, position.gap > .05};
            });
        }
        std::cerr << eligible << " eligible positions across " << by_seed.size() << " seeds\n";
        int accepted = 0;
        for (std::size_t index = 0; all || accepted < limit; ++index) {
            bool attempted = false;
            for (const auto& [seed, positions] : by_seed) {
                if (index >= positions.size()) continue;
                attempted = true;
                const auto& position = positions[index];
                if (label(position, breadth, seconds, random)) {
                    if (++accepted == limit) return 0;
                } else {
                    std::cerr << seed << '/' << position.game_number << '/' << position.turn
                              << " | did not settle within budget\n";
                }
            }
            if (!attempted) break;
        }
        if (all) {
            std::cerr << "Labeled " << accepted << " of " << eligible << " positions\n";
            return 0;
        }
        throw std::runtime_error("Only labeled " + std::to_string(accepted) + " positions");
    } catch (const std::exception& error) {
        std::cerr << error.what() << '\n';
        return 1;
    }
}
