#include "dominoes/game.hpp"

#include <algorithm>
#include <bitset>
#include <numeric>
#include <utility>

namespace dominoes {
namespace {

constexpr std::array directions{Direction::left, Direction::right, Direction::up, Direction::down};

Seat other(Seat seat) {
    return seat == Seat::first ? Seat::second : Seat::first;
}

int pip_total(const Player& player) {
    return std::accumulate(player.hand.begin(), player.hand.end(), 0,
                           [](int total, Domino tile) { return total + tile.pip_total(); });
}

std::array<bool, directions.size()> canonical_directions(const Board& board) {
    std::array<bool, directions.size()> keep{};
    bool seen_empty_arm = false;
    std::bitset<Domino::max_pip + 1> seen_pips;
    const auto shape = board.shape();
    for (auto direction : directions) {
        const auto& arm = board.chain(direction);
        bool canonical = true;
        if (shape == BoardShape::snake) {
            const auto& snake = board.chain(Direction::left);
            canonical = direction == Direction::left ||
                        snake.front().exposed_pip() != snake.back().reversed().exposed_pip();
        } else if (arm.empty()) {
            canonical = !std::exchange(seen_empty_arm, true);
        } else if (shape == BoardShape::cross) {
            const auto& end = arm.front();
            if (!end.tile().is_double() || end.score() == 0) {
                const auto pip = static_cast<std::size_t>(end.exposed_pip());
                canonical = !seen_pips.test(pip);
                seen_pips.set(pip);
            }
        }
        keep[static_cast<std::size_t>(direction)] = canonical;
    }
    return keep;
}

}  // namespace

std::optional<Seat> Game::active_seat() const {
    return phase_ == Phase::playing ? next_seat_ : std::nullopt;
}

std::optional<Seat> Game::winner() const {
    if (phase_ != Phase::match_complete) {
        return std::nullopt;
    }
    return players_[0].score > players_[1].score ? Seat::first : Seat::second;
}

bool Game::deal(std::span<const Domino> tiles) {
    if (phase_ != Phase::awaiting_deal || tiles.size() != Domino::tile_count) {
        return false;
    }
    std::bitset<Domino::tile_count> seen;
    for (auto tile : tiles) {
        if (seen.test(tile.id())) {
            return false;
        }
        seen.set(tile.id());
    }
    constexpr int hand_size = 7;
    players_[0].hand.assign(tiles.begin(), tiles.begin() + hand_size);
    players_[1].hand.assign(tiles.begin() + hand_size, tiles.begin() + 2 * hand_size);
    boneyard_.assign(tiles.begin() + 2 * hand_size, tiles.end());
    board_ = Board{};
    previous_pass_ = false;
    phase_ = next_seat_ ? Phase::playing : Phase::awaiting_highest_double;
    return true;
}

std::optional<Game> Game::with_hidden_tiles(
    Seat observer, std::span<const Domino> opponent_hand,
    std::span<const Domino> draw_order) const {
    const Seat opponent = other(observer);
    if (phase_ != Phase::playing || opponent_hand.size() != player(opponent).hand.size() ||
        draw_order.size() != boneyard_.size()) {
        return std::nullopt;
    }

    std::bitset<Domino::tile_count> used;
    for (auto tile : player(observer).hand) {
        used.set(tile.id());
    }
    for (auto tile : Domino::all()) {
        if (board_.contains(tile)) {
            used.set(tile.id());
        }
    }
    for (auto tiles : {opponent_hand, draw_order}) {
        for (auto tile : tiles) {
            if (used.test(tile.id())) {
                return std::nullopt;
            }
            used.set(tile.id());
        }
    }
    if (!used.all()) {
        return std::nullopt;
    }

    Game copy = *this;
    copy.players_[static_cast<std::size_t>(opponent)].hand.assign(opponent_hand.begin(),
                                                                 opponent_hand.end());
    copy.boneyard_.assign(draw_order.begin(), draw_order.end());
    return copy;
}

std::vector<Move> Game::legal_moves() const {
    std::vector<Move> moves;
    if (const auto seat = active_seat()) {
        for (auto direction : directions) {
            for (auto tile : player(*seat).hand) {
                if (board_.place(tile, direction)) {
                    moves.push_back({tile, direction});
                }
            }
        }
    }
    return moves;
}

std::vector<Action> Game::legal_actions() const {
    if (phase_ == Phase::awaiting_highest_double) {
        return {PlayHighestDouble{}};
    }
    if (phase_ != Phase::playing) {
        return {};
    }
    const auto moves = legal_moves();
    if (moves.empty()) {
        return {boneyard_.empty() ? Action{Pass{}} : Action{Draw{}}};
    }
    const auto keep = canonical_directions(board_);
    std::vector<Action> actions;
    for (const auto& move : moves) {
        if (keep[static_cast<std::size_t>(move.direction)]) {
            actions.emplace_back(move);
        }
    }
    return actions;
}

std::optional<TurnResult> Game::apply(const Action& action) {
    if (std::holds_alternative<PlayHighestDouble>(action)) {
        return play_highest_double();
    }
    const auto seat = active_seat();
    if (!seat) {
        return std::nullopt;
    }
    auto& hand = players_[static_cast<std::size_t>(*seat)].hand;
    if (const auto* move = std::get_if<Move>(&action)) {
        if (std::ranges::find(hand, move->tile) == hand.end()) {
            return std::nullopt;
        }
        if (auto next = board_.place(move->tile, move->direction)) {
            return play_move(*seat, *move, std::move(*next));
        }
        return std::nullopt;
    }
    if (!legal_moves().empty()) {
        return std::nullopt;
    }

    TurnResult result{};
    result.action = action;
    result.actor = seat;
    if (std::holds_alternative<Draw>(action)) {
        if (boneyard_.empty()) {
            return std::nullopt;
        }
        result.pre_draw_hand_size = hand.size();
        result.pre_draw_playable_tiles = playable_tiles();
        hand.insert(hand.begin(), boneyard_.front());
        boneyard_.erase(boneyard_.begin());
        previous_pass_ = false;
    } else {
        if (!boneyard_.empty()) {
            return std::nullopt;
        }
        next_seat_ = other(*seat);
        if (std::exchange(previous_pass_, true)) {
            result.round = settle(RoundEnd::locked, *seat);
        }
    }
    return result;
}

std::optional<TurnResult> Game::play_highest_double() {
    if (phase_ != Phase::awaiting_highest_double) {
        return std::nullopt;
    }
    for (int pip = Domino::max_pip; pip >= 0; --pip) {
        const Domino tile(pip, pip);
        for (auto seat : {Seat::first, Seat::second}) {
            const auto& hand = player(seat).hand;
            if (std::ranges::find(hand, tile) != hand.end()) {
                phase_ = Phase::playing;
                auto result = play_move(seat, {tile, Direction::left},
                                        board_.place(tile, Direction::left).value());
                result.action = PlayHighestDouble{};
                return result;
            }
        }
    }
    phase_ = Phase::awaiting_deal;
    return TurnResult{};
}

TurnResult Game::play_move(Seat seat, Move move, Board next) {
    auto& current = players_[static_cast<std::size_t>(seat)];
    board_ = std::move(next);
    std::erase(current.hand, move.tile);
    const int board_score = board_.score();
    const int points = board_score % 5 == 0 ? board_score : 0;
    current.score += points;
    next_seat_ = other(seat);
    previous_pass_ = false;

    TurnResult result{};
    result.action = move;
    result.actor = seat;
    result.placement = move;
    result.play_points = points;
    if (current.hand.empty()) {
        result.round = settle(RoundEnd::dominoes, seat);
    }
    return result;
}

RoundResult Game::settle(RoundEnd reason, Seat actor) {
    Seat recipient = actor;
    int pips = pip_total(player(other(actor)));
    if (reason == RoundEnd::locked) {
        pips -= pip_total(player(actor));
        if (pips < 0) {
            recipient = other(actor);
            pips = -pips;
        }
    }
    const int bonus = ((pips + 2) / 5) * 5;
    players_[static_cast<std::size_t>(recipient)].score += bonus;
    const int first = players_[0].score;
    const int second = players_[1].score;
    phase_ = std::max(first, second) >= 150 && first != second
                 ? Phase::match_complete : Phase::awaiting_deal;
    next_seat_ = reason == RoundEnd::dominoes ? std::optional{actor} : std::nullopt;
    return {reason, bonus > 0 ? std::optional{recipient} : std::nullopt, bonus};
}

std::vector<Domino> Game::playable_tiles() const {
    std::vector<Domino> tiles;
    std::bitset<Domino::tile_count> seen;
    for (auto direction : directions) {
        for (auto tile : Domino::all()) {
            if (!seen.test(tile.id()) && board_.place(tile, direction)) {
                tiles.push_back(tile);
                seen.set(tile.id());
            }
        }
    }
    return tiles;
}

}  // namespace dominoes
