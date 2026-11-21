#pragma once

#include "dominoes/board.hpp"

#include <span>
#include <variant>

namespace dominoes {

enum class Seat { first, second };
enum class Phase { awaiting_deal, awaiting_highest_double, playing, match_complete };
enum class RoundEnd { dominoes, locked };

struct Move {
    Domino tile;
    Direction direction;
    bool operator==(const Move&) const = default;
};
struct Draw {};
struct Pass {};
struct PlayHighestDouble {};
using Action = std::variant<Move, Draw, Pass, PlayHighestDouble>;

struct Player {
    std::vector<Domino> hand;
    int score = 0;
};

struct RoundResult {
    RoundEnd reason;
    std::optional<Seat> bonus_recipient;
    int bonus;
};

struct TurnResult {
    Action action = PlayHighestDouble{};
    // A deal without any doubles requests a redeal and has no acting seat.
    std::optional<Seat> actor;
    std::optional<Move> placement;
    int play_points = 0;
    std::optional<std::size_t> pre_draw_hand_size;
    std::vector<Domino> pre_draw_playable_tiles;
    std::optional<RoundResult> round;
};

class Game {
public:
    Phase phase() const { return phase_; }
    std::optional<Seat> active_seat() const;
    std::optional<Seat> winner() const;
    const Board& board() const { return board_; }
    const Player& player(Seat seat) const { return players_.at(static_cast<std::size_t>(seat)); }
    const std::vector<Domino>& boneyard() const { return boneyard_; }

    // A deal is seven tiles per seat, then the boneyard in draw order.
    [[nodiscard]] bool deal(std::span<const Domino> tiles);
    // Replace the hidden tiles for one observer while preserving the public position.
    [[nodiscard]] std::optional<Game> with_hidden_tiles(
        Seat observer, std::span<const Domino> opponent_hand,
        std::span<const Domino> draw_order) const;
    std::vector<Move> legal_moves() const;
    std::vector<Action> legal_actions() const;
    std::vector<Domino> playable_tiles() const;
    [[nodiscard]] std::optional<TurnResult> apply(const Action& action);

private:
    std::optional<TurnResult> play_highest_double();
    TurnResult play_move(Seat seat, Move move, Board next);
    RoundResult settle(RoundEnd reason, Seat actor);

    Board board_;
    std::array<Player, 2> players_;
    std::vector<Domino> boneyard_;
    Phase phase_ = Phase::awaiting_deal;
    std::optional<Seat> next_seat_;
    bool previous_pass_ = false;
};

}  // namespace dominoes
