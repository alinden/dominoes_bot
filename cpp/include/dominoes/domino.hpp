#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <optional>

namespace dominoes {

class Domino {
public:
    static constexpr int max_pip = 6;
    static constexpr std::size_t tile_count = 28;

    Domino(int first, int second);

    int high() const { return high_; }
    int low() const { return low_; }
    bool is_double() const { return high_ == low_; }
    int pip_total() const { return high_ + low_; }
    std::size_t id() const { return static_cast<std::size_t>(high_ * (high_ + 1) / 2 + low_); }

    static const std::array<Domino, tile_count>& all();
    bool operator==(const Domino&) const = default;

private:
    int high_;
    int low_;
};

using TileMask = std::uint32_t;

inline TileMask tile_bit(Domino tile) { return TileMask{1} << tile.id(); }

class PlayedDomino {
public:
    PlayedDomino(Domino tile, int exposed_pip);

    Domino tile() const { return tile_; }
    int exposed_pip() const { return exposed_pip_; }
    int score() const;
    std::optional<PlayedDomino> connect(Domino next) const;
    PlayedDomino reversed() const;
    bool operator==(const PlayedDomino&) const = default;

private:
    Domino tile_;
    int exposed_pip_;
};

}  // namespace dominoes
