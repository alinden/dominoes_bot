#include "dominoes/hand_candidates.hpp"

#include <algorithm>
#include <bit>

namespace dominoes {
namespace {

using Mask = std::uint32_t;
constexpr Mask all_tiles = (Mask{1} << Domino::tile_count) - 1;

Mask bit(Domino tile) {
    return Mask{1} << tile.id();
}

Mask mask(std::span<const Domino> tiles) {
    Mask result = 0;
    for (auto tile : tiles) {
        result |= bit(tile);
    }
    return result;
}

void enumerate(Mask available, std::size_t count, Mask selected, std::vector<Mask>& hands) {
    if (count == 0) {
        hands.push_back(selected);
        return;
    }
    while (static_cast<std::size_t>(std::popcount(available)) >= count) {
        const Mask first = Mask{1} << std::countr_zero(available);
        available ^= first;
        enumerate(available, count - 1, selected | first, hands);
    }
}

Mask unknown_tiles(const Game& position, Seat observer) {
    Mask unknown = all_tiles & ~mask(position.player(observer).hand);
    for (auto tile : Domino::all()) {
        if (position.board().contains(tile)) {
            unknown &= ~bit(tile);
        }
    }
    return unknown;
}

std::vector<Domino> tiles_from(Mask tiles) {
    std::vector<Domino> result;
    result.reserve(std::popcount(tiles));
    while (tiles != 0) {
        result.push_back(Domino::all()[std::countr_zero(tiles)]);
        tiles &= tiles - 1;
    }
    return result;
}

}  // namespace

HandCandidates::HandCandidates(Seat observer, std::span<const Domino> own_hand,
                               std::size_t opponent_size)
    : observer_(observer) {
    enumerate(all_tiles & ~mask(own_hand), opponent_size, 0, hands_);
}

void HandCandidates::observe(const Game& position, const TurnResult& result) {
    if (!result.actor || result.round) {
        return;
    }
    const bool own_action = result.actor == observer_;
    if (result.placement) {
        const auto tile = result.placement->tile;
        if (std::holds_alternative<PlayHighestDouble>(result.action)) {
            Mask higher_doubles = 0;
            for (int pip = tile.high() + 1; pip <= Domino::max_pip; ++pip) {
                higher_doubles |= bit(Domino(pip, pip));
            }
            std::erase_if(hands_, [higher_doubles](Mask hand) { return hand & higher_doubles; });
        }
        if (!own_action) {
            std::erase_if(hands_, [tile](Mask hand) { return !(hand & bit(tile)); });
            for (auto& hand : hands_) {
                hand &= ~bit(tile);
            }
        }
        return;
    }

    if (own_action) {
        if (std::holds_alternative<Draw>(result.action)) {
            const Mask drawn = bit(position.player(observer_).hand.front());
            std::erase_if(hands_, [drawn](Mask hand) { return hand & drawn; });
        }
        return;
    }

    const bool is_draw = std::holds_alternative<Draw>(result.action);
    const Mask playable = is_draw ? mask(result.pre_draw_playable_tiles)
                                  : mask(position.playable_tiles());
    std::erase_if(hands_, [playable](Mask hand) { return hand & playable; });
    if (is_draw) {
        const Mask unknown = unknown_tiles(position, observer_);
        std::vector<Mask> expanded;
        for (const auto hand : hands_) {
            for (Mask choices = unknown & ~hand; choices != 0; choices &= choices - 1) {
                expanded.push_back(hand | (Mask{1} << std::countr_zero(choices)));
            }
        }
        std::ranges::sort(expanded);
        expanded.erase(std::unique(expanded.begin(), expanded.end()), expanded.end());
        hands_ = std::move(expanded);
    }
}

std::vector<SearchSample> HandCandidates::sample(const Game& position, std::size_t count,
                                                 std::mt19937& random) const {
    if (hands_.empty()) {
        return {};
    }
    const Mask unknown = unknown_tiles(position, observer_);
    std::uniform_int_distribution<std::size_t> pick(0, hands_.size() - 1);
    std::vector<SearchSample> result;
    result.reserve(count);
    while (result.size() < count) {
        const Mask hand = hands_[pick(random)];
        auto draw_order = tiles_from(unknown & ~hand);
        std::shuffle(draw_order.begin(), draw_order.end(), random);
        result.push_back({tiles_from(hand), std::move(draw_order), 1.0});
    }
    return result;
}

std::vector<ScoredAction> HandCandidates::score_moves(const Game& position, int depth,
                                                      std::mt19937& random) const {
    const auto samples = sample(position, search_samples, random);
    return samples.empty() ? std::vector<ScoredAction>{}
                           : score_sampled_actions(position, observer_, depth, samples);
}

}  // namespace dominoes
