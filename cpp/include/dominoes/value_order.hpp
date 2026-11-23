#pragma once

#include "dominoes/search.hpp"

#include <algorithm>
#include <array>
#include <cmath>
#include <fstream>
#include <stdexcept>

namespace dominoes {

inline constexpr std::size_t feature_count = 120;
inline constexpr std::size_t max_units = 128;

inline std::array<float, max_units> value_features(const dominoes::Game& game, dominoes::Seat mover) {
    using namespace dominoes;
    std::array<float, max_units> values{};
    const auto other = mover == Seat::first ? Seat::second : Seat::first;
    const auto& own = game.player(mover);
    const auto& opponent = game.player(other);
    for (auto tile : own.hand) values[tile.id()] = 1;
    for (auto tile : opponent.hand) values[28 + tile.id()] = 1;
    for (auto tile : Domino::all()) {
        if (game.board().contains(tile)) values[56 + tile.id()] = 1;
    }
    const auto& board = game.board();
    const auto shape = board.shape();
    for (int index = 0; index < 4; ++index) {
        if (shape == BoardShape::empty || (index >= 2 && (shape == BoardShape::snake || shape == BoardShape::tee))) {
            continue;
        }
        const auto& chain = board.chain(static_cast<Direction>(index));
        const auto end = shape == BoardShape::snake && index == 1
            ? board.chain(Direction::left).back().reversed()
            : chain.empty() ? board.spinner().value() : chain.front();
        values[84 + 8 * index + end.exposed_pip()] = 1;
        values[84 + 8 * index + 7] = end.tile().is_double();
    }
    values[116] = own.score / 150.0f;
    values[117] = opponent.score / 150.0f;
    values[118] = game.active_seat() == mover;
    values[119] = game.boneyard().size() / 14.0f;
    return values;
}

class ValueOrder {
public:
    explicit ValueOrder(const char* path) {
        std::ifstream input(path);
        load(input);
    }

    explicit ValueOrder(std::istream& input) {
        load(input);
    }

    double operator()(const Game& game, Seat mover) const {
        auto current = value_features(game, mover);
        std::array<float, max_units> result;
        for (const auto& layer : layers_) {
            std::copy(layer.bias.begin(), layer.bias.end(), result.begin());
            for (std::size_t column = 0; column < layer.columns; ++column) {
                if (current[column] == 0) continue;
                const auto* weight = layer.weight.data() + column * layer.units;
                for (std::size_t unit = 0; unit < layer.units; ++unit) {
                    result[unit] += current[column] * weight[unit];
                }
            }
            for (std::size_t unit = 0; unit < layer.units; ++unit) {
                current[unit] = std::max(0.0f, result[unit]);
            }
        }
        return std::tanh(result[0]);
    }

private:
    void load(std::istream& input) {
        std::size_t count;
        if (!(input >> count) || count < 1 || count > 5) throw std::runtime_error("Invalid model layer count");
        std::size_t expected = feature_count;
        for (std::size_t i = 0; i < count; ++i) {
            std::size_t columns, units;
            if (!(input >> columns >> units) || columns != expected || units < 1 || units > max_units) {
                throw std::runtime_error("Invalid model dimensions");
            }
            Layer layer{columns, units, std::vector<float>(units), std::vector<float>(columns * units)};
            for (auto* values : {&layer.bias, &layer.weight}) {
                for (float& value : *values) {
                    if (!(input >> value) || !std::isfinite(value)) throw std::runtime_error("Invalid model weight");
                }
            }
            layers_.push_back(std::move(layer));
            expected = units;
        }
        if (expected != 1) throw std::runtime_error("Model must return one value");
    }

    struct Layer {
        std::size_t columns;
        std::size_t units;
        std::vector<float> bias;
        std::vector<float> weight;
    };
    std::vector<Layer> layers_;
};

}  // namespace dominoes
