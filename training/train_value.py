"""Train and compare two post-move evaluators on round-settlement labels."""

import argparse
from array import array
from collections import Counter, defaultdict
import json
import math
from pathlib import Path
import random
import statistics
import time

import torch
from torch import nn

TILES = [(high, low) for high in range(7) for low in range(high + 1)]
TILE_ID = {tile: index for index, tile in enumerate(TILES)}
TIE_TOLERANCE = 1e-6


def tile_flags(ids):
    ids = set(ids)
    return [float(index in ids) for index in range(len(TILES))]


def board_ends(chains, spinner):
    left, right, up, down = chains
    if spinner < 0:
        if not left:
            return [(None, False)] * 4
        outer, inner = left[-1]
        return [(left[0][0], left[0][0] == left[0][1]), (inner, inner == outer),
                (None, False), (None, False)]

    pip = TILES[spinner][0]
    ends = [(arm[0][0], arm[0][0] == arm[0][1]) if arm else (pip, True)
            for arm in (left, right, up, down)]
    if left and not right:  # A tee exposes only the two horizontal directions.
        ends[2:] = [(None, False), (None, False)]
    return ends


def features(row, score_feature=False):
    state = row["after"]
    perspective = row["perspective"]
    hands = state["firstHand"], state["secondHand"]
    scores = state["scores"]
    chains = state["chains"]
    spinner = state["spinner"]
    played = [TILE_ID[tuple(sorted(tile, reverse=True))] for arm in chains for tile in arm]
    if spinner >= 0:
        played.append(spinner)
    encoded = tile_flags(hands[perspective]) + tile_flags(hands[1 - perspective]) + tile_flags(played)
    for pip, double in board_ends(chains, spinner):
        encoded.extend(float(pip == candidate) for candidate in range(7))
        encoded.append(float(double))
    encoded.extend([scores[perspective] / 150, scores[1 - perspective] / 150,
                    float(state["active"] == perspective), state["boneyardSize"] / 14])
    if score_feature:
        encoded.append(heuristic(row))
    return encoded


def heuristic(row):
    scores = row["after"]["scores"]
    perspective = row["perspective"]
    lead = scores[perspective] - scores[1 - perspective]
    return math.tanh(lead / max(30, 150 - max(scores)))


def train(model, x, y, name, weights=None):
    started = time.perf_counter()
    optimizer = torch.optim.AdamW(model.parameters(), lr=0.003, weight_decay=0.01)
    loss = nn.MSELoss(reduction="none")

    def batch_loss(prediction, target, weight):
        errors = loss(prediction, target)
        return (errors if weight is None else errors * weight).mean()
    shuffle = torch.Generator().manual_seed(613)
    with torch.no_grad():
        start = batch_loss(predict_tensor(model, x), y, weights).item()
    for epoch in range(80):
        for indices in torch.randperm(len(x), generator=shuffle).split(64):
            optimizer.zero_grad()
            batch_loss(model(x[indices]), y[indices], None if weights is None else weights[indices]).backward()
            optimizer.step()
        if len(x) >= 100000 and (epoch + 1) % 20 == 0:
            print(f"{name}: epoch {epoch + 1}/80", flush=True)
    with torch.no_grad():
        finished = batch_loss(predict_tensor(model, x), y, weights).item()
    print(f"{name}: training MSE {start:.4f} -> {finished:.4f}", flush=True)
    return {"training_mse": finished, "training_seconds": time.perf_counter() - started}


def predict_tensor(model, x):
    return torch.cat([model(batch) for batch in x.split(8192)])


def encode_rows(rows, score_feature):
    encoded = array("f")
    for row in rows:
        encoded.extend(features(row, score_feature))
    return torch.frombuffer(encoded, dtype=torch.float32).reshape(len(rows), -1)


def label_rows(path):
    with path.open() as source:
        for line in source:
            if line.strip():
                row = json.loads(line)
                row.pop("before", None)
                yield row


def value_net(inputs, width, depth):
    layers = []
    for _ in range(depth):
        layers.extend([nn.Linear(inputs, width), nn.ReLU()])
        inputs = width
    layers.extend([nn.Linear(inputs, 1), nn.Tanh()])
    return nn.Sequential(*layers)


def position_key(row):
    return row["seed"], row["game"], row["turn"]


def late_mix(labels, report_path, early_seeds, board_count):
    holdout = set(json.loads(report_path.read_text())["held_out_seeds"])
    available = set()
    validation = []
    for row in label_rows(labels):
        seed = row["seed"]
        if seed in early_seeds:
            raise ValueError("Early and late deal seeds overlap")
        if seed in holdout:
            validation.append(row)
        else:
            available.add(position_key(row))
    if {row["seed"] for row in validation} != holdout:
        raise ValueError("Late labels do not contain all held-out deal seeds")
    if len(available) < board_count:
        raise ValueError("Too few eligible late boards for the requested training mix")
    selected = set(random.Random(613).sample(sorted(available), board_count))
    training = [row for row in label_rows(labels) if position_key(row) in selected]
    return training, validation, holdout


def assess(rows, predictions):
    errors = [abs(prediction - row["target"]) for row, prediction in zip(rows, predictions)
              if not row["moveEndsRound"]]
    groups = defaultdict(list)
    for row, prediction in zip(rows, predictions):
        # Search already knows the exact value when a move directly settles.
        value = row["target"] if row["moveEndsRound"] else prediction
        groups[position_key(row), tuple(row["opponentHand"])].append((row["target"], value))

    by_position = defaultdict(list)
    for (position, _), moves in groups.items():
        best = max(target for target, _ in moves)
        worst = min(target for target, _ in moves)
        if best - worst <= TIE_TOLERANCE:
            continue
        predicted_best = max(prediction for _, prediction in moves)
        picks = [target for target, prediction in moves if predicted_best - prediction <= TIE_TOLERANCE]
        hit_rate = statistics.mean(best - target <= TIE_TOLERANCE for target in picks)
        regret = statistics.mean(best - target for target in picks)
        by_position[position].append((hit_rate, regret))

    return {
        "mae": statistics.mean(errors),
        "mse": statistics.mean(error * error for error in errors),
        "best_move_rate": statistics.mean(statistics.mean(hit for hit, _ in groups)
                                          for groups in by_position.values()),
        "mean_regret": statistics.mean(statistics.mean(regret for _, regret in groups)
                                        for groups in by_position.values()),
        "informative_decisions": len(by_position),
        "informative_hand_groups": sum(map(len, by_position.values())),
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("labels", type=Path)
    parser.add_argument("output", type=Path)
    architecture_options = tuple(f"{depth}x{units}" for units in (32, 128) for depth in range(1, 5))
    extra_architectures = ("2x64", "2x256")
    selection = parser.add_mutually_exclusive_group()
    selection.add_argument("--sweep", action="store_true", help="compare 1–4 hidden layers at 32 and 128 units")
    selection.add_argument("--architectures", nargs="+", choices=architecture_options + extra_architectures,
                           help="train selected architectures from the sweep")
    parser.add_argument("--score-feature", action="store_true", help="include the engine's score heuristic as an input")
    parser.add_argument("--skip-linear", action="store_true", help="reuse an existing linear benchmark")
    parser.add_argument("--reference", type=Path, nargs=2, metavar=("LABELS", "REPORT"),
                        help="also score the held-out deals from a previous run")
    parser.add_argument("--holdout-report", type=Path,
                        help="reuse the held-out deal seeds from an earlier report")
    parser.add_argument("--mix-late", type=Path, nargs=2, metavar=("LABELS", "REPORT"),
                        help="mix in late boards while preserving the prior late holdout")
    parser.add_argument("--late-share", type=float, default=.25,
                        help="fraction of training boards drawn from the late dataset (default: .25)")
    parser.add_argument("--balance-boards", action="store_true",
                        help="give each training board equal total weight in the loss")
    args = parser.parse_args()
    if not 0 < args.late_share < 1:
        parser.error("late share must be between zero and one")
    rows = list(label_rows(args.labels))
    seeds = sorted({row["seed"] for row in rows})
    if len(seeds) < 5:
        parser.error("need at least five distinct deal seeds")
    held_out = set(seeds[::5])
    if args.holdout_report:
        held_out = set(json.loads(args.holdout_report.read_text())["held_out_seeds"])
        if not held_out or not held_out < set(seeds):
            parser.error("holdout report must identify a nonempty proper subset of the dataset's deal seeds")
    training_boards = [row for row in rows if row["seed"] not in held_out]
    mix = None
    late_validation = None
    early_count = len(training_boards)
    if args.mix_late:
        early_boards = len({position_key(row) for row in training_boards})
        late_count = round(early_boards * args.late_share / (1 - args.late_share))
        if late_count < 1:
            parser.error("late share and early board count select no late boards")
        late_training, late_validation, late_holdout = late_mix(
            *args.mix_late, set(seeds), late_count)
        training_boards.extend(late_training)
        mix = {"late_share": args.late_share,
               "early_training_decisions": early_boards, "late_training_decisions": late_count,
               "late_held_out_seeds": sorted(late_holdout)}
    training_indices = [i for i, row in enumerate(training_boards) if not row["moveEndsRound"]]
    training = [training_boards[i] for i in training_indices]
    validation = [row for row in rows if row["seed"] in held_out]
    torch.set_num_threads(1)
    torch.manual_seed(613)
    x_training_boards = encode_rows(training_boards, args.score_feature)
    x = x_training_boards[training_indices]
    y = torch.tensor([[row["target"]] for row in training], dtype=torch.float32)
    weights = None
    if mix or args.balance_boards:
        counts = Counter(position_key(row) for row in training)
        average = len(training) / len(counts)
        weights = torch.tensor([[average / counts[position_key(row)]] for row in training], dtype=torch.float32)
    if mix:
        mix["early_optimized_decisions"] = sum(key[0] in seeds for key in counts)
        mix["late_optimized_decisions"] = len(counts) - mix["early_optimized_decisions"]
    x_validation = encode_rows(validation, args.score_feature)
    evaluation_sets = {"training_rank": (training_boards, x_training_boards),
                       "held_out": (validation, x_validation)}
    exact_indices = [i for i, row in enumerate(validation) if "leafTiles" not in row]
    if 0 < len(exact_indices) < len(validation):
        evaluation_sets["exact_held_out"] = ([validation[i] for i in exact_indices],
                                            x_validation[exact_indices])
    if late_validation is not None:
        evaluation_sets["early_training"] = (training_boards[:early_count], x_training_boards[:early_count])
        evaluation_sets["late_training"] = (training_boards[early_count:], x_training_boards[early_count:])
        evaluation_sets["late_held_out"] = (late_validation, encode_rows(late_validation, args.score_feature))
    if args.reference:
        reference_labels, reference_report = args.reference
        reference_seeds = set(json.loads(reference_report.read_text())["held_out_seeds"])
        if reference_seeds.intersection(seeds):
            parser.error("reference and current deal seeds overlap")
        reference = [row for row in label_rows(reference_labels) if row["seed"] in reference_seeds]
        if {row["seed"] for row in reference} != reference_seeds:
            parser.error("reference labels do not contain all requested held-out deal seeds")
        evaluation_sets["reference"] = (reference, encode_rows(reference, args.score_feature))
    width = x.shape[1]
    args.output.mkdir(parents=True, exist_ok=True)
    report = {
        "feature_count": width,
        "score_feature": args.score_feature,
        "balanced_boards": weights is not None,
        "training_rows": len(training),
        "training_decisions": len({position_key(row) for row in training}),
        "held_out_seeds": sorted(held_out),
        "validation_rows": len(validation),
        "validation_decisions": len({position_key(row) for row in validation}),
        "models": {},
    }
    if mix:
        report["mix"] = mix
    baseline = {key: assess(group, [heuristic(row) for row in group])
                for key, (group, _) in evaluation_sets.items()}
    report["models"]["heuristic"] = baseline.pop("held_out") | baseline
    print(f"Training on {len(training):,} labels; held out {len(validation):,} labels.", flush=True)
    (args.output / "metrics.json").write_text(json.dumps(report, indent=2) + "\n")
    models = {"linear": nn.Sequential(nn.Linear(width, 1), nn.Tanh())}
    if args.sweep or args.architectures:
        for name in architecture_options + extra_architectures:
            depth, units = map(int, name.split("x"))
            model = value_net(width, units, depth)
            if (args.sweep and name in architecture_options) or (args.architectures and name in args.architectures):
                models[name] = model
    else:
        models["neural"] = value_net(width, 32, 1)
    if args.skip_linear:
        del models["linear"]
    for name, model in models.items():
        training_stats = train(model, x, y, name, weights)
        model.eval()
        with torch.no_grad():
            results = {key: assess(group, predict_tensor(model, inputs).flatten().tolist())
                       for key, (group, inputs) in evaluation_sets.items()}
        metrics = results.pop("held_out")
        report["models"][name] = metrics | results | training_stats | {
            "parameters": sum(parameter.numel() for parameter in model.parameters()),
        }
        torch.save(model.state_dict(), args.output / f"{name}.pt")
        print(f"{name}: held-out MSE {metrics['mse']:.4f}, "
              f"best move {metrics['best_move_rate']:.1%}, regret {metrics['mean_regret']:.4f}, "
              f"{training_stats['training_seconds']:.1f} s", flush=True)
        print(f"{name}: training best move {results['training_rank']['best_move_rate']:.1%}", flush=True)
        if mix:
            print(f"{name}: early train {results['early_training']['best_move_rate']:.1%}, "
                  f"late train {results['late_training']['best_move_rate']:.1%}, "
                  f"late held-out {results['late_held_out']['best_move_rate']:.1%}, "
                  f"late regret {results['late_held_out']['mean_regret']:.4f}", flush=True)
        (args.output / "metrics.json").write_text(json.dumps(report, indent=2) + "\n")

    print(json.dumps(report, indent=2))


if __name__ == "__main__":
    main()
