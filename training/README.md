# Round-settlement labels

This pilot uses the completed Bayes versus Baseline series. The replay converter
takes the scoreboard and the two decision logs. It reproduces each seeded deal
and marks logged choices with at least two scored moves and at most 200 feasible
opponent hands.

From the repository root, with a Release C++ build configured:

```sh
match=target/bot-match-logs/bayes-settlement-20pairs
python3 training/replay_match.py "$match/scoreboard.log" "$match"/cpp-depth-*/*.log \
  > target/neural-replay.txt
cmake --build cpp/build/release --parallel 4
g++ -std=c++20 -O3 -Icpp/include training/label_positions.cpp \
  cpp/build/release/libdominoes_greedy.a cpp/build/release/libdominoes_search.a \
  cpp/build/release/libdominoes_engine.a \
  -pthread -o target/label_positions
target/label_positions 50 200 8 < target/neural-replay.txt \
  > target/neural-pilot.jsonl 2> target/neural-pilot.log
```

The labeler takes the number of positions, maximum worlds per position, and
seconds per position. Pass `all` in place of the number to attempt every
eligible position. It cycles through deal seeds, prioritizing positions with
five or fewer remaining draws and close depth-15 scores. It allocates distinct
draw orders across the feasible hands and enumerates those orders when they fit
within each hand's share of the budget. The search uses a conservative bound
of at most 68 actions. A position contributes only when all selected hands
and all distinct root moves finish within the deadline. The progress log records
skipped positions and runtime.

An optional fourth argument supplies a previous run's progress log. The labeler
then attempts only positions that timed out in that run. To give the difficult
positions more time while keeping the completed labels:

```sh
target/label_positions all 200 8 < target/neural-replay.txt \
  > target/neural-full.jsonl 2> target/neural-full.log
target/label_positions all 200 30 target/neural-full.log < target/neural-replay.txt \
  > target/neural-retry.jsonl 2> target/neural-retry.log
cat target/neural-full.jsonl target/neural-retry.jsonl > target/neural-full-retried.jsonl
```

Each JSONL row contains a move's target conditional on a particular opponent
hand. Its value averages the evaluated draw orders for that hand. The row carries
the pre-move and post-move state, the scoring player's seat, and whether the move
itself ended the round. The boneyard size is included for learning. The draw
order stays out of the features. Keep rows with the same deal seed together
when splitting training and validation, including the seat-swapped game.

## Generate fresh boards

```sh
target/label_positions generate 5000 24 2 100000 \
  > target/neural-5000.jsonl 2> target/neural-5000.log
```

The arguments select the number of boards, maximum draw orders per hand,
seconds per board, and first deal seed. Fresh games use greedy
play, occasional random moves, and a policy that sheds high-pip tiles.
The generator keeps at most eight positions per seed and deduplicates
by spinner and ordered board chains, independent of both hands and scores.

The quotas are 10% positions with an empty boneyard from rounds that ended in
a lock, 20% positions with one or two tiles in the mover's hand, 25% positions
with both jamming and non-jamming moves against the dealt opponent hand, and
45% other positions. The rollout identifies candidate positions. The same
round-settlement search as the replay labeler supplies their labels.

This batch focuses on boards with at least 13 placed tiles and at most six
tiles in the boneyard. It scores every distinct legal move conditional on the
dealt opponent hand, using every draw order through four remaining tiles and
sampling up to 24 distinct orders otherwise. The data stores the scenario
hands and boneyard size. It excludes the hidden draw order.

The optional final argument selects the maximum number of hands per board.
The generator keeps the dealt hand and samples additional distinct hands
consistent with the recorded plays, draws, and passes. An empty boneyard
leaves just one possible hand. All selected hands must finish before the
board is saved.

```sh
target/label_positions generate 50000 24 8 300000 10 \
  > target/neural-50000-10hands.jsonl 2> target/neural-50000-10hands.log
```

## Train the pilot evaluator

With CPU PyTorch installed in the training virtual environment, run:

```sh
target/neural-venv/bin/python training/train_value.py \
  target/neural-pilot.jsonl target/neural-models
```

The encoder uses the post-move state from the scoring player's perspective.
Its 120 inputs record both hands, played tiles, exposed pips and doubles by
direction, both scores, the next player, and the boneyard size. Every training
example follows a tile play, so the previous-pass flag is always clear.

The script trains a linear scorer and a one-hidden-layer network with 32 units.
It compares both against the engine's score heuristic on whole held-out deal
seeds. Moves that immediately settle a round use the engine's value directly
and are excluded from training and prediction-error metrics.

`metrics.json` contains the held-out scores. Best move rate measures how often
the top-ranked move has the highest label. Mean regret measures how much value
the top-ranked move gives up relative to the best label. Both ranking metrics
average by decision, giving equal weight to positions with many or few
informative opponent hands. A hand is informative if at least two moves have
different target values. The `.pt` files contain the learned weights.

Run the architecture sweep on a fixed dataset:

```sh
target/neural-venv/bin/python training/train_value.py --sweep \
  target/neural-full.jsonl target/neural-size-sweep
```

The sweep trains one through four hidden layers at each of two widths, 32 and
128 units per layer. All runs use the same features, deal split, optimizer,
minibatch order, and number of epochs. The report adds training loss, parameter
count, and elapsed training time for each model. The best model on these held-out
deals still needs a check on fresh seeds before using it in the engine.
To train only selected networks, put `--architectures 2x128 3x128` after the
dataset and output paths.

Pass `--score-feature` to add the engine's current score heuristic as a 121st
input. Compare a run with this flag against a run without it using the same
dataset. The heuristic also remains a standalone benchmark in both reports.

The report includes `training_rank` for the same move-ranking metrics on the
training boards. Use `--reference LABELS REPORT` to also evaluate the held-out
seeds listed in an earlier report. The reference seeds must be disjoint from
the current dataset. This keeps a comparison fixed when the new dataset changes
how many hands each board contributes.

## Benchmark move ordering

Export the linear, 1×32, and 2×128 checkpoints from the 50,000-board run:

```sh
for model in linear 1x32 2x128; do
  target/neural-venv/bin/python training/export_value.py \
    "target/neural-50000-models/${model}.pt" > "target/neural-50000-models/${model}.weights"
done
target/label_positions benchmark 20 24 2 \
  target/neural-50000-models/{linear,1x32,2x128}.weights \
  < target/neural-replay.txt > target/neural-order-benchmark.csv
```

The benchmark takes the number of positions per boneyard-size group, draw
orders per dealt hand, and a deadline in seconds. It compares the previous
search, bounded pruning with the original move order, and bounded pruning
with the score heuristic and each model. The CSV includes elapsed time, searched
nodes, and draw cutoffs. Settled values must agree across completed runs.

Each root move is still scored independently to obtain an exact value for the
sampled scenarios. Inside its subtree, alpha-beta bounds both players' choices,
and the unevaluated probability mass bounds the possible contribution of
remaining draw outcomes. The orderer ranks moves. The search bounds determine
the cutoffs.

To compare 1×32 at every eligible node with 1×32 only when at least seven
boneyard tiles remain, include the full set of logged decisions and run:

```sh
match=target/bot-match-logs/bayes-settlement-20pairs
python3 training/replay_match.py "$match/scoreboard.log" "$match"/cpp-depth-*/*.log \
  --max-hands 200000 > target/neural-order-replay-all.txt
target/label_positions benchmark-early 6 24 8 \
  target/neural-50000-models/{linear,1x32,2x128}.weights \
  < target/neural-order-replay-all.txt > target/neural-pruning-moves-3-4.csv
```

This selects up to six decisions each before the third and fourth tile
placements. Pruning with the original move order is the control.
Use `benchmark-early-wide` with the same arguments to compare the 1×32 and
2×128 hybrids on the identical positions and draw samples.
Append `5 8` to compare the fifth and sixth placements with a minimum
boneyard size of eight for learned ordering.
Use `benchmark-early-medium` and add the 2×64 weights after the other model
paths to include the medium model in either matched timing test.

## Label earlier boards

`benchmark-band` generates fresh decisions with six through twelve tiles
already placed. It compares the original move order with the 1×32, 2×64,
and 2×128 models. Each model orders moves while at least eight tiles remain
in the boneyard. The comparison uses the same hands and draw samples for
every policy, with four search workers as in production labeling.

```sh
target/label_positions benchmark-band 28 12 5 400000 5 \
  target/neural-50000-models/1x32.weights \
  target/neural-50000-medium/2x64.weights \
  target/neural-50000-models/2x128.weights \
  > target/neural-band-pilot.csv 2> target/neural-band-pilot.log
```

The numeric arguments are board count, draw orders per hand, seconds per
board per policy, first seed, and maximum hands per board. Use
`generate-band` with the same arguments followed by two policy names,
one for boards with six through eight tiles placed and one for nine through
twelve. Policy names are `original`, `1x32`, `2x64`, and `2x128`. The
generator targets equal numbers at each board size and saves only boards
where every selected hand and root move reached settlement.
Append a first and last board size to `benchmark-band` to time a different
range. Append a single policy name after the range to measure just that
policy. For example, `3 5 2x128` tests boards with three through five tiles
already placed using the 2×128 hybrid.

To train on the early band while retaining late positions:

```sh
target/neural-venv/bin/python -u training/train_value.py \
  target/neural-10000-early.jsonl target/neural-early-mixed-models \
  --architectures 2x64 2x128 --skip-linear \
  --mix-late target/neural-50000-10hands.jsonl target/neural-50000-models/metrics.json
```

The trainer reserves every fifth early deal seed and reuses the 50,000-board
run's late holdout. It samples one late training board for every three early
training boards and gives each board equal total weight in the loss. Metrics
include training and held-out move ranking separately for both stages.
Pass `--late-share .5` for an even board mix. To compare the two wider models
on that mix, use `--architectures 2x128 2x256` in the same command.

To compare the 6–12 specialist with the policy that labeled the original
early dataset, run:

```sh
target/neural-venv/bin/python training/export_value.py \
  target/neural-early-specialist/2x128.pt > target/neural-early-specialist/2x128.weights
target/label_positions benchmark-phases 70 12 8 700000 5 \
  target/neural-early-specialist/2x128.weights target/neural-50000-medium/2x64.weights \
  target/neural-50000-models/2x128.weights \
  > target/neural-6-12-phases.csv 2> target/neural-6-12-phases.log
```

The specialist orders moves on nodes with 6–12 tiles placed and at least
eight boneyard tiles. The late-model variants switch to the old 2×64 from
13 tiles placed, with separate runs for 4–6 and 8+ boneyard tiles.
Unmatched nodes use the original move order.

Use `benchmark-queue` with the same arguments to compare one hand at a time
with a shared four-worker queue. Both runs use the specialist plus late
4–6 ordering policy and check that the per-hand scores match.

Generate an additional batch with the shared queue, excluding every layout
already in the first early dataset:

```sh
python3 training/board_keys.py target/neural-10000-early.jsonl \
  > target/neural-10000-early-board-keys.txt
target/label_positions generate-phases 10000 12 8 800000 5 \
  target/neural-early-specialist/2x128.weights target/neural-50000-medium/2x64.weights \
  target/neural-50000-models/2x128.weights target/neural-10000-early-board-keys.txt \
  > target/neural-10000-early-extra.jsonl 2> target/neural-10000-early-extra.log
```

The queue shares workers across hands. It saves each hand's values separately
and retains a board only once all selected hands reach settlement.

To retrain the specialist after verifying the two early batches are disjoint:

```sh
cat target/neural-10000-early.jsonl target/neural-10000-early-extra.jsonl \
  > target/neural-20000-early.jsonl
target/neural-venv/bin/python -u training/train_value.py \
  target/neural-20000-early.jsonl target/neural-early-specialist-20000 \
  --architectures 2x128 2x64 --skip-linear --balance-boards \
  --holdout-report target/neural-early-specialist/metrics.json
```

The original early holdout stays fixed, and all additional boards are
available for training. The report uses the same ranking and regret metrics
as the first specialist run.

The trainer also accepts `--architectures 2x64`. Use `--skip-linear` when the
linear baseline is already available for the same dataset and feature setup.
