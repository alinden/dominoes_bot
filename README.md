Beats you at dominoes.

**[Play Bonefish Dominoes](https://bonefishdominoes.com/)**

Bonefish plays two-player All Fives. A game ends after round scoring when one
player has at least 150 points and leads. A tie starts another round, even
above 150. The browser runs the engine and its neural models on your device.

## How the AI works

### Infer the opponent's hand

Each player knows their own hand and the public history of the round. Bonefish
represents a possible opponent hand as a 28-bit tile mask and keeps a weight
for each feasible hand. Plays, draws, and passes rule out hands that conflict
with the rules. For example, a draw means the player had no playable tile
before they drew.

Bayesian inference adds softer evidence from the move the opponent chose.
Bonefish reconstructs a candidate hand, then scores the move and its
alternatives from the opponent's perspective using sampled hidden tiles.
In the browser, it downweights a hand only when a shallow neural search and a
deeper check with fresh samples both favor the same alternative by more than
0.15 on the engine's −1 to +1 value scale. If inference runs out of time, Bonefish
keeps the hard evidence and skips the reweighting for that turn.

### Search sampled futures

The engine uses a Monte Carlo approximation of expectiminimax. It samples
opponent hands from the current distribution and pairs them with possible
boneyard orders. Each legal move is scored against the same samples. Search
maximizes Bonefish's value on its turns, minimizes it on the opponent's turns,
and averages the outcomes at draws. When few hands or draw orders remain,
the C++ engine can enumerate them.

Search groups worlds that share a draw prefix and uses alpha-beta bounds to
skip branches that cannot change the choice. With a move deadline, it searches
at increasing depths and keeps the last complete pass. This lets it compare
all root moves at the same depth even if the next pass times out.

### Use neural models for ordering and evaluation

Three small networks specialize in opening, midgame, and endgame positions.
Their inputs include both hands in the sampled world, played tiles, exposed
ends, scores, and boneyard size. The models learn from positions scored by
search. Some opening labels use a midgame network at a cutoff, and deeper
labels come from search through round settlement under sampled hidden tiles.

During play, the networks order promising moves so search can find useful
bounds sooner. They also estimate the value of a position when search reaches
its depth limit. If a branch reaches round settlement, the engine scores the
settled position directly.

## Architecture

| Component | Responsibility |
| --- | --- |
| [C++ engine](cpp/src/game.cpp) and [board](cpp/src/board.cpp) | Headless All Fives rules, legal moves, spinner geometry, scoring, and round transitions. |
| [C++ search](cpp/src/search.cpp) and [hand inference](cpp/src/hand_candidates.cpp) | Sample hidden worlds, score moves, and update the opponent-hand distribution. Native search supports multiple workers. |
| [Neural search policy](cpp/include/dominoes/neural_search.hpp) | Shared model selection, move ordering, and depth-limit evaluation for the browser and native match bot. |
| [Browser](web/) | Vite and TypeScript render the interactive board. A Web Worker runs the C++ engine compiled to WebAssembly. GitHub Pages serves the static build. |
| [Training](training/) | C++ tools generate labels, PyTorch fits and evaluates models, and an exporter writes weights the C++ runtime can load. |
| [Tests and matches](cpp/tests/) | Rule and search fixtures include positions replayed from Scala. The [match runner](src/main/scala/bot_match.scala) can swap seats on the same deal seed and checks score agreement after each action. |

## Scala and C++

**Scala is the original implementation and the easiest place to read the core
algorithm.** Start with [the move-selection code](src/main/scala/robot.scala) and
the explicit [max](src/main/scala/max_node.scala),
[min](src/main/scala/min_node.scala), and [average](src/main/scala/avg_node.scala)
search nodes.

**C++ is the faster, current implementation behind Bonefish.** It uses the
same underlying search ideas with compact hand representations, bounded
draw branches, shared search work across workers in native builds, and neural
move ordering and evaluation.

## Build, test, and experiment

### Run the terminal versions

From the repository root, build C++ with a C++20 compiler and CMake.

```sh
cmake -S cpp -B cpp/build/debug -DCMAKE_BUILD_TYPE=Debug
cmake --build cpp/build/debug
ctest --test-dir cpp/build/debug --output-on-failure
./cpp/build/debug/dominoes
```

The terminal defaults to a greedy opponent. Use `--search` for lookahead,
`--depth N` and `--workers N` to tune it, and `--bayes` for hand weighting.
To record a trace, add `--log-decisions target/cpp-decisions.log`. The log
includes the real opponent hand for diagnostics. Type `h` in the game for
terminal commands.

For Scala, install sbt and run:

```sh
sbt test
sbt -J-Xmx4G 'run --play_human --log-decisions target/decisions.log'
```

### Build and deploy the browser game

With Emscripten activated and Node.js installed, build the engine and fetch
the published weights. Local training is optional.

```sh
emcmake cmake -S cpp -B cpp/build/web -DCMAKE_BUILD_TYPE=Release
cmake --build cpp/build/web --parallel
mkdir -p web/public/engine
cp cpp/build/web/dominoes.mjs cpp/build/web/dominoes.wasm web/public/engine/
for model in opening specialist late; do
  curl -fsSL "https://bonefishdominoes.com/engine/$model.weights" \
    -o "web/public/engine/$model.weights"
done
npm --prefix web ci
npm --prefix web run dev
```

Fast uses depth 9 and up to two seconds of move search. Full strength uses
depth 15 through five tiles and searches toward round settlement from six
onward with a 10-second move cap. Inference has a separate budget and runs
at up to 1,000 hands for Fast and 6,000 for Full strength.

Use `npm --prefix web run build` to produce `web/dist`. With the
`gh-pages` branch checked out as a sibling worktree at `../bonefish-pages`,
run `bash web/deploy-pages.sh --dry-run` to preview the deployment or
`bash web/deploy-pages.sh` to publish. Set `GH_PAGES_WORKTREE` for a
different path. The script preserves the custom-domain file.

### Run paired bot matches

Build the native Release bot before running a match.

```sh
cmake -S cpp -B cpp/build/release -DCMAKE_BUILD_TYPE=Release
cmake --build cpp/build/release --parallel
```

Compare C++ depth 15 to Scala with 30 seconds each for Scala inference and
move search. The last two arguments are the pair count and first deal seed.

```sh
sbt -J-Xmx4G 'runMain BotMatch ./cpp/build/release/dominoes_match_bot 15 30 30 1 42'
```

Compare two C++ bots with depth ceilings of 15 and 68, a 60-second move cap,
and one seat-swapped pair starting at seed 48.

```sh
sbt 'runMain CppDepthMatch ./cpp/build/release/dominoes_match_bot 15 68 60 1 48'
```

The runners print the scoreboard and paths to separate decision logs under
`target/bot-match-logs` by default. After the seed, `CppDepthMatch` accepts
a log directory, both breadths, the second bot's move budget, worker count,
and both inference budgets. You can also append three exported neural weight
files in opening, midgame, and late order to use neural search for the first bot.

### Train and evaluate models

The [training guide](training/README.md) covers generating labels, comparing
architectures on held-out deal seeds, benchmarking move ordering, and
exporting the weights. The [sample games](games_vs_greedy/) illustrate the
original Scala bot's play. The [rewrite plan](rewrite_plan.md) and
[potential improvements](potential_improvements.md) record development notes.
