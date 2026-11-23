# dominoes_bot
Beats you at dominoes.

## How to run the program

### C++ terminal game

Build and start a human-versus-greedy match from the repository root with a C++20 compiler and CMake.

```sh
cmake -S cpp -B cpp/build/debug -DCMAKE_BUILD_TYPE=Debug
cmake --build cpp/build/debug
./cpp/build/debug/dominoes
```

Run `./cpp/build/debug/dominoes --show-bot-hand` to reveal the bot's hand for testing. The boneyard stays hidden. Type `h` during your turn for the command list and tile indices.

Add `--search` to play against the lookahead bot. It samples equally from hands consistent with observed actions
and searches three actions ahead across 16 samples. Use `--depth N` to enable search at a different depth. Both flags
can be combined with `--show-bot-hand`.
When 32 or fewer opponent hands remain possible, move search covers every hand with its model weight. With three
or fewer boneyard tiles, it also checks every draw order. The decision log reports when it uses this coverage.
Use `--workers N` to enable parallel search with up to `N` workers. The default is one.

Use `--bayes` to enable Bayesian hand weighting. Public plays, draws, and passes first eliminate impossible hands.
After an opponent play, the bot evaluates each feasible hand directly when at most 300 remain. It updates the weights
only after every sampled search branch reaches round settlement. If the inference deadline expires, it keeps the hard
evidence and previously learned weights. The 15-second inference budget is independent of move search. Set a different
budget with `--inference-time-seconds N`. The decision log reports when inference runs, skips, or times out.

Use `--log-decisions cpp-decisions.log` alongside `--search` or `--depth N` to record candidate moves, their mean
search scores, and the chosen move. Each run overwrites the specified log file.
The log also shows the most and least likely opponent tiles, top candidate hands, and the actual hand's reported
probability and rank. The actual hand is included for diagnostics, and the log can reveal it during play.

### Scala game

To try it, install [sbt](https://www.scala-sbt.org/) and then run `sbt -J-Xmx4G "run --play_human"`.
This sets the maximum JVM heap size to 4 GB, which is recommended for AI search. The program can run
with less memory, but performance may suffer.

To see how its thinking, use the decision log: `sbt -J-Xmx4G "run --play_human --log-decisions decisions.log"`.
Consider reading through the sample [decisions.log](decisions.log) in the repo to see an example of how the bot thinks
without needing to run a game yourself.

The Scala bot defaults to 15 seconds for opponent-hand inference and a fresh 30 seconds for move search. Set the
budgets with `--inference-time-seconds N` and `--turn-time-seconds N` in the `run` arguments. If inference runs out
of time, search uses the available hand estimate or samples one from the unknown tiles. Setting `--turn-time-seconds 0`
uses the original timing policy. Display pauses are outside both budgets.

### Play in a browser

The terminal-style web app runs the C++ bot in a Web Worker. Open **Settings** to
start a series and choose Greedy or Search, depth, breadth, Bayes, separate search
and inference budgets, and the number of games. The browser build uses one search
thread. A search budget of zero removes the move-time limit.

With a recent Emscripten SDK activated and Node.js installed, run from the repo root:

```sh
emcmake cmake -S cpp -B cpp/build/web -DCMAKE_BUILD_TYPE=Release
cmake --build cpp/build/web --parallel
mkdir -p web/public/engine
cp cpp/build/web/dominoes.mjs cpp/build/web/dominoes.wasm web/public/engine/
npm --prefix web ci
npm --prefix web run dev
```

Open the local URL Vite prints. In Terminal view, choose a tile from its direction
row. In Board view, drag a tile to a highlighted end, or click the tile and then
click the highlighted end. Draw and Pass
are available in both views. Switch views in Settings during play. After a C++
change, rebuild and copy both engine files again.
Run `npm --prefix web run build` to generate the static site in `web/dist`.

**Neural search** orders moves with the opening specialist through five tiles,
the midgame specialist at 6–12 tiles with at least eight boneyard tiles, and
the late model at 13 or more tiles with four to six boneyard tiles. At a depth
cutoff it chooses the value model by the leaf's tile count. If fewer than six
tiles are placed at the start of a turn, the search goes up to depth 15.
From six tiles onward it aims for round settlement, subject to the search time
setting. The depth input controls the regular Search model.
To enable this option, stage these files before running or building Vite:

```sh
cp target/neural-opening-specialist-18000/3x128.weights web/public/engine/opening.weights
cp target/neural-early-specialist-20000/2x128.weights web/public/engine/specialist.weights
cp target/neural-50000-medium/2x64.weights web/public/engine/late.weights
```

See the [training guide](training/README.md) for exporting checkpoints to
`.weights` files.

### C++ versus Scala

Build the Release C++ binaries, then run a paired head-to-head match from the repository root.

```sh
cmake -S cpp -B cpp/build/release -DCMAKE_BUILD_TYPE=Release
cmake --build cpp/build/release
sbt -J-Xmx4G 'runMain BotMatch ./cpp/build/release/dominoes_match_bot 15 30 30 1 42'
```

The arguments after the executable path are C++ depth, Scala inference seconds, Scala search seconds, number of
pairs, and the first deal seed. C++ uses its hard-constraint hand model and 16 samples. Each pair swaps seats and
reuses the same ordered deals. The runner checks scores after each action and prints decision times and game results.

The runner prints paths to separate C++ and Scala decision logs at startup. Follow both in another pane with
`tail -F /path/to/cpp-decisions.log /path/to/scala-decisions.log`, or follow one per pane. Both logs use the turn
numbers shown in the match output. Pass a directory after the seed to change where the runner creates its log folder.

### C++ depth comparison

Run a seat-swapped pair with depth ceilings of 15 and 68 and a 60-second search budget for each bot.

```sh
sbt 'runMain CppDepthMatch ./cpp/build/release/dominoes_match_bot 15 68 60 1 48'
```

Each decision log shows the deepest completed pass and whether it reached round settlement on every sampled branch.
On timeout the bot plays the best move from the last completed pass. You can pass a log directory and a separate
breadth for each bot after the seed. For example, `15 68 60 1 48 target/bot-match-logs 30 12` gives depth 15 a
breadth of 30 and depth 68 a breadth of 12. Both breadths default to 16.
The search budget defaults to the same value for both bots. After the breadths, optionally pass the second bot's
budget and the worker limit for both bots. A budget of `0` runs one direct search at the configured depth without a
time limit. For example, `68 15 60 1 49 target/bot-match-logs 12 30 60 4` uses up to four workers per search.
Two final arguments can enable C++ inference independently, with `0` selecting the hard-constraint baseline.
For a Bayes versus baseline pair, use `15 15 60 1 50 target/bot-match-logs 16 16 60 4 15 0`.

To run unit tests: `sbt test`.

## How the AI works

The primary challenge in dominoes is making decisions with incomplete information. Each player knows their own tiles and the public game state,
but not the opponent's hand. Drawing tiles introduces an additional source of randomness.

The AI maintains a probability distribution over possible opponent hands, updating it as new information becomes available. Representing complete
hands rather than independent tile probabilities captures dependencies between tiles and allows observations to eliminate impossible configurations and
to reduce the likelihood of hands that would've likely been played differently.

Move selection uses a Monte Carlo approximation of [Expectiminimax](https://en.wikipedia.org/wiki/Expectiminimax). For each available move,
the AI samples possible opponent hands from the probability distribution and evaluates the resulting game states through a recursive game-tree
search. The sampling approach handles uncertainty about the opponent's hand and randomness from future tile draws within the same search framework.

To estimate how likely an observed move would be for a particular hand, the AI simulates the opponent's decision-making process from their perspective.
It uses these estimates to update the probability distribution through Bayesian inference.

The implementation uses bounded search depths, sampling, and additional heuristics to manage computational complexity. The resulting algorithm is an
approximation rather than an exhaustive solution to the game.

## Architecture

The implementation is divided into three main components: the game engine, the search algorithm, and the opponent-hand model.

### Game engine

`Game` and `Board` represent the current game state, including the players' hands, scores, tiles on the board, and remaining boneyard. The game engine handles legal moves, scoring, drawing, passing, and round termination independently of the AI.

### Game-tree search

`Robot` coordinates move selection using a Monte Carlo approximation of expectiminimax. The search tree contains three types of nodes:

- `MaxNode` selects the highest-valued move for the robot.
- `MinNode` selects the lowest-valued move from the robot's perspective, representing the opponent's decisions.
- `AvgNode` evaluates uncertain outcomes by sampling possible opponent hands and simulating draws.

```text
MaxNode (robot chooses a move)
  |
  +-- AvgNode (Move A)
  |     |
  |     +-- MinNode (sampled opponent hand 1)
  |     |     |
  |     |     +-- AvgNode (opponent response)
  |     |           |
  |     |           +-- MaxNode (next robot turn)
  |     |           |
  |     |           +-- MaxNode (next robot turn)
  |     |
  |     +-- MinNode (sampled opponent hand 2)
  |
  +-- AvgNode (Move B)
        |
        +-- ...
```

The search alternates between player decisions and probabilistic evaluations. At terminal positions, it evaluates the resulting scores; at the search depth limit, it uses a heuristic.

To balance playing strength and response time, the robot performs successive searches with increasing depth and simulation budgets. It stops after a search exceeds a configurable time target or the available search configurations are exhausted.

### Opponent-hand inference

`Perspective` represents a player's knowledge of the game, including a probability distribution over possible opponent hands.

The distribution is updated as the opponent plays, draws, or passes. Bayesian inference uses both the constraints imposed by observed actions and estimates of how likely the opponent would have been to choose those actions with different hands.

Possible hands are represented using a 28-bit integer mask, with one bit for each domino in a double-six set. This allows efficient membership checks and hand updates while reducing memory allocation during inference.

The search algorithm samples from this distribution, allowing the robot to evaluate moves against plausible opponent hands rather than assuming that the hidden tiles are uniformly distributed.

## How to play
When it's your turn, you'll see a `*` next to **human** in the score. You're **human** and it's **robot**.  It'll
look something like this:

```
----------------------------------
human*(0): [5,0] [4,4] [2,1] [3,0] [1,1] [6,3] [5,4]
robot(10): [x,x] [x,x] [x,x] [x,x] [x,x] [x,x]
boneyard: [x,x] [x,x] [x,x] [x,x] [x,x] [x,x] [x,x] [x,x] [x,x] [x,x] [x,x] [x,x] [x,x] [x,x]
tiles: 28
spinner: [5,5]
left:
right:
up:
down:
board score: 10
```

Type `h` for the help menu. Type `m 0 l` to move the 0th piece to the left, for example. Type `d` to draw and
`p` to pass.

After that, it'll be the computer's turn. It can take a while to decide what to play, especially at the beginning
of the game.

The rules are based on the **Fives** version of dominoes. The match ends after round scoring if one player has at least
150 points and leads their opponent. A tie starts another round, even at or above 150. Reaching 150 during play leaves
the current round in progress. Rounds can end in **dominoes** if
a player is able to play all of their tiles. It can also end in a **locked game** if neither player is able to play and
the **boneyard** - the pile of dominoes to draw from - is empty. If a player gets **dominoes**, they start the next round
with the piece of their choice. The first round and any round following a **locked game** starts with the highest double
that either player has. The program will play that piece automatically. If neither player has a double, the round is
restarted after shuffling the pieces.

During a round, players alternate turns, playing one tile per turn. They can play a piece if it connects to the end
of one of parts of the board: the **spinner** or the **left**, **right**, **up** or **down** chain. It connects if either of its numbers
match the number on the outside of the piece its connecting to. The **spinner** is the first double played in a round. It is the only
piece in a round that players can connect to from 4 directions, so ends up influencing play in the round significantly. Before there
is a **spinner**, players can play on both sides of the **left** chain. For example, in a round that has no **spinner** and only
a **left**, where the **left** is `[6,2] - [2,3]`, players could play tiles with at least one 3 to the right or tiles with
at least one 6 to the left. If all 4 directions off of the **spinner** have some tiles, players can no longer play off of the
**spinner**. To see some sample play, [here](https://github.com/alinden/dominoes_bot/blob/main/games_vs_greedy/best_of_11.txt) are some games between the AI and a greedy algorithm playing as human.

If a player doesn't have any legal moves, they have to **draw**. By drawing, they take one tile from the boneyard and
then have a new turn. If the piece they drew doesn't connect to the board, they will have to draw again.

If there are no tiles in the boneyard and a player can't play, they have to **pass**.

When both players **pass** consecutively, the game is locked.

Players can score points during a round by playing their tiles so that the board score is a multiple of 5 after their turn.
For example, if the board score is 25 after your turn, you will score 25 points. If the board score is not a multiple of 5,
neither player gets any points. The board score is the sum of the ends of the chains on the board. With doubles, both numbers
are counted. For other pieces, only the number that can be connected to counts.

At the end of a round that ends in **dominoes**, the player who played all their tiles get the sum of the opponent's tiles,
rounded to the nearest 5.

In a **locked game**, the player with fewer remaining pips receives the difference between the two hands' pip totals,
rounded to the nearest 5. Equal pip totals award neither player points.
