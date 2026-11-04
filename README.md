# dominoes_bot
Beats you at dominoes.

## How to run the program
To try it, install [sbt](https://www.scala-sbt.org/) and then run `sbt -J-Xmx4G "run --play_human"`.
This sets the maximum JVM heap size to 4 GB, which is recommended for AI search. The program can run
with less memory, but performance may suffer.

To see how its thinking, use the decision log: `sbt -J-Xmx4G "run --play_human --log-decisions decisions.log"`.
Consider reading through the sample [decisions.log](decisions.log) in the repo to see an example of how the bot thinks
without needing to run a game yourself.

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

The rules are based on the **Fives** version of dominoes. The game is made up of several rounds and ends when one player
has more than 150 points - and more points than their opponent - at the end of a round. Rounds can end in **dominoes** if
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

At the end of a round that ends in a **locked game**, the player with a lower tile count gets the sum of their opponent's tiles minus the sum
of the their tiles, rounded to the nearest 5.
