# dominoes_bot
Beats you at dominoes.

## How to run the program
To try it, install [sbt](https://www.scala-sbt.org/) and then run `sbt "run --play_human"`.

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

## How the AI works
todo
