package game

import org.scalatest.flatspec.AnyFlatSpec

class BoardSpec extends AnyFlatSpec {

  "An empty Board" should "be created" in {
    // val x = Board.empty()
    val x = Board(
      None,
      List(),
      List(),
      List(),
      List(),
    )
    assert(x.spinner == None)
    assert(x.left == List())
    assert(x.right == List())
    assert(x.up == List())
    assert(x.down == List())
  }

  "An empty Board" should "transition through spinner states" in {
    val empty = Board.empty()
    assert(empty.state() == Empty)

    val spinner = empty.addFirstTile(Domino(6, 6))
    assert(spinner.state() == SpinnerOnly)

    val tee = spinner.addLeft(Domino(6, 3)).get
    assert(tee.state() == Tee)

    val cross = tee.addRight(Domino(6, 4)).get
    assert(cross.state() == Cross)

    assert(empty.isEmpty())
    assert(!cross.isEmpty())
    assert(cross.length() == 3)
  }

  "addRight" should "extend the left chain when there is no spinner" in {
    val board = Board.empty()
      .addFirstTile(Domino(6, 3))

    val result = board.addRight(Domino(3, 2))

    assert(result.isDefined)

    val updated = result.get

    assert(updated.spinner.isEmpty)
    assert(updated.right.isEmpty)
    assert(updated.left.length == 2)
    assert(updated.tiles().toSet == Set(Domino(6, 3), Domino(3, 2)))
    assert(updated.state() == Snake)
    assert(updated.score() == 8)
  }

  "addRight" should "reject incompatible tiles when there is no spinner" in {
    val board = Board.empty()
      .addFirstTile(Domino(6, 3))

    assert(board.addRight(Domino(5, 2)).isEmpty)
  }

  "A Board" should "calculate scores with a spinner" in {
    val spinner = Board.empty().addFirstTile(Domino(5, 5))
    assert(spinner.score() == 10)

    val tee = spinner.addLeft(Domino(5, 2)).get
    assert(tee.score() == 12) // spinner 10 + exposed 2

    val cross = tee.addRight(Domino(5, 3)).get
    assert(cross.score() == 5) // exposed 2 + 3

    val threeBranches = cross.addUp(Domino(5, 4)).get
    assert(threeBranches.score() == 9) // 2 + 3 + 4

    val fourBranches = threeBranches.addDown(Domino(5, 1)).get
    assert(fourBranches.score() == 10) // 2 + 3 + 4 + 1
  }

  "A Board" should "calculate scores for a snake" in {
    val first = Board.empty().addFirstTile(Domino(6, 4))
    assert(first.state() == Snake)
    assert(first.score() == 10)

    val second = first.addLeft(Domino(6, 2)).get
    assert(second.score() == 6) // exposed 2 + 4

    val third = second.addRight(Domino(4, 3)).get
    assert(third.score() == 5) // exposed 2 + 3
  }

  "A Board" should "reject invalid tile connections" in {
    val board = Board.empty().addFirstTile(Domino(6, 6))

    assert(board.addLeft(Domino(5, 3)).isEmpty)
    assert(board.addRight(Domino(5, 3)).isEmpty)
    assert(board.addUp(Domino(5, 3)).isEmpty)
    assert(board.addDown(Domino(5, 3)).isEmpty)

    assert(board.isEmpty() == false)
    assert(board.length() == 1)
  }

  "A Board" should "remain unchanged when a tile is added" in {
    val original = Board.empty().addFirstTile(Domino(6, 6))
    val updated = original.addLeft(Domino(6, 3)).get

    assert(original.left.isEmpty)
    assert(original.length() == 1)
    assert(original.score() == 12)

    assert(updated.left.length == 1)
    assert(updated.length() == 2)
    assert(updated.score() == 15)

    assert(original.contains(Domino(6, 6)))
    assert(!original.contains(Domino(6, 3)))
    assert(updated.contains(Domino(6, 3)))
  }

  "A Board" should "convert a snake into a spinner board" in {
    val first = Board.empty().addFirstTile(Domino(6, 4))
    val snake = first.addRight(Domino(4, 3)).get

    assert(snake.state() == Snake)
    assert(snake.score() == 9)

    val withSpinner = snake.addSpinner(Domino(6, 6), Left).get

    assert(withSpinner.left == snake.left.map(_.backwards()).reverse)
    assert(withSpinner.state() == Tee)
    assert(withSpinner.spinner.get.tile == Domino(6, 6))
    assert(withSpinner.left.length == 2)
    assert(withSpinner.score() == 15) // spinner 12 + exposed 3

    assert(snake.state() == Snake)
  }

  "A Board" should "add a spinner to the right end of a snake" in {
    val first = Board.empty().addFirstTile(Domino(6, 4))
    val snake = first.addRight(Domino(4, 3)).get

    val withSpinner = snake.addSpinner(Domino(3, 3), Right).get

    assert(withSpinner.state() == Tee)
    assert(withSpinner.spinner.get.tile == Domino(3, 3))
    assert(withSpinner.left == snake.left)
    assert(withSpinner.right.isEmpty)
    assert(withSpinner.up.isEmpty)
    assert(withSpinner.down.isEmpty)
    assert(withSpinner.score() == 12) // spinner 6 + exposed 6

    assert(snake.state() == Snake)
  }

  "A Board" should "reject a spinner that cannot connect to the snake" in {
    val first = Board.empty().addFirstTile(Domino(6, 4))
    val snake = first.addRight(Domino(4, 3)).get

    assert(snake.addSpinner(Domino(5, 5), Left).isEmpty)
    assert(snake.addSpinner(Domino(5, 5), Right).isEmpty)
  }

  "A Board" should "reject a second spinner" in {
    val board = Board.empty().addFirstTile(Domino(6, 6))

    assert(board.addSpinner(Domino(5, 5), Left).isEmpty)
  }
}
