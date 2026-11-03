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

}
