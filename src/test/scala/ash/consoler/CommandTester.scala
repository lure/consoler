package ash.consoler

import ash.consoler.model._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpecLike, Matchers}

@RunWith(classOf[JUnitRunner])
class CommandTester extends FunSpecLike with Matchers {

  describe("Commands must modify canvas, integration test") {

    // Integration test must be enough as canvas behaviour is tested.
    it("Integration test") {
      val holder = new CanvasHolder
      Canvas(20, 4).act(holder)
      holder.width shouldBe 20
      holder.height shouldBe 4

      Line(1, 2, 6, 2).act(holder)
      Line(6, 3, 6, 4).act(holder)
      Rectangle(16, 1, 20, 3).act(holder)
      BucketFill(10, 3, 'o').act(holder)
      val expected =
        Array(
          "oooooooooooooooxxxxx".toCharArray,
          "xxxxxxooooooooox   x".toCharArray,
          "     xoooooooooxxxxx".toCharArray,
          "     xoooooooooooooo".toCharArray)
      holder.canvas shouldBe expected
    }

    it("Integration: fill inside a rectangle") {
      val holder = new CanvasHolder
      Canvas(5, 5).act(holder)
      Rectangle(2, 2, 4, 4).act(holder)
      BucketFill(3, 3, 'n').act(holder)
      val expected = Array(
        "     ".toCharArray,
        " xxx ".toCharArray,
        " xnx ".toCharArray,
        " xxx ".toCharArray,
        "     ".toCharArray)
      holder.canvas shouldBe expected
    }

    it("Integration: fill rectangle lines itself") {
      val holder = new CanvasHolder
      Canvas(5, 5).act(holder)
      Rectangle(2, 2, 4, 4).act(holder)
      BucketFill(2, 2, 'n').act(holder)
      val expected = Array(
        "     ".toCharArray,
        " nnn ".toCharArray,
        " n n ".toCharArray,
        " nnn ".toCharArray,
        "     ".toCharArray)
      holder.canvas shouldBe expected
    }

    it("Integration: creation and cleanup") {
      val holder = new CanvasHolder
      Canvas(7, 7).act(holder)
      Line(2, 3, 2, 4).act(holder)
      holder.getColor(1, 2) shouldBe 'x'
      Clear.act(holder)

      holder.canvas.forall(row => row.forall(' ' ==)) shouldBe true
    }
  }
}
