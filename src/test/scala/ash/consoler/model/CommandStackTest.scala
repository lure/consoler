package ash.consoler.model

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpecLike, Matchers}

/**
  * Let's assume there is user command stack.
  * * First of all, stack is not strictly required. But we want to have the stack for the next reason:
  *
  * 1. No doubt interviewer will want to see undo/redo implementation. So better now than in a hury during interview.
  * 2. Stack allows to replay situation to the very point things get wrong.
  * 3. Stack allows to use ANY renders. Build ones, use anywhere.
  * 4. Stack can be easily serialized to any format.
  * 5. It's a nice occasion to show up. Sort of. Yeah. Anyway.
  *
  * User: Shubert Alexandr 
  * Date: 22.11.2016
  */
@RunWith(classOf[JUnitRunner])
class CommandStackTest extends FunSpecLike with Matchers {

  describe("Command stack must be able to append new commands and return commands") {
    it("stores new command") {
      val cs = CommandStack()
        .pushCommand(Canvas(100, 100)).pushCommand(BucketFill(90, 70, 'C'))
        .pushCommand(BucketFill(90, 50, 'B')).pushCommand(Quit)

      cs.commands should contain theSameElementsInOrderAs List(Canvas(100, 100), BucketFill(90, 70, 'C'), BucketFill(90, 50, 'B'), Quit)
    }

    it("returns empty command list if no command were pushed") {
      CommandStack().commands shouldBe 'Empty
    }

    it("modifying operation on returned list does not affect stack itself") {
      val cs = CommandStack().pushCommand(Canvas(100, 100))
      cs.commands.updated(0, Quit)
      cs.commands shouldBe Vector(Canvas(100, 100))
    }
  }

  describe("Command stack must provide undo/redo opeations") {
    val cs = CommandStack()
    it("tells availability of undo/redo by corresponding calls") {
      cs.hasUndo shouldBe false
      cs.hasRedo shouldBe false

      val csGrid = cs.pushCommand(Canvas(100, 100))
      csGrid.hasUndo shouldBe true
      csGrid.hasRedo shouldBe false

      val csGridUndoed = csGrid.undo
      csGridUndoed.hasUndo shouldBe false
      csGridUndoed.hasRedo shouldBe true
    }

    it("Must travel back and forth by stack with undo/redo") {
      val travel = cs.pushCommand(Canvas(100, 100)).pushCommand(BucketFill(90, 70, 'C'))
        .pushCommand(BucketFill(90, 50, 'B')).pushCommand(Quit)

      val undone = travel.undo
      undone.commands should contain theSameElementsInOrderAs List(Canvas(100, 100), BucketFill(90, 70, 'C'), BucketFill(90, 50, 'B'))
      undone.redo.commands should contain theSameElementsInOrderAs List(Canvas(100, 100), BucketFill(90, 70, 'C'), BucketFill(90, 50, 'B'), Quit)
    }


    it("Allows undo/redo on empty stack, but does nothing") {
      noException should be thrownBy cs.undo
      noException should be thrownBy cs.redo
      cs.undo.commands shouldBe 'Empty
      cs.redo.commands shouldBe 'Empty
    }
  }
}
