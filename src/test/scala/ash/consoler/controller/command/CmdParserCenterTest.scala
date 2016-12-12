package ash.consoler.controller.command

import ash.consoler.consts.StubStringSource
import ash.consoler.model._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpecLike, Matchers}


@RunWith(classOf[JUnitRunner])
class CmdParserCenterTest extends FunSpecLike with Matchers {

  describe("Command matches command line with concrete command. It should") {
    val center: CmdParserCenter = new CmdParserCenterImpl()

    it("return Left with error on empty string") {
      center.parse("") shouldBe StubStringSource.emptyCommandMessage
      center.parse("   ") shouldBe StubStringSource.emptyCommandMessage
    }

    it("return Left with error on unknown command") {
      center.parse("Z") shouldBe StubStringSource.unknownCommand("Z")
    }

    it("Only command name used to cpecify reason. Leading spaces are ignored") {
      center.parse("    rbrtlvkcklwcwe r34 34 3 34") shouldBe StubStringSource.unknownCommand("RBRTLVKCKLWCWE")
    }

    it("commands are case-insensitive") {
      center.parse("Q") shouldBe Right(Quit)
      center.parse("q") shouldBe Right(Quit)
    }

    it("return parse valid Commands") {
      center.parse("c 10 20") shouldBe Right(Canvas(10, 20))
      center.parse("b 10 20 c") shouldBe Right(BucketFill(10, 20, 'c'))
      center.parse("L 10 20 30 40") shouldBe Right(Line(10, 20, 30, 40))
      center.parse("r 10 20 30 40") shouldBe Right(Rectangle(10, 20, 30, 40))
    }

    it("notify about invalid parameters"){
      // single test is enough, see CommandParserTest
      center.parse("c as 9999999999999999999999") shouldBe Left("Canvas x: [as] invalid value, y: [9999999999999999999999] invalid value")
    }
  }
}
