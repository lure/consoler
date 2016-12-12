package ash.consoler.validator

import ash.consoler.consts.{StringSource, StubStringSource}
import ash.consoler.model.{BucketFill, CanvasHolder, Line, Rectangle}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpecLike, Matchers}


/**
  * validators perform additional checks, based on current conditions.
  */
@RunWith(classOf[JUnitRunner])
class ValidatorTest extends FunSpecLike with Matchers {
  val holder = new CanvasHolder
  holder.init(10, 13)
  implicit val res: StringSource = StubStringSource

  describe("Line validator must ") {
    val validator = new LineValidator

    it("return command itself as a passed validation result") {
      val line = Line(2, 7, 7, 7)
      validator.validate(line, holder) shouldBe Right(line)
    }

    it("warn about non-horizontal/vertical coordinates") {
      val line = Line(2, 7, 3, 3)
      validator.validate(line, holder) shouldBe Left(res.neitherHorizonOrVertical)
    }

    it("warn about inappropriate coordinates, picking first invalid pair") {
      validator.validate(Line(2, 17, 7, 17), holder) shouldBe Left(res.invalidPoint(2, 17))
      validator.validate(Line(7, 10, 7, 17), holder) shouldBe Left(res.invalidPoint(7, 17))
      validator.validate(Line(-7, 10, -7, 10), holder) shouldBe Left(res.invalidPoint(-7, 10))
    }
  }

  describe("Rectangle validator must ") {
    val validator = new RectangleValidator

    it("return command itself as a passed validation result") {
      val value = Rectangle(2, 7, 7, 7)
      validator.validate(value, holder) shouldBe Right(value)
    }

    it("warn about inappropriate coordinates, picking first invalid pair") {
      validator.validate(Rectangle(2, 17, 7, 17), holder) shouldBe Left(res.invalidPoint(2, 17))
      validator.validate(Rectangle(7, 10, 7, 17), holder) shouldBe Left(res.invalidPoint(7, 17))
      validator.validate(Rectangle(-7, 10, 1, 5), holder) shouldBe Left(res.invalidPoint(-7, 10))
    }
  }

  describe("BucketFill validator must ") {
    val validator = new BucketFillValidator

    it("return command itself as a passed validation result") {
      val value = BucketFill(2, 7, 'z')
      validator.validate(value, holder) shouldBe Right(value)
    }

    it("warn about inappropriate coordinates, picking first invalid pair") {
      validator.validate(BucketFill(2, 17, 'c'), holder) shouldBe Left(res.invalidPoint(2, 17))
      validator.validate(BucketFill(7, 17, 'c'), holder) shouldBe Left(res.invalidPoint(7, 17))
      validator.validate(BucketFill(-1, -2, 'c'), holder) shouldBe Left(res.invalidPoint(-1, -2))
    }

    it("warn about invalid character as a color") {
      validator.validate(BucketFill(7, 10, 0x17), holder) shouldBe Left(res.invalidColor(0x17))
    }
  }

}
