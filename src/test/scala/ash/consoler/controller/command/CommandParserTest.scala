package ash.consoler.controller.command

import ash.consoler.model._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpecLike, Matchers}

/**
  * Let's assume we want per-token parsing for known commands
  * User: Shubert Alexandr
  * Date: 24.11.2016
  */
@RunWith(classOf[JUnitRunner])
class CommandParserTest extends FunSpecLike with Matchers {

  implicit def strToList(str: String): List[String] = str.split("\\s+").toList

  describe("Canvas parser"){
    val parser = new CanvasParser
    it("should parse string that contains 2 numbers"){
      parser("10 20") shouldBe Right(Canvas(10, 20))
      parser("0 0") shouldBe Right(Canvas(0, 0))
    }

    it ("should signal absent params"){
      parser("") shouldBe Left("Canvas x not found, y not found")
      parser("1 ") shouldBe Left("Canvas 1, y not found")
    }

    it ("should signal invalid params"){
      parser("a b ") shouldBe Left("Canvas x: [a] invalid value, y: [b] invalid value")
      parser("1 b") shouldBe Left("Canvas 1, y: [b] invalid value")
      parser("a 100") shouldBe Left("Canvas x: [a] invalid value, 100")
      parser("a 1001231231231231321") shouldBe Left("Canvas x: [a] invalid value, y: [1001231231231231321] invalid value")
      parser("a 100000") shouldBe Left("Canvas x: [a] invalid value, y: [100000] must be between 0 and 32767")
    }

    it("should ignore spaces") {
      parser("10  20") shouldBe Right(Canvas(10, 20))
    }
  }

  describe("Line parser"){
    val parser = new LineParser
    it("should parse string that contains 2 numbers"){
      parser("10 20 60 70") shouldBe Right(Line(10, 20, 60, 70))
      parser("0 0 0 0") shouldBe Right(Line(0, 0, 0 ,0))
    }

    it ("should signal absent params"){
      parser("") shouldBe Left("Line x1 not found, y1 not found, x2 not found, y2 not found")
      parser("1 ") shouldBe Left("Line 1, y1 not found, x2 not found, y2 not found")
    }

    it ("should signal invalid params"){
      parser("a b c d") shouldBe Left("Line x1: [a] invalid value, y1: [b] invalid value, x2: [c] invalid value, y2: [d] invalid value")
      parser("1 b c d") shouldBe Left("Line 1, y1: [b] invalid value, x2: [c] invalid value, y2: [d] invalid value")
      parser("a 100 20 30") shouldBe Left("Line x1: [a] invalid value, 100, 20, 30")
    }

    it("should ignore spaces") {
      parser("10  20      50 70") shouldBe Right(Line(10, 20, 50, 70))
    }
  }

  describe("Rectangle parser"){
    val parser = new RectangleParser
    it("should parse string that contains 2 numbers"){
      parser("10 20 60 70") shouldBe Right(Rectangle(10, 20, 60, 70))
      parser("0 0 0 0") shouldBe Right(Rectangle(0, 0, 0 ,0))
    }

    it ("should signal absent params"){
      parser("") shouldBe Left("Rectangle x1 not found, y1 not found, x2 not found, y2 not found")
      parser("1 ") shouldBe Left("Rectangle 1, y1 not found, x2 not found, y2 not found")
    }

    it ("should signal invalid params"){
      parser("a b c d") shouldBe Left("Rectangle x1: [a] invalid value, y1: [b] invalid value, x2: [c] invalid value, y2: [d] invalid value")
      parser("1 b c d") shouldBe Left("Rectangle 1, y1: [b] invalid value, x2: [c] invalid value, y2: [d] invalid value")
      parser("a 100 20 30") shouldBe Left("Rectangle x1: [a] invalid value, 100, 20, 30")
    }

    it("should ignore spaces") {
      parser("10  20      50 70") shouldBe Right(Rectangle(10, 20, 50, 70))
    }
  }

  describe("Fill parser"){
    val parser = new BucketParser
    it("should parse string that contains 2 numbers"){
      parser("10 20 c") shouldBe Right(BucketFill(10, 20, 'c'))
    }

    it ("should signal absent params"){
      parser("") shouldBe Left("BucketFill x not found, y not found, color not found")
      parser("1 ") shouldBe Left("BucketFill 1, y not found, color not found")
    }

    it ("should signal invalid params"){
      parser("a b g") shouldBe Left("BucketFill x: [a] invalid value, y: [b] invalid value, g")
      parser("1 2 css") shouldBe Left("BucketFill 1, 2, color: [css] one character expected")
      parser("a 100 ö") shouldBe Left("BucketFill x: [a] invalid value, 100, ö")
    }

    it("should ignore spaces") {
      parser("10  20   c") shouldBe Right(BucketFill(10, 20, 'c'))
    }
  }
}
