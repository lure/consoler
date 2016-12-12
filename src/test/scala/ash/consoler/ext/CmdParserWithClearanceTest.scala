package ash.consoler.ext

import ash.consoler.model.Clear
import org.junit.runner.RunWith
import org.scalatest.{FunSpec, Matchers}
import org.scalatest.junit.JUnitRunner

/**
  *
  */
@RunWith(classOf[JUnitRunner])
class CmdParserWithClearanceTest extends FunSpec with Matchers{

  describe("CmdParserWithClearance provides additional command parsings"){
    it("should parse single capital 'C' as a Clear command"){
      val cpwc = new CmdParserWithClearance

      cpwc.parse("C") shouldBe Right(Clear)
      cpwc.parse("c") shouldBe Left("Canvas x not found, y not found")
    }
  }
}
