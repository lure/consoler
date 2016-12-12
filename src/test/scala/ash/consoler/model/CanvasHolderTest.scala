package ash.consoler.model

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpecLike, Matchers}

@RunWith(classOf[JUnitRunner])
class CanvasHolderTest extends FunSpecLike with Matchers {

  describe("canvas holder should provide init and reset calls") {
    it("Unitialized holder contain no canvas") {
      intercept[IllegalStateException](new CanvasHolder().canvas)
      new CanvasHolder().canvasOpt shouldBe None
    }

    it("Unitialized holder throws IllegalStateException on reset") {
      intercept[IllegalStateException](new CanvasHolder().reset())
    }

    it("init must create a canvas filled with Default Color") {
      val holder = new CanvasHolder()
      holder.init(5, 10)
      val canvas = holder.canvas
      canvas.forall(_.forall(_ == holder.DefaultColor))
      canvas.length shouldBe 10
      canvas(0).length shouldBe 5
    }

    it("should not recreate canvas on reset call with same specs") {
      val holder = new CanvasHolder()
      holder.init(7, 5)
      val canvas = holder.canvas
      assert(holder.init(7, 5).canvas eq canvas)
    }
  }

  describe("holder must provide utility methods") {
    it("height / width must return initialized canvas height and width") {
      val holder = new CanvasHolder()
      holder.init(10, 15)
      holder.width shouldBe 10
      holder.height shouldBe 15
    }

    it("setColor / getColor must change specified point") {
      val holder = new CanvasHolder()
      holder.init(7, 5)
      holder.getColor(3, 4) shouldBe holder.DefaultColor
      holder.setColor(3, 4, 'b')
      holder.getColor(3, 4) shouldBe 'b'
      val exp = Array.fill[Char](5, 7)(holder.DefaultColor)
      exp(4).update(3, 'b')
      holder.canvas shouldBe exp
    }

    it("height / width must throw exception if canvas has not been initialized") {
      val holder = new CanvasHolder()
      intercept[IllegalStateException](holder.width)
      intercept[IllegalStateException](holder.height)
    }

    it("setColor / getColor must throw exception if canvas has not been initialized or coord is out of bounds") {
      val holder = new CanvasHolder()
      intercept[IllegalStateException](holder.getColor(0, 0))
      intercept[IllegalStateException](holder.setColor(0, 0, 'v'))
      holder.init(1, 1)
      intercept[IllegalArgumentException](holder.getColor(5, 5))
      intercept[IllegalArgumentException](holder.setColor(5, 5, 'v'))

    }
  }
}
