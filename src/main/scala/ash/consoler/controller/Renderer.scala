package ash.consoler.controller

import ash.consoler.model.CanvasHolder

/**
  * Assuming there could other renderers, declare common interface to it.
  */
abstract class Renderer {
  /**
    * Renders image to underlying graphical context
    *
    * @param holder model holder
    */
  def renderImage(holder: CanvasHolder): Unit

  /**
    * Render message to underlying graphical context
    *
    * @param message text to render.
    */
  def renderMsg(message: String): Unit
}

/**
  * Console supplied renderer
  */
class ConsoleRenderer extends Renderer {
  override def renderImage(holder: CanvasHolder): Unit = {
    val canvas = holder.canvas
    val xAxis = canvas(0).length + 2
    println("-" * xAxis)
    for {
      y <- canvas.indices
    } {
      print("|")
      print(canvas(y).mkString(""))
      println("|")
    }
    println("-" * xAxis)
  }

  override def renderMsg(message: String): Unit = {
    println(message)
  }
}
