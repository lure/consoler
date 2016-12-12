package ash.consoler.model


/**
  * Canvas representation.
  * Hold mutable array of char limited to positive Short. Short was chosen to limit memory consumption. At the moment
  * there is no screen resolution higher than 8K UHD. Short provides 32767 * 32767 even that would require 4gb of memory.
  *
  * Notes: This part leaves me in doubt. BufferedImage provides anything required to fulfill the task, and I have
  * https://github.com/lure/InstagramPrinter/blob/master/src/main/scala/ru/shubert/insta/service/Template.scala
  * written 2 years ago. Updates must be done through gc context and actual state is rendered via gtRGB call.
  *
  * User: Shubert Alexandr
  * Date: 25.11.2016
  */
class CanvasHolder {

  type CanvasType = Array[Array[Char]]
  type CanvasOptionType = Option[CanvasType]
  // TODO: Option!!!
  protected var realCanvas: CanvasOptionType = None
  // space, like " "
  val DefaultColor: Char = 0x20

  private val NoCanvasMsg = "Canvas has not been initialised"
  private val InvalidPointMsg = "Invalid point coordinates"

  /**
    * Creates new canvas if none exists or new canvas differs in size from existing one, resets existing otherwise
    *
    * @param width  x length
    * @param height y lenght
    * @return
    */
  def init(width: Int, height: Int): CanvasHolder = {
    realCanvas = realCanvas
      .withFilter(c => c.length == height && c(0).length == width)
      .map { c =>
        reset()
        c
      }.orElse(
      // this reversion is used to optimize cpu memory. Row access is used more often than column and this will
      // allow cpu to load sequential memory blocks into cache
      Some(Array.fill[Char](height, width)(DefaultColor))
    )
    this
  }

  /**
    * Used in undo/redo operation as that requires stack replays. Also, could ease GC work a bit by not throwing
    * away a number of instances.
    *
    * @return itself
    */
  def reset(): CanvasHolder = {
    realCanvas.foreach { c =>
      for {
        y <- c.indices
        x <- c(0).indices
      } {
        c(y).update(x, DefaultColor)
      }
    }
    realCanvas.getOrElse(throw new IllegalStateException("canvas not exists"))

    this
  }

  def canvas: CanvasType = realCanvas.getOrElse(throw new IllegalStateException(NoCanvasMsg))

  def canvasOpt: CanvasOptionType = realCanvas

  def height: Int = realCanvas.map(_.length).getOrElse(throw new IllegalStateException(NoCanvasMsg))

  def width: Int = realCanvas.map(_ (0).length).getOrElse(throw new IllegalStateException(NoCanvasMsg))

  /**
    * Returns char representation of color for given point. May throw IllegalArgumentException if
    * point is not located on canvas.
    * Note: coords are zero-based. See command source for explanations.
    *
    * @param x x coord
    * @param y y coord
    * @return color
    */
  def getColor(x: Int, y: Int): Char = {
    ifAccessiblePoint(x, y) { cnv =>
      cnv(y)(x)
    }
  }

  /**
    * Set color for given point. May throw IllegalArgumentException if
    * point is not located on canvas.
    * Note: coords are zero-based. See command source for explanations.
    *
    * @param x x coord
    * @param y y coord
    * @param c color
    */
  def setColor(x: Int, y: Int, c: Char): Unit = {
    ifAccessiblePoint(x, y) { cnv =>
      cnv(y).update(x, c)
    }
  }

  /**
    * Executes function f if canvas exists and requested point located on canvas
    *
    * @param x x coord
    * @param y y coord
    * @param f function to execute
    * @tparam T function return type
    * @return outcome of function f
    */
  protected def ifAccessiblePoint[T](x: Int, y: Int)(f: (CanvasType) => T): T = {
    realCanvas.map { c =>
      if (y >= 0 && x >= 0 && x < c(0).length && y < c.length) {
        f(c)
      } else {
        throw new IllegalArgumentException(InvalidPointMsg)
      }
    }.getOrElse(throw new IllegalStateException(NoCanvasMsg))
  }
}
