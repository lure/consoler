package ash.consoler.model

import ash.consoler.model.DrawingUtils._

import scala.collection.mutable

/**
  * User command hierarchy. The common root allows pattern matching and extension.
  */
sealed trait Command

/**
  * Marker interface, signals that command modifies canvas.
  * There could be other marker interfaces, for example ones that modifies stack (undo / redo)
  */
trait DrawingCommand extends Command {
  /**
    * Project itself on model.
    * Promise is that model and command are totally compatible or command knows how to gracefully fix issues.
    *
    * @param holder canvas model holder.
    * @return
    */
  def act(holder: CanvasHolder): CanvasHolder
}

case class Canvas(x: Int, y: Int) extends DrawingCommand {
  override def act(holder: CanvasHolder): CanvasHolder = {
    holder.init(x, y)
  }
}

case object Quit extends Command

case class Line(x1: Int, y1: Int, x2: Int, y2: Int) extends DrawingCommand {

  override def act(holder: CanvasHolder): CanvasHolder = {
    val (dx1, dy1, dx2, dy2) = orderPointsT(adjustCoordinates(x1, y1, x2, y2))
    val canvas = holder.canvas

    if (dy1 == dy2) {
      val row = canvas(dy1)
      for (i <- dx1 to dx2)
        row.update(i, DEFAULT_COLOR)
    } else {
      for (i <- dy1 to dy2)
        canvas(i).update(dx1, DEFAULT_COLOR)
    }

    holder
  }
}

case class Rectangle(x1: Int, y1: Int, x2: Int, y2: Int) extends DrawingCommand {

  override def act(holder: CanvasHolder): CanvasHolder = {
    def applyLine(nx1: Int, ny1: Int, nx2: Int, ny2: Int): CanvasHolder = {
      val ln = Line(nx1, ny1, nx2, ny2)
      ln.act(holder)
    }

    if (x1 == x2 || y1 == y2) {
      applyLine(x1, y1, x2, y2)
    } else {
      applyLine(x1, y1, x1, y2)
      applyLine(x1, y2, x2, y2)
      applyLine(x2, y2, x2, y1)
      applyLine(x2, y1, x1, y1)
    }
    holder
  }
}

case class BucketFill(x: Int, y: Int, color: Char) extends DrawingCommand {
  override def act(holder: CanvasHolder): CanvasHolder = {

    val (dx, dy, _, _) = adjustCoordinates(x, y)

    if (holder.getColor(dx, dy) != color) {
      val oldColor = holder.getColor(dx, dy)

      def isOldColor(cx: Int, cy: Int): Boolean = holder.getColor(cx, cy) == oldColor

      val pointsToVisit = new mutable.Stack[(Int, Int)]
      pointsToVisit.push(dx -> dy)

      while (pointsToVisit.nonEmpty) {
        val (x, y) = pointsToVisit.pop()
        var y1 = y
        while (y1 >= 0 && isOldColor(x, y1)) y1 -= 1
        y1 += 1
        var spanLeft = false
        var spanRight = false
        while (y1 < holder.height && isOldColor(x, y1)) {
          holder.setColor(x, y1, color)
          if (x > 0 && spanLeft != isOldColor(x - 1, y1)) {
            if (isOldColor(x - 1, y1)) pointsToVisit.push(x - 1 -> y1)
            spanLeft = !spanLeft
          }
          if (x < holder.width - 1 && spanRight != isOldColor(x + 1, y1)) {
            if (isOldColor(x + 1, y1)) pointsToVisit.push(x + 1 -> y1)
            spanRight = !spanRight
          }
          y1 += 1
        }
      }
    }
    holder
  }
}

case object Clear extends DrawingCommand {
  /**
    * Cleans the whole canvas
    *
    * @param holder canvas model holder.
    * @return modified canvas itself
    */
  override def act(holder: CanvasHolder): CanvasHolder = {
    val canvas = holder.canvas
    canvas.foreach { row =>
      row.indices.foreach(i => row.update(i, holder.DefaultColor))
    }
    holder
  }
}

object DrawingUtils {
  val DEFAULT_COLOR: Char = 'x'

  /**
    * Swap cords to have lesser point first
    *
    * @param x1 point1 x
    * @param y1 point1 y
    * @param x2 point2 x
    * @param y2 point2 yy
    * @return tuple of 4, where first 2 values are x,y of first point and latter 2 - x,y of second point.
    */
  def orderPoints(x1: Int, y1: Int, x2: Int, y2: Int): (Int, Int, Int, Int) = {
    if (x1 <= x2 && y1 <= y2) {
      (x1, y1, x2, y2)
    } else {
      (x2, y2, x1, y1)
    }
  }

  /**
    * Since user input starts from 1 and arrays are zero-based, there is some choices:
    * 1. modify commands: plays bad if command stack should be shown to user. Smells bad as we are unable to
    * debug things untouched.
    * 2. make model keep + 1 cell and start drawing from it. Simply worst decision of all.
    * 3. adjust numbers at drawing point. Since short living vars allocated on stack, no gc impact should be seen.
    *
    * @param x1 point1 x
    * @param y1 point1 y
    * @param x2 point2 x
    * @param y2 point2 y
    * @return tuple4 of points decreased by 1
    */
  def adjustCoordinates(x1: Int, y1: Int, x2: Int = 1, y2: Int = 1): (Int, Int, Int, Int) = {
    (x1 - 1, y1 - 1, x2 - 1, y2 - 1)
  }

  val orderPointsT: ((Int, Int, Int, Int)) => (Int, Int, Int, Int) = orderPoints _ tupled
}