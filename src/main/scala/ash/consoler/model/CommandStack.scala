package ash.consoler.model

/**
  * There is two approach to store graphical information:
  * 1. pure polygon/primitive stack backed by graphical subsystem (OpenGl, DX, WinAPI, etc)
  * 2. command stack deployed to canvas per-command. No intersection recalculation, heave canvas
  * detection algorithm usage. Used in embeds, low-resource systems.
  *
  * User: Shubert Alexandr
  * Date: 22.11.2016
  */
case class CommandStack(private val stackPointer: Int = 0, private val commandStack: Vector[Command] = Vector()) {
  // TODO: Tech debt OOM. Solution: reset stack to last N commands and store it with corresponding canvas state.
  def pushCommand(c: Command): CommandStack = {
    if (stackPointer < commandStack.length)
      this.copy(stackPointer + 1, commandStack.take(stackPointer) :+ c)
    else
      this.copy(stackPointer + 1, commandStack :+ c)
  }

  def undo: CommandStack = {
    if (stackPointer > 0) {
      this.copy(stackPointer - 1)
    } else {
      this
    }
  }

  def redo: CommandStack = {
    if (stackPointer < commandStack.length) {
      this.copy(stackPointer + 1)
    } else {
      this
    }
  }

  /**
    * If there is a number of clients this call should return [+LinearSeq] instead.
    *
    * @return command list adjusted by current undo/redo state
    */
  def commands: Vector[Command] = {
    commandStack.take(stackPointer)
  }

  def hasRedo: Boolean = stackPointer < commandStack.length

  def hasUndo: Boolean = stackPointer > 0
}
