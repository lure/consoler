package ash.consoler.controller

import ash.consoler.consts.StringSource
import ash.consoler.controller.command.CmdParserCenter
import ash.consoler.model._
import ash.consoler.validator.ValidationCenter

import scala.annotation.tailrec

/**
  * Here I want Akka FSM. can't justify bringing full Akka framework for a console utility.
  *
  * User: Shubert Alexandr
  * Date: 25.11.2016
  */
class StateController(cmdSource: CmdParserCenter, validator: ValidationCenter, res: StringSource, renderer: Renderer) {
  protected val canvas: CanvasHolder = new CanvasHolder
  protected val commandStack = CommandStack()

  def mainCycle(): Unit = {
    renderer.renderMsg(res.welcomeMessage)
    while (state(nextCommand)) {}
  }

  def onCommand(command: DrawingCommand): Unit = {
    validator.performValidation(command, canvas) match {
      case Right(cmd) =>
        command.act(canvas)
        commandStack.pushCommand(command)
        renderer.renderImage(canvas)
      case Left(msg) => renderer.renderMsg(msg)
    }
  }

  protected lazy val expectCanvas: PartialFunction[Command, Boolean] = {
    case command: Canvas =>
      onCommand(command)
      state = drawState
      true
  }

  protected lazy val expectPrimitives: PartialFunction[Command, Boolean] = {
    case command: DrawingCommand if !command.isInstanceOf[Canvas] =>
      onCommand(command)
      true
  }

  protected lazy val defaultBehaviour: PartialFunction[Command, Boolean] = {
    case Quit =>
      renderer.renderMsg(res.exitMessage)
      false
    case _ =>
      renderer.renderMsg(res.unexpectedCommand)
      true
  }
  protected lazy val initState: PartialFunction[Command, Boolean] = expectCanvas orElse defaultBehaviour
  protected lazy val drawState: PartialFunction[Command, Boolean] = expectPrimitives orElse defaultBehaviour
  protected var state: PartialFunction[Command, Boolean] = initState

  @tailrec
  protected final def nextCommand: Command = {
    val line = scala.io.StdIn.readLine(res.prompt)
    cmdSource.parse(line) match {
      case Right(c) => c
      case Left(e) =>
        renderer.renderMsg(e)
        nextCommand
    }
  }
}