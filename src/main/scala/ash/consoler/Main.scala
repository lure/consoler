package ash.consoler

import ash.consoler.consts.StubStringSource
import ash.consoler.controller.{ConsoleRenderer, StateController}
import ash.consoler.ext.CmdParserWithClearance
import ash.consoler.validator.ValidationCenterImpl


object Main extends App {

//  val sc = new StateController(new CmdParserCenterImpl,
  val sc = new StateController(new CmdParserWithClearance,
    ValidationCenterImpl,
    StubStringSource,
    new ConsoleRenderer)

  sc.mainCycle()
}
