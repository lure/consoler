package ash.consoler.ext

import ash.consoler.controller.command.{ClearParser, CmdParserCenterImpl}
import ash.consoler.model.Command

class CmdParserWithClearance extends CmdParserCenterImpl {
  val clearParser = new ClearParser

  /**
    * Overridden to provide Clear command
    *
    * @return partial function to apply BEFORE default behaviour.
    */
  override def additionalParse: PartialFunction[List[String], Either[String, Command]] = {
    case "C" :: Nil => clearParser(List())
  }
}