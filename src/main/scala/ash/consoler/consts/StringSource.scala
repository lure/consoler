package ash.consoler.consts

/**
  * This is a stub. In a real world app this must be some sort of proxy available via injection or lookup which
  * provides localized strings. So many things to do, so little time.
  */
trait StringSource {
  def emptyCommandMessage: Left[String, Nothing]
  def unknownCommand(cmd: String): Left[String, Nothing]
  def exitMessage: String
  def unexpectedCommand: String
  def welcomeMessage: String
  def initPrompt: String
  def prompt: String
  def invalidPoint(x: Int, y: Int): String
  def invalidColor(c: Char): String
  def neitherHorizonOrVertical: String
  def unknownCommand: String
  def coordMustBeBetween: String
}

object StubStringSource extends StringSource{
  val drawCommand = "L x1 y1 x2 y2 | R x1 y1 x2 y2 | G x y | B x1 y1 c |Q"
  val canvasCommand = "C x y | Q"

  val DefaultCommandList: String = "Try one of " + canvasCommand + " | " +drawCommand
  val emptyCommandMessage = Left("Empty command. " + DefaultCommandList)
  def unknownCommand(cmd: String) = Left(s"Unknown command [$cmd]. " + DefaultCommandList)
  val exitMessage = "Exiting, bye."
  val unexpectedCommand = "Unexpected command."
  val initPrompt: String = "Please, define canvas or quit "+ canvasCommand
  val drawPrompt: String = "Please, use one of draw commands or quit: " + drawCommand
  val welcomeMessage: String = "Hello.\n" + initPrompt
  val prompt = "enter command: "
  def invalidPoint(x: Int, y: Int): String = "Invalid coordinates %d, %d".format(x, y)
  def invalidColor(c: Char): String = "Invalid color '" + c + "'"
  val neitherHorizonOrVertical = "Line is neither horizontal or vertical"
  val unknownCommand = "Unknown command "
  val coordMustBeBetween: String = "Values must be between 0 and "+Short.MaxValue
}
