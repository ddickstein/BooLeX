package errors {
  sealed abstract class CompileTimeError
  final case class SyntaxError(msg: String, line: Int, col: Int) extends CompileTimeError
  final case class TypeError(msg: String, line: Int, col: Int) extends CompileTimeError
}

package object errors {
  private val ERROR_REGEX = """(\[\d+:\d+\]\s.+?:)(.+)""".r

  def printerr(err: CompileTimeError) {
    def _printerr(line: Int, col: Int, errTyp: String, msg: String) {
      Console.err.println(Console.RED + "[" + line + ":" + col + "] " + errTyp + ": " + Console.RESET + msg)
    }
    err match {
      case SyntaxError(msg, line, col) => _printerr(line, col, "Syntax error", msg)
      case TypeError(msg, line, col) => _printerr(line, col, "Type error", msg)
    }
  }
}
