import scala.util.parsing.input.Position

package errors {
  sealed abstract class CompileTimeError
  final case class SyntaxError(msg: String, posOpt: Option[Position]) extends CompileTimeError
  final case class TypeError(msg: String, posOpt: Option[Position]) extends CompileTimeError
  final case class MiscError(msg: String, posOpt: Option[Position]) extends CompileTimeError
  final case class ErrorList(errors: Seq[CompileTimeError]) extends CompileTimeError
}

package object errors {
  private val ERROR_REGEX = """(\[\d+:\d+\]\s.+?:)(.+)""".r

  def printerr(err: CompileTimeError) {
    def _printerr(errTyp: String, msg: String, posOpt: Option[Position]) {
      val pos = posOpt.map(pos => "[" + pos + "] ").getOrElse("")
      Console.err.println(Console.RED + pos + errTyp + ": " + Console.RESET + msg)
    }

    err match {
      case SyntaxError(msg, posOpt) => _printerr("Syntax error", msg, posOpt)
      case TypeError(msg, posOpt) => _printerr("Type error", msg, posOpt)
      case MiscError(msg, posOpt) => _printerr("Error", msg, posOpt)
      case ErrorList(errors) => errors.foreach(printerr)
    }
  }
}
