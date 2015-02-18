package typechecker

import errors.{CompileTimeError, TypeError}

sealed abstract class TypeCheckResult[+T]
final case class Success[+T](result: T) extends TypeCheckResult[T]
final case class NoSuccess(msg: String, line: Int, col: Int) extends TypeCheckResult[Nothing]

object BoolexTypeChecker {
  import parser.contexts._
  def check(module: ModuleContext): Either[CompileTimeError, ModuleContext] = {
    val blxTypeChecker = new BoolexTypeCheckerImpl()
    val result = blxTypeChecker.checkModule(module)
    result match {
      case Success(result) => Right(result)
      case NoSuccess(msg, line, col) => Left(TypeError(msg, line, col))
    }
  }

  final private class BoolexTypeCheckerImpl {
    val scopes = new BoolexScope()

    def checkModule(module: ModuleContext): TypeCheckResult[ModuleContext] = {
      //val circuitDeclarations = module.decs
      return Success(module)
    }
  }
}
