package typechecker

import parser.contexts._
import errors._

object BoolexTypeChecker {
  def check(module: ModuleContext): Either[CompileTimeError, ModuleContext] = {
    return Right(module)
  }
}
