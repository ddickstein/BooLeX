package typechecker

import parser.contexts._

object BoolexTypeChecker {
  def check(module: ModuleContext): Either[String, ModuleContext] = {
    return Right(module)
  }
}
