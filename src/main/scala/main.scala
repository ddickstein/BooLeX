import parser.BoolexParser
import typechecker.BoolexTypeChecker
import scala.io.Source

object Main {
  def main(args: Array[String]) {
    val specification = Source.fromFile("examples/foo.blex").mkString
    val parseTree = BoolexParser.parse(specification)
    val checkedParseTree = parseTree.right.flatMap(module => {
      BoolexTypeChecker.check(module) match {
        case (warnings, Nil) => Right((warnings, module))
        case (warnings, errors) => Left(errors ++: warnings)
      }
    })
    checkedParseTree.left.foreach(_.foreach(errors.printerr))
    checkedParseTree.right.foreach({ case (warnings, module) => {
      warnings.foreach(errors.printerr)
    }})
  } 
}
