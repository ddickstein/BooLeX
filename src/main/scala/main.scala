import parser.BoolexParser
import typechecker.BoolexTypeChecker
import scala.io.Source

object Main {
  def main(args: Array[String]) {
    val specification = Source.fromFile("examples/foo.blex").mkString
    val parseTree = BoolexParser.parse(specification)
    val checkedParseTree = parseTree.right.flatMap(BoolexTypeChecker.check)
    checkedParseTree.left.foreach(errors.printerr)
    checkedParseTree.right.foreach(_ => println("Success!"))
    checkedParseTree.right.foreach(println)
  } 
}