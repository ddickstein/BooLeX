import backend.{CircuitBuilder, CircuitRunner}
import itreegen.BoolexIRGenerator
import parser.BoolexParser
import typechecker.BoolexTypeChecker

import library._

object Main {
  def main(args: Array[String]) {
    val parseTree = BoolexParser.parse(CircuitDemo.specification)
    val checkedParseTree = parseTree.right.flatMap(module => {
      BoolexTypeChecker.check(module) match {
        case (warnings, Nil) => Right((warnings, module))
        case (warnings, errors) => Left(errors ++: warnings)
      }
    })
    checkedParseTree.left.foreach(_.foreach(errors.printerr))
    checkedParseTree.right.foreach({ case (warnings, module) => {
      warnings.foreach(errors.printerr)
      val builder = new CircuitBuilder()
      val inputSockets = BoolexIRGenerator.generate(builder)(module).inputs
      val runner = new CircuitRunner(100, sockets => {
        println(sockets.map(s => "(" + s._1 + ": " + s._2 + ")").mkString("{ ", ", ", " }"))
      })
      runner.start(inputSockets, Some(builder.trueSocket), Some(builder.falseSocket))
      CircuitDemo.testInputs.foreach(input => {
        debug3("Simulating: " + input)
        input.zipWithIndex.map({ case (value, index) => runner.update(inputSockets(index), value) })
        safeSleep(2000)
      })
      runner.stop
    }})
  } 
}
