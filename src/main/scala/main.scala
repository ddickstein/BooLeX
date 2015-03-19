import backend.{CircuitBuilder, EventManager}
import itreegen.BoolexIRGenerator
import parser.BoolexParser
import typechecker.BoolexTypeChecker

object Main {
  def main(args: Array[String]) {
    val specification = scala.io.Source.fromFile("examples/foo.blex").mkString
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
      val builder = new CircuitBuilder()
      val inputSockets = BoolexIRGenerator.generate(builder)(module).inputs
      val eventManager = new EventManager(100, sockets => {
        println(sockets.map(s => "(" + s._1 + ": " + s._2 + ")").mkString("{ ", ", ", " }"))
      })
      eventManager.start(inputSockets, Some(builder.trueSocket), Some(builder.falseSocket))
      eventManager.update(inputSockets(0), true)
      Thread.sleep(5000)
      // eventManager.update(inputSockets(1), true)
      // Thread.sleep(1000)
      // eventManager.update(inputSockets(0), false)
      // eventManager.update(inputSockets(1), false)
      // Thread.sleep(1000)
      eventManager.stop
    }})
  } 
}
