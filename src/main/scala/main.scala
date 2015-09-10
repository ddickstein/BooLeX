import backend.{CircuitBuilder, CircuitRunner}
import itreegen.BoolexIRGenerator
import parser.{BoolexParseTreeRewriter, BoolexParser}
import typechecker.BoolexTypeChecker
import vhdl.{Boolex2VhdlPreprocessor, Boolex2VhdlTranslator}

import debugger._
import library._

object Main {
  def main(args: Array[String]) {
    val parseTree = BoolexParser.parse(CircuitDemo.specification)
    val rewrittenParseTree = parseTree.right
      .map(BoolexParseTreeRewriter.rewrite)
    val checkedParseTreeAndMetaData = rewrittenParseTree.right
      .flatMap(module => {
        BoolexTypeChecker.check(module) match {
          case ((warnings, Nil), Some(metaData)) =>
            Right((warnings, (module, metaData)))
          case ((warnings, errors), _) => Left(errors ++: warnings)
        }
      })
    checkedParseTreeAndMetaData.left.foreach(_.foreach(errors.printerr))
    checkedParseTreeAndMetaData.right.foreach({
      case (warnings, (module, metaData)) => {
        warnings.foreach(errors.printerr)

        // Translation logic
        val (module2, metaData2) = Boolex2VhdlPreprocessor.preprocess(
          module,
          metaData
        )
        Boolex2VhdlTranslator.translate(module2, metaData2)

        // Simulation logic
        val builder = new CircuitBuilder()
        val inputSockets = BoolexIRGenerator.generate(builder)(module).inputs
        val runner = new CircuitRunner(100, sockets => println(
          sockets
            .map(s => "(" + s._1 + ": " + s._2 + ")")
            .mkString("{ ", ", ", " }")
        ))
        runner.start(
          inputSockets,
          Some(builder.trueSocket),
          Some(builder.falseSocket),
          builder.clocks
        )
        CircuitDemo.testInputs.foreach(input => {
          debug3("Simulating: " + input)
          input.zipWithIndex.map({
            case (value, index) => runner.update(inputSockets(index), value)
          })
          safeSleep(2000)
        })
        runner.stop
      }
    })
  }
}
