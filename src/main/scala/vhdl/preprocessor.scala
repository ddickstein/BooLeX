package vhdl

import scala.collection.mutable.{HashMap, HashSet}

import library._

// Note that this preprocessor drops positional information, but all error checking has already happened, so this is ok
object Boolex2VhdlPreprocessor {
  import parser.contexts._
  import typechecker.CircuitMetaData

  // List includes VHDL reserved words as well as some words we reserve for translation
  private val reservedWords = Set("abs", "access", "after", "alias", "all", "and", "architecture", "array", "assert",
    "attribute", "begin", "block", "body", "buffer", "bus", "case", "component", "configuration", "constant",
    "disconnect", "downto", "else", "elsif", "end", "entity", "exit", "file", "for", "function", "generate",
    "generic", "group", "guarded", "if", "impure", "in", "inertial", "inout", "integer", "is", "label", "library",
    "linkage", "literal", "loop", "map", "mod", "nand", "new", "next", "nor", "not", "null", "of", "on", "open",
    "or", "others", "out", "package", "port", "postponed", "procedure", "process", "pure", "range", "record",
    "register", "reject", "rem", "report", "reset", "return", "rol", "ror", "select", "severity", "signal", "shared",
    "sla", "sll", "sra", "srl", "subtype", "then", "to", "transport", "type", "unaffected", "units", "until", "use",
    "variable", "wait", "when", "while", "with", "xnor", "xor")

  def preprocess(module: ModuleContext, metaData: CircuitMetaData): (ModuleContext, CircuitMetaData) = {
    val blx2vhdlPreprocessor = new Boolex2VhdlPreprocessorImpl(module, metaData)
    (blx2vhdlPreprocessor.preprocessModule, blx2vhdlPreprocessor.metaData)
  }

  final private class Boolex2VhdlPreprocessorImpl(module: ModuleContext, oldMetaData: CircuitMetaData) {
    private val renameCircuit = getNewCircuitNameMap
    private var renameVariable: Map[String,String] = null // ok b/c will be set before use
    private var tempGenerator: Iterator[String] = null // ok b/c will be set before use
    val metaData = CircuitMetaData(
      dependencyGraph = oldMetaData.dependencyGraph.map({ case (circuit, dependencies) => {
        renameCircuit(circuit) -> dependencies.map(renameCircuit)
      }}),
      circuitSpecs = oldMetaData.circuitSpecs.map({ case (circuit, specs) => renameCircuit(circuit) -> specs }),
      circuitClocksMapOpt = Some(module.circuits.map(circuit => {
        renameCircuit(circuit.name.name) ->
          (circuit.assignments.flatMap(_.values) ++: circuit.output.outputs).flatMap(getClocks).toSet
      }).toMap)
    )

    def preprocessModule: ModuleContext = ModuleContext(module.circuits.map(preprocessCircuit))

    private def preprocessCircuit(circuit: CircuitDeclarationContext): CircuitDeclarationContext = {
      renameVariable = getNewVariableNameMap(circuit)
      tempGenerator = Iterator.from(1).map(x => "temp_%02d".format(x))
      val name = renameCircuit(circuit.name.name)
      val params = circuit.paramsOpt.getOrElse(Nil)
      val dependencies = metaData.dependencyGraph(name)
      val circuitClocksMap = metaData.circuitClocksMapOpt.get
      val dependentClocks = for {
        dependency <- dependencies
        clock <- circuitClocksMap(dependency)
      } yield clock
      val circuitClocks = (dependentClocks ++ circuitClocksMap(name)).toList
        .sortBy(_ * -1)
        .map(period => ClockDivider.approximateClock(period)._2)
      val extraParams = (if (circuitClocks.nonEmpty && name == "Main") {
        List("clk_50MHz", "reset")
      } else {
        circuitClocks
      }).map(Symbol)
      val assignments1 = circuit.assignments.flatMap(preprocessAssignment)
      val (assignments2, outStatement) = preprocessOutStatement(circuit.output)
      CircuitDeclarationContext(
        name = Symbol(name),
        paramsOpt = Some(params ++: extraParams).filter(_.nonEmpty),
        assignments = assignments1 ++: assignments2,
        output = outStatement
      )
    }

    private def preprocessAssignment(assignment: AssignmentContext): Seq[AssignmentContext] = {
      val (newAssignments1, newExpressions) = assignment.values.map(preprocessExpression).flatUnzip
      val replacementMap = HashMap.empty[String, String].withDefault(identity)
      val newAssignments2 = (for {
        (variable, expression) <- assignment.variables.zip(newExpressions)
        newVariable = renameVariable(variable.name)
      } yield {
        if (expression.isInstanceOf[Variable] && expression.asInstanceOf[Variable].name.startsWith("temp_")) {
          replacementMap += expression.asInstanceOf[Variable].name -> newVariable
          None
        } else {
          Some(AssignmentContext(List(Symbol(newVariable)), List(expression)))
        }
      }).flatten
      for {
        newAssignment <- newAssignments1 ++: newAssignments2
        variables = newAssignment.variables.map(sym => Symbol(replacementMap(sym.name)))
        values = newAssignment.values
      } yield AssignmentContext(variables, values)
    }

    private def preprocessOutStatement(outStatement: OutStatementContext): (
      Seq[AssignmentContext],
      OutStatementContext
    ) = {
      val outGenerator = Iterator.from(1).map(x => "out_%02d".format(x))
      val (newAssignments1, newExpressions) = outStatement.outputs.map(preprocessExpression).flatUnzip
      val newOutputs = List.fill(newExpressions.size)(outGenerator.next)
      val newAssignments2 = for {
        (newExpression, newOutput) <- newExpressions.zip(newOutputs)
      } yield {
        AssignmentContext(List(Symbol(newOutput)), List(newExpression))
      }
      (newAssignments1 ++: newAssignments2, OutStatementContext(newOutputs.map(Variable)))
    }

    private def preprocessExpression(expression: ExpressionContext): (
      Seq[AssignmentContext],
      Seq[ExpressionContext]
    ) = expression match {
      case NotExpression(exp) => {
        val (assignments, newExp) = preprocessExpression(exp).mapRight(_.apply(0))
        (assignments, List(NotExpression(newExp)))
      }
      case BufferExpression(exp) => {
        val (assignments, newExp) = preprocessExpression(exp).mapRight(_.apply(0))
        (assignments, List(BufferExpression(newExp)))
      }
      case AndExpression(exp1, exp2) => {
        val (assignments1, newExp1) = preprocessExpression(exp1).mapRight(_.apply(0))
        val (assignments2, newExp2) = preprocessExpression(exp2).mapRight(_.apply(0))
        (assignments1 ++: assignments2, List(AndExpression(newExp1, newExp2)))
      }
      case NandExpression(exp1, exp2) => {
        val (assignments1, newExp1) = preprocessExpression(exp1).mapRight(_.apply(0))
        val (assignments2, newExp2) = preprocessExpression(exp2).mapRight(_.apply(0))
        (assignments1 ++: assignments2, List(NandExpression(newExp1, newExp2)))
      }
      case XorExpression(exp1, exp2) => {
        val (assignments1, newExp1) = preprocessExpression(exp1).mapRight(_.apply(0))
        val (assignments2, newExp2) = preprocessExpression(exp2).mapRight(_.apply(0))
        (assignments1 ++: assignments2, List(XorExpression(newExp1, newExp2)))
      }
      case XnorExpression(exp1, exp2) => {
        val (assignments1, newExp1) = preprocessExpression(exp1).mapRight(_.apply(0))
        val (assignments2, newExp2) = preprocessExpression(exp2).mapRight(_.apply(0))
        (assignments1 ++: assignments2, List(XnorExpression(newExp1, newExp2)))
      }
      case OrExpression(exp1, exp2) => {
        val (assignments1, newExp1) = preprocessExpression(exp1).mapRight(_.apply(0))
        val (assignments2, newExp2) = preprocessExpression(exp2).mapRight(_.apply(0))
        (assignments1 ++: assignments2, List(OrExpression(newExp1, newExp2)))
      }
      case NorExpression(exp1, exp2) => {
        val (assignments1, newExp1) = preprocessExpression(exp1).mapRight(_.apply(0))
        val (assignments2, newExp2) = preprocessExpression(exp2).mapRight(_.apply(0))
        (assignments1 ++: assignments2, List(NorExpression(newExp1, newExp2)))
      }
      case BooleanValue(value: Boolean) => (Nil, List(expression))
      case Variable(name: String) => (Nil, List(Variable(renameVariable(name))))
      case Clock(milliseconds: Number) => (Nil, List(Variable(
        ClockDivider.approximateClock(milliseconds.number.toInt)._2)
      ))
      case CircuitCallContext(name: Symbol, arguments: Seq[ExpressionContext]) => {
        var (assignments, newExpressions) = arguments.map(preprocessExpression).flatUnzip
        val newArguments = for (newExpression <- newExpressions) yield {
          if (newExpression.isInstanceOf[Variable]) {
            newExpression
          } else {
            val temp = tempGenerator.next
            assignments :+= AssignmentContext(List(Symbol(temp)), List(newExpression))
            Variable(temp)
          }
        }
        val variables = List.fill(oldMetaData.circuitSpecs(name.name)._2)(tempGenerator.next)
        assignments :+= AssignmentContext(
          variables = variables.map(Symbol),
          values = List(CircuitCallContext(Symbol(renameCircuit(name.name)), newArguments))
        )
        (assignments, variables.map(Variable))
      }
    }

    private def getClocks(expression: ExpressionContext): Set[Int] = expression match {
      case NotExpression(exp) => getClocks(exp)
      case BufferExpression(exp) => getClocks(exp)
      case AndExpression(exp1, exp2) => getClocks(exp1) ++: getClocks(exp2)
      case NandExpression(exp1, exp2) => getClocks(exp1) ++: getClocks(exp2)
      case XorExpression(exp1, exp2) => getClocks(exp1) ++: getClocks(exp2)
      case XnorExpression(exp1, exp2) => getClocks(exp1) ++: getClocks(exp2)
      case OrExpression(exp1, exp2) => getClocks(exp1) ++: getClocks(exp2)
      case NorExpression(exp1, exp2) => getClocks(exp1) ++: getClocks(exp2)
      case BooleanValue(value: Boolean) => Set.empty[Int]
      case Variable(name: String) => Set.empty[Int]
      case Clock(milliseconds: Number) => Set(milliseconds.number.toInt)
      case CircuitCallContext(name: Symbol, arguments: Seq[ExpressionContext]) => arguments.flatMap(getClocks).toSet
    }

    private def getNewCircuitNameMap: Map[String, String] = {
      val nameGenerator = Iterator.from(1).map(x => "__%02d".format(x))
      val usedNames = HashSet.empty[String]
      (for {
        circuit <- module.circuits
        oldName = circuit.name.name
        baseName = oldName.replace("_","").toLowerCase // remove the underscores
        newName = if (usedNames.contains(baseName) || reservedWords.contains(baseName)) {
          oldName.toCamelCase + nameGenerator.next
        } else {
          oldName.toCamelCase
        }
      } yield {
        usedNames += baseName
        (oldName -> newName)
      }).toMap.withDefault(identity)
    }

    private def getNewVariableNameMap(circuit: CircuitDeclarationContext): Map[String, String] = {
      val nameGenerator = Iterator.from(1).map(x => "__%02d".format(x))
      val baseCircuitNames = module.circuits.map(_.name.name.replace("_","").toLowerCase).toSet
      val usedNames = HashSet.empty[String]
      val params = circuit.paramsOpt.toList.flatten.map(_.name)
      val locals = circuit.assignments.flatMap(_.variables).map(_.name)
      (for {
        oldName <- params ++: locals
        baseName = oldName.replace("_","").toLowerCase // remove the underscores
        baseVarName = baseName.applyIf(name => baseCircuitNames.contains(name))("var_" + _)
        newName = if (usedNames.contains(baseVarName) || reservedWords.contains(baseVarName)) {
          oldName.toMixedCase + nameGenerator.next
        } else {
          oldName.toMixedCase
        }
      } yield {
        usedNames += baseName
        (oldName -> newName)
      }).toMap.withDefault(identity)
    }
  
  }
}
