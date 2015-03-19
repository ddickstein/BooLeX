package itreegen

import scala.collection.mutable.HashMap

import library._

object BoolexIRGenerator {
  import parser.contexts._
  def generate[T](builder: IRTreeBuilder[T])(module: ModuleContext): T = {
    val blxIRGenerator = new BoolexIRGeneratorImpl(builder, module)
    return blxIRGenerator.generateCircuit(module.circuits.find(_.name.name == "main").get)
  }

  final private class BoolexIRGeneratorImpl[T](builder: IRTreeBuilder[T], module: ModuleContext) {
    val knownCircuits = module.circuits.mapBy(_.name)
    var scopes = List.empty[HashMap[String, T]]
    var inTopLevel = true

    def generateCircuit(circuit: CircuitDeclarationContext): T = {
      scopes +:= HashMap.empty[String, T]
      val inputTree = builder.group(circuit.paramsOpt.toList.flatMap(_.map(param => getOrCreateInScope(param.name))))
      for (assignment <- circuit.assignments) {
        val sourceTree = builder.group(assignment.values.map(generateExpression))
        val destinationTree = builder.group(assignment.variables.map(variable => getOrCreateInScope(variable.name)))
        builder.chain(sourceTree, destinationTree) // we don't need the outputs of this step
      }
      val outputTree = builder.group(circuit.output.outputs.map(generateExpression))
      val circuitTree = builder.blackBox(inputTree, outputTree, inTopLevel)
      scopes = scopes.tail
      return circuitTree
    }

    def generateExpression(expression: ExpressionContext): T = expression match {
      case NotExpression(exp) => builder.not(generateExpression(exp))
      case AndExpression(exp1, exp2) => builder.and(generateExpression(exp1), generateExpression(exp2))
      case NandExpression(exp1, exp2) => builder.nand(generateExpression(exp1), generateExpression(exp2))
      case XorExpression(exp1, exp2) => builder.xor(generateExpression(exp1), generateExpression(exp2))
      case XnorExpression(exp1, exp2) => builder.xnor(generateExpression(exp1), generateExpression(exp2))
      case OrExpression(exp1, exp2) => builder.or(generateExpression(exp1), generateExpression(exp2))
      case NorExpression(exp1, exp2) => builder.nor(generateExpression(exp1), generateExpression(exp2))
      case BooleanValue(value) => builder.newConstNode(value)
      case Variable(name) => getOrCreateInScope(name)
      case CircuitCallContext(name, arguments) => {
        val wasInTopLevel = inTopLevel
        inTopLevel = false
        val argumentsTree = builder.group(arguments.map(generateExpression))
        val circuitTree = generateCircuit(knownCircuits(name))
        inTopLevel = wasInTopLevel
        builder.chain(argumentsTree, circuitTree)
      }
    }

    def getOrCreateInScope(name: String): T = {
      if (!scopes.head.contains(name)) {
        scopes.head += (name -> builder.newNode(inTopLevel.optionally(name)))
      }
      return scopes.head(name);
    }
  }
}
