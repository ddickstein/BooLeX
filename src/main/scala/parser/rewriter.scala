package parser

import library._

object BoolexParseTreeRewriter {
  import contexts._

  def rewrite(module: ModuleContext): ModuleContext = {
    val nonInputCircuits = (for {
      circuit <- module.circuits
      name = circuit.name.name
      if name != "_"
      if circuit.paramsOpt.forall(_.isEmpty)
      } yield name).toSet
    val blxParseTreeRewriter = new BoolexParseTreeRewriterImpl(nonInputCircuits)
    return blxParseTreeRewriter.rewriteModule(module)
  }

  final private class BoolexParseTreeRewriterImpl(nonInputCircuits: Set[String]) {
    def rewriteModule(module: ModuleContext): ModuleContext = {
      val newModuleContext = ModuleContext(module.circuits.map(rewriteCircuit))
      newModuleContext.setPos(module.pos)
      return newModuleContext
    }

    def rewriteCircuit(circuit: CircuitDeclarationContext): CircuitDeclarationContext = {
      val newCircuit = CircuitDeclarationContext(
        name = circuit.name,
        paramsOpt = circuit.paramsOpt,
        assignments = circuit.assignments.map(rewriteAssignment),
        output = rewriteOutStatement(circuit.output)
      )
      newCircuit.setPos(circuit.pos)
      return newCircuit
    }
    
    def rewriteAssignment(assignment: AssignmentContext): AssignmentContext = {
      val newAssignment = AssignmentContext(assignment.variables, assignment.values.map(rewriteExpression))
      newAssignment.setPos(assignment.pos)
      return newAssignment
    }

    def rewriteOutStatement(outStatement: OutStatementContext): OutStatementContext = {
      val newOutStatement = OutStatementContext(outStatement.outputs.map(rewriteExpression))
      newOutStatement.setPos(outStatement.pos)
      return newOutStatement
    }

    def rewriteExpression(expression: ExpressionContext): ExpressionContext = {
      val newExpression = expression match {
        case NotExpression(exp) => NotExpression(rewriteExpression(exp))
        case BufferExpression(exp) => BufferExpression(rewriteExpression(exp))
        case AndExpression(exp1, exp2) => AndExpression(rewriteExpression(exp1), rewriteExpression(exp2))
        case NandExpression(exp1, exp2) => NandExpression(rewriteExpression(exp1), rewriteExpression(exp2))
        case XorExpression(exp1, exp2) => XorExpression(rewriteExpression(exp1), rewriteExpression(exp2))
        case XnorExpression(exp1, exp2) => XnorExpression(rewriteExpression(exp1), rewriteExpression(exp2))
        case OrExpression(exp1, exp2) => OrExpression(rewriteExpression(exp1), rewriteExpression(exp2))
        case NorExpression(exp1, exp2) => NorExpression(rewriteExpression(exp1), rewriteExpression(exp2))
        case Variable(name) if nonInputCircuits.contains(name) => { // recast variable as circuit call
          val symbol = Symbol(name)
          symbol.setPos(expression.pos)
          CircuitCallContext(symbol, Nil)
        }
        case CircuitCallContext(name, arguments) => CircuitCallContext(name, arguments.map(rewriteExpression))
        case _ => expression
      }
      newExpression.setPos(expression.pos)
      return newExpression
    }
  }
}
