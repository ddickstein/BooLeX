package typechecker

import errors.{CompileTimeError, ErrorList, MiscError, TypeError}
import identity._

sealed abstract class TypeCheckResult[+T]
final case class Success[+T](result: T) extends TypeCheckResult[T]
final case class NoSuccess(errors: Seq[CompileTimeError]) extends TypeCheckResult[Nothing]

object BoolexTypeChecker {
  import parser.contexts._
  def check(module: ModuleContext): Either[CompileTimeError, ModuleContext] = {
    val blxTypeChecker = new BoolexTypeCheckerImpl()
    val result = blxTypeChecker.checkModule(module)
    result match {
      case Success(result) => Right(result)
      case NoSuccess(errors) => Left(ErrorList(errors))
    }
  }

  final private class BoolexTypeCheckerImpl {
    val scopes = new BoolexScope()

    def checkModule(module: ModuleContext): TypeCheckResult[ModuleContext] = {
      val duplicateCircuitNameErrors = (for {
        circuit <- module.circuits
        name = circuit.name.name
        pos = circuit.name.pos
      } yield {
        (!scopes.addSymbol(name)).optionally(MiscError("Duplicate identifier: \'" + name + "\'", pos))
      }).flatten
      val noMainError = (!scopes.containsSymbol("main"))
        .optionally(MiscError("Could not find \'main\' circuit.")).toList
      if (duplicateCircuitNameErrors.size + noMainError.size > 0) {
        return NoSuccess(noMainError ++: duplicateCircuitNameErrors)
      } else {
        val dependencies = Map(for {
          circuit <- module.circuits
          name = circuit.name.name
          dependencies = getCircuitDependencies(circuit)
        } yield (name -> dependencies))
        val cyclicErrors = checkCyclicDependencies(dependencies)
        if (cyclicErrors.size > 0) {
          return NoSuccess(cyclicErrors)
        } else {
          val circuitsByName = module.circuits.groupBy(_.name.name).mapValues(_.)
          
          orderByIndependence(dependencies).flatMap(circuitsByName.get)


          val errorList = getErrors(module.circuits.map(checkCircuitDeclaration))
          return if (errorList.isEmpty) Success(module) else NoSuccess(errorList)
        }
      }
    }

    def checkCircuitDeclaration(declaration: CircuitDeclarationContext): TypeCheckResult[CircuitDeclarationContext] = {
      val name = declaration.name.name
      val formals = declaration.paramsOpt.toList.flatten
      scopes.startScope(name)
      val duplicateParameterNamesErrorOpts = formals.map(formal => {
        (!scopes.addSymbol(formal.name, BooleanType))
          .optionally(MiscError("Duplicate identifier: \'" + formal.name + "\'", formal.pos))
      })
      val assignmentErrors = getErrors(declaration.assignments.map(checkAssignment))
      val outputErrors = getErrors(checkOutStatement(declaration.output))
      scopes.endScope
      val errorList = duplicateParameterNamesErrorOpts.flatten ++: assignmentErrors ++: outputErrors
      return if (errorList.isEmpty) Success(declaration) else NoSuccess(errorList)
    }

    def checkAssignment(assignment: AssignmentContext): TypeCheckResult[AssignmentContext] = {

    }

    def checkOutStatement(outStatement: OutStatementContext): TypeCheckResult[OutStatementContext] = {
      
    }

    def getErrors(result: TypeCheckResult[Context]): Seq[CompileTimeError] = {
      (result.isInstanceOf[NoSuccess]).optionally(result.asInstanceOf[NoSuccess].errors).toList.flatten
    }

    def getErrors(results: Seq[TypeCheckResult[Context]]): Seq[CompileTimeError] = for {
      result <- results
      if result.isInstanceOf[NoSuccess]
      error <- result.asInstanceOf[NoSuccess].errors
    } yield error

    def countExpressionOutputs(exp: ExpressionContext): Int = exp match {
      case NotExpression(exp) => countExpressionOutputs(exp)
      case AndExpression(exp1, exp2) => countExpressionOutputs(exp1) + countExpressionOutputs(exp2)
      case NandExpression(exp1, exp2) => countExpressionOutputs(exp1) + countExpressionOutputs(exp2)
      case XorExpression(exp1, exp2) => countExpressionOutputs(exp1) + countExpressionOutputs(exp2)
      case XnorExpression(exp1, exp2) => countExpressionOutputs(exp1) + countExpressionOutputs(exp2)
      case OrExpression(exp1, exp2) => countExpressionOutputs(exp1) + countExpressionOutputs(exp2)
      case NorExpression(exp1, exp2) => countExpressionOutputs(exp1) + countExpressionOutputs(exp2)
      case BooleanValue(value: Boolean) => 1
      case Variable(name: String) => 1
      case CircuitCallContext(name, arguments) =>
    }
  }
}
