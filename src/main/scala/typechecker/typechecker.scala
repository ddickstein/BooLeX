package typechecker

import errors.{CompileTimeError, ErrorList, MiscError, TypeError}
import identity._
import library._

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
        inputs = circuit.paramsOpt.map(_.size).getOrElse(0)
      } yield {
        (!scopes.addSymbol(name, PartialCircuitType(inputs)))
          .optionally(MiscError("Duplicate identifier: \'" + name + "\'", Some(pos)))
      }).flatten
      val noMainError = (!scopes.containsSymbol("main"))
        .optionally(MiscError("Could not find \'main\' circuit.")).toList
      if (duplicateCircuitNameErrors.size + noMainError.size > 0) {
        return NoSuccess(noMainError ++: duplicateCircuitNameErrors)
      } else {
        val dependencyGraph = (for {
          circuit <- module.circuits
          name = circuit.name.name
          dependencies = getCircuitDependencies(circuit)
        } yield (name -> dependencies)).toMap
        val cyclicErrors = checkCyclicDependencies(dependencyGraph)
        if (cyclicErrors.nonEmpty) {
          return NoSuccess(cyclicErrors.toList)
        } else {
          val circuitsByName = module.circuits.mapBy(_.name.name)
          val errorList = getErrors(for {
            name <- topologicalSort(dependencyGraph)
            circuit <- circuitsByName.get(name)
          } yield {
            checkCircuitDeclaration(circuit)
          })
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
          .optionally(MiscError("Duplicate identifier: \'" + formal.name + "\'", Some(formal.pos)))
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
      // TODO(dani): confirm that the circuit is fully defined at this point
      case CircuitCallContext(name, arguments) => scopes.getSymbolType(name.name)
        .map(_.asInstanceOf[CircuitType].outputs)
        .getOrElse(0)
    }

    def getCircuitDependencies(declaration: CircuitDeclarationContext): Set[String] = {
      def _getCircuitDependencies(context: Context): Set[String] = context match {
        case AssignmentContext(_, values) => values.map(_getCircuitDependencies).flatten.toSet
        case OutStatementContext(outputs) => outputs.map(_getCircuitDependencies).flatten.toSet
        case NotExpression(exp) => _getCircuitDependencies(exp)
        case AndExpression(exp1, exp2) => _getCircuitDependencies(exp1) ++ _getCircuitDependencies(exp2)
        case NandExpression(exp1, exp2) => _getCircuitDependencies(exp1) ++ _getCircuitDependencies(exp2)
        case XorExpression(exp1, exp2) => _getCircuitDependencies(exp1) ++ _getCircuitDependencies(exp2)
        case XnorExpression(exp1, exp2) => _getCircuitDependencies(exp1) ++ _getCircuitDependencies(exp2)
        case OrExpression(exp1, exp2) => _getCircuitDependencies(exp1) ++ _getCircuitDependencies(exp2)
        case NorExpression(exp1, exp2) => _getCircuitDependencies(exp1) ++ _getCircuitDependencies(exp2)
        case CircuitCallContext(name, arguments) => arguments.map(_getCircuitDependencies).flatten.toSet + name.name
        case _ => Set.empty[String]
      }
      (declaration.output +: declaration.assignments).map(_getCircuitDependencies).flatten.toSet
    }

    def checkCyclicDependencies[A](dependencyGraph: Map[A, Set[A]]): Option[MiscError] = {
      val mutableGraph = scala.collection.mutable.Map() ++ dependencyGraph
      def _checkCyclicDependencies(node: A, parents: List[A]): (Set[A], Option[MiscError]) = {
        val checked = (for {
          dependency <- mutableGraph.getOrElse(node, Nil)
        } yield {
          if (parents.contains(dependency)) {
            val dependencyError = MiscError("Circular dependency: " +
              (node +: (node +: parents.take(parents.indexOf(dependency) + 1)).reverse).mkString(" -> ")
            )
            return (Set(node), Some(dependencyError))
          } else {
            val (checked, errorOpt) = _checkCyclicDependencies(dependency, node +: parents)
            if (errorOpt.nonEmpty) {
              return (checked + node, errorOpt)
            } else {
              checked
            }
          }
        }).flatten.toSet
        return (checked + node, None)
      }
      while (mutableGraph.nonEmpty) {
        val node = mutableGraph.keys.head
        val (checked, errorOpt) = _checkCyclicDependencies(node, Nil)
        if (errorOpt.nonEmpty) {
          return errorOpt
        } else {
          mutableGraph --= checked  
        }
      }
      return None
    }

    def topologicalSort[A](dependencyGraph: Map[A, Set[A]]): Seq[A] = {
      val mutableGraph = scala.collection.mutable.Map() ++ (for {
        (node, dependencies) <- dependencyGraph
      } yield {
        (node -> scala.collection.mutable.Set(dependencies.toSeq:_*))
      })
      val linearizedDependencies = scala.collection.mutable.ListBuffer.empty[A]
      val independentNodes = scala.collection.mutable.Set.empty[A]
      independentNodes ++= mutableGraph.filter({ case (_, deps) => deps.isEmpty }).keys
      mutableGraph --= independentNodes
      while (independentNodes.nonEmpty) {
        val node = independentNodes.head
        independentNodes -= node
        linearizedDependencies += node
        mutableGraph.values.foreach(_ -= node)
        val newIndependentNodes = mutableGraph.filter({ case (_, deps) => deps.isEmpty }).keys
        mutableGraph --= newIndependentNodes
        independentNodes ++= newIndependentNodes
      }
      return linearizedDependencies.toList
    }
  }
}
