package typechecker

import errors.{CompileTimeError, MiscError, TypeError, Warning}
import library._

object BoolexTypeChecker {
  import parser.contexts._
  def check(module: ModuleContext): (
    (Seq[CompileTimeError], Seq[CompileTimeError]),
    Option[CircuitMetaData]
  ) = {
    val blxTypeChecker = new BoolexTypeCheckerImpl()
    val (errors, dependencyGraphOpt) = blxTypeChecker.checkModule(module)
    return (errors.partition(_.isInstanceOf[Warning]), dependencyGraphOpt)
  }

  final private class BoolexTypeCheckerImpl {
    val scopes = new BoolexScope()

    def checkModule(module: ModuleContext): (Seq[CompileTimeError], Option[CircuitMetaData]) = {
      val duplicateCircuitNameErrors = (for {
        circuit <- module.circuits
        name = circuit.name.name
        if name != "_" // we ignore this circuit for now and deal with it later
        pos = circuit.name.pos
        inputs = circuit.paramsOpt.map(_.size).getOrElse(0)
      } yield {
        (!scopes.addSymbol(name, PartialCircuitType(inputs)))
          .optionally(MiscError("Duplicate identifier: \'" + name + "\'", Some(pos)))
      }).flatten
      val noMainError = (!scopes.containsSymbol("main")).optionalList(MiscError("Could not find \'main\' circuit"))
      if (duplicateCircuitNameErrors.size + noMainError.size > 0) {
        return (noMainError ++: duplicateCircuitNameErrors, None)
      } else {
        val dependencyGraph = (for {
          circuit <- module.circuits
          name = circuit.name.name
          dependencies = getCircuitDependencies(circuit)
        } yield (name -> dependencies)).toMap
        val cyclicErrors = checkCyclicDependencies(dependencyGraph)
        if (cyclicErrors.nonEmpty) {
          return (cyclicErrors.toList, None)
        } else {
          val circuitsByName = module.circuits.mapBy(_.name.name)
          val errorList = (for {
            name <- dependencyGraph.sortTopologically
            circuit <- circuitsByName.get(name)
          } yield {
            checkCircuitDeclaration(circuit)
          }).flatten
          // at this point, type checking has concluded
          val circuitSpecs = (for {
            circuit <- module.circuits
            name = circuit.name.name
            typeData <- scopes.getSymbolType(name).map(_.asInstanceOf[CircuitType])
          } yield {
            name -> (typeData.inputs, typeData.outputs)
          }).toMap
          return (errorList, Some(CircuitMetaData(dependencyGraph, circuitSpecs)))
        }
      }
    }

    def checkCircuitDeclaration(declaration: CircuitDeclarationContext): Seq[CompileTimeError] = {
      val circuitName = declaration.name.name
      val formals = declaration.paramsOpt.toList.flatten
      val assignments = declaration.assignments
      val output = declaration.output
      val nameErrors = (if (circuitName == "_") {
        List(TypeError("\'_\' is an invalid circuit name", Some(declaration.name.pos)))
      } else {
        Nil
      }) ++: (for {
        formal <- formals
        if formal.name == "_"
      } yield {
        TypeError("\'_\' is an invalid name for a formal parameter", Some(formal.pos))
      })
      scopes.startScope(circuitName)
      val duplicateParameterNamesErrors = for {
        formal <- formals
        if formal.name != "_"
        if !scopes.addSymbol(formal.name, BooleanType)
      } yield {
        MiscError("Duplicate identifier: \'" + formal.name + "\'", Some(formal.pos))
      }
      val assignmentErrors = assignments.flatMap(checkAssignment)
      val outputErrors = checkOutStatement(output)
      val promiseErrors = for {
        (variable, pos) <- scopes.getPersonalPromises
      } yield {
        MiscError("Unfulfilled promise: \'" + variable + "\'", Some(pos))
      }
      scopes.endScope
      val allErrors = nameErrors ++: duplicateParameterNamesErrors ++: assignmentErrors ++: promiseErrors ++: outputErrors
      val warnings = if (allErrors.isEmpty) {
        val outputDependencyGraph = {
          val inputDependencies = List("%input%" -> Set.empty[String])
          val formalDependencies = formals.map(_.name -> Set("%input%"))
          val localDependencies = for {
            assignment <- assignments
            dependencies = assignment.values.flatMap(getVariablesInExpression).toSet
            variable <- assignment.variables.map(_.name)
            if variable != "_"
          } yield {
            variable -> dependencies
          }
          val outputDependencies = List(("%output%" -> output.outputs.flatMap(getVariablesInExpression).toSet))
          (inputDependencies ++: formalDependencies ++: localDependencies ++: outputDependencies).toMap
        }
        val allVariablesByName = (formals ++: assignments.flatMap(_.variables).filter(_.name != "_")).mapBy(_.name)
        val inputDependencyGraph = outputDependencyGraph.invert
        val variablesAffectedByInput = findAllDependencies(inputDependencyGraph, "%input%")
        val variablesAffectingOutput = findAllDependencies(outputDependencyGraph, "%output%")
        val variablesNotAffectedByInput = allVariablesByName.keys.toSet -- variablesAffectedByInput
        val variablesNotAffectingOutput = allVariablesByName.keys.toSet -- variablesAffectingOutput
        (for (name <- variablesNotAffectedByInput) yield {
          Warning("Variable \'" + name + "\' is not affected by the input to circuit \'" + circuitName + "\'",
            allVariablesByName.get(name).map(_.pos))
        }) ++: (for (name <- variablesNotAffectingOutput) yield {
          Warning("Variable \'" + name + "\' does not affect the output of circuit \'" + circuitName + "\'",
            allVariablesByName.get(name).map(_.pos))
        }).toList
      } else {
        Nil
      }
      return allErrors ++: warnings
    }

    def checkAssignment(assignment: AssignmentContext): Seq[CompileTimeError] = {
      val variables = assignment.variables
      val values = assignment.values
      val outputs = values.flatMap(value => List.fill(countExpressionOutputs(value))(value))
      val assignmentErrors = (if (variables.size > outputs.size) {
        val variable = variables(outputs.size)
        Some(MiscError("Missing value for variable \'" + variable.name + "\'", Some(variable.pos)))
      } else if (outputs.size > variables.size) {
        val value = outputs(variables.size)
        Some(MiscError(
          "Missing variable for expression. Be sure to account for all expression outputs.",
          Some(value.pos)
        ))
      } else {
        None
      }).toList
      val duplicateIdentifierErrors = (for {
        variable <- variables
        if variable.name != "_" // duplicate _'s allowed
      } yield {
        (!scopes.fillPromise(variable.name))
          .optionally(MiscError("Duplicate identifier: \'" + variable.name + "\'", Some(variable.pos)))
      }).flatten
      val expressionErrors = (for {
        value <- values
      } yield {
        checkExpression(value)
      }).flatten
      return assignmentErrors ++: duplicateIdentifierErrors ++: expressionErrors
    }

    def checkOutStatement(outStatement: OutStatementContext): Seq[CompileTimeError] = {
      val currentCircuit = scopes.getOwner
      val outputs = outStatement.outputs
      if (currentCircuit != "_") {
        scopes.completeCircuit(currentCircuit, outputs.map(countExpressionOutputs).sum)
      }
      return outputs.flatMap(checkExpression)
    }

    def checkExpression(expression: ExpressionContext): Seq[CompileTimeError] = expression match {
      case BufferExpression(exp) => checkUnaryExpression(exp)
      case NotExpression(exp) => checkUnaryExpression(exp)
      case AndExpression(exp1, exp2) => checkUnaryExpression(exp1) ++: checkUnaryExpression(exp2)
      case NandExpression(exp1, exp2) => checkUnaryExpression(exp1) ++: checkUnaryExpression(exp2)
      case XorExpression(exp1, exp2) => checkUnaryExpression(exp1) ++: checkUnaryExpression(exp2)
      case XnorExpression(exp1, exp2) => checkUnaryExpression(exp1) ++: checkUnaryExpression(exp2)
      case OrExpression(exp1, exp2) => checkUnaryExpression(exp1) ++: checkUnaryExpression(exp2)
      case NorExpression(exp1, exp2) => checkUnaryExpression(exp1) ++: checkUnaryExpression(exp2)
      case BooleanValue(value) => Nil
      case Variable(name) => if (name == "_") {
        List(TypeError("\'_\' is an invalid expression", Some(expression.pos)))
      } else {
        (for {
          variableType <- scopes.getSymbolType(name)
          if variableType != BooleanType
          if !variableType.isInstanceOf[BooleanPromiseType]
        } yield {
          TypeError("\'" + name + "\' is not a true/false value", Some(expression.pos))
        }).orElse({
          scopes.addSymbol(name, BooleanPromiseType(expression.pos))
          None
        }).toList
      }
      case Clock(milliseconds) => if (milliseconds.number.toInt < 100) {
        List(MiscError("Clock period must be greater than 100ms", Some(milliseconds.pos)))
      } else if (milliseconds.number.toInt > 1000000) {
        List(MiscError("Clock period must be less than 5,000,000ms", Some(milliseconds.pos)))
      } else {
        Nil
      }
      case CircuitCallContext(name, arguments) => {
        val typeOpt = scopes.getSymbolType(name.name)
        val typeErrors = if (typeOpt.exists(_.isInstanceOf[CircuitType])) {
          val numFormalParameters = typeOpt.map(_.asInstanceOf[CircuitType].inputs).getOrElse(0)
          val numActualParameters = arguments.map(countExpressionOutputs).sum
          (numActualParameters != numFormalParameters).optionalList({
            TypeError("Expected " + numFormalParameters + " parameters for " +
              name.name + " but " + numActualParameters + " arguments were found", Some(name.pos))
          })
        } else {
          List(TypeError("\'" + name.name + "\' is not a circuit", Some(name.pos)))
        }
        val argumentErrors = (for {
          argument <- arguments
        } yield {
          checkExpression(argument)
        }).flatten
        return typeErrors ++: argumentErrors
      }
    }

    def checkUnaryExpression(expression: ExpressionContext): Seq[CompileTimeError] = {
      val outputs = countExpressionOutputs(expression)
      return (outputs > 1).optionalList({
        TypeError("Expected expression with 1 output but found expression with " + outputs, Some(expression.pos))
      }) ++: checkExpression(expression)
    }

    def countExpressionOutputs(exp: ExpressionContext): Int = exp match {
      case CircuitCallContext(name, arguments) => scopes.getSymbolType(name.name)
        .map(_.asInstanceOf[CircuitType].outputs)
        .getOrElse(0)
      case _ => 1
    }

    def getVariablesInExpression(exp: ExpressionContext): Seq[String] = exp match {
      case BufferExpression(exp) => getVariablesInExpression(exp)
      case NotExpression(exp) => getVariablesInExpression(exp)
      case AndExpression(exp1, exp2) => getVariablesInExpression(exp1) ++: getVariablesInExpression(exp2)
      case NandExpression(exp1, exp2) => getVariablesInExpression(exp1) ++: getVariablesInExpression(exp2)
      case XorExpression(exp1, exp2) => getVariablesInExpression(exp1) ++: getVariablesInExpression(exp2)
      case XnorExpression(exp1, exp2) => getVariablesInExpression(exp1) ++: getVariablesInExpression(exp2)
      case OrExpression(exp1, exp2) => getVariablesInExpression(exp1) ++: getVariablesInExpression(exp2)
      case NorExpression(exp1, exp2) => getVariablesInExpression(exp1) ++: getVariablesInExpression(exp2)
      case BooleanValue(value) => Nil
      case Variable(name) => List(name)
      case Clock(milliseconds) => List("%input%")
      case CircuitCallContext(_, arguments) => arguments.flatMap(getVariablesInExpression)
    }

    def getCircuitDependencies(declaration: CircuitDeclarationContext): Set[String] = {
      def _getCircuitDependencies(context: Context): Set[String] = context match {
        case AssignmentContext(_, values) => values.map(_getCircuitDependencies).flatten.toSet
        case OutStatementContext(outputs) => outputs.map(_getCircuitDependencies).flatten.toSet
        case BufferExpression(exp) => _getCircuitDependencies(exp)
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
            val cyclicParents = parents.take(parents.indexOf(dependency) + 1)
            val dependencyError = MiscError("Circular dependency: " +
              (node +: (cyclicParents.applyIf(!_.contains(node))(node +: _)).reverse).mkString(" -> ")
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

    def findAllDependencies[A](dependencyGraph: Map[A, Set[A]], node: A): Set[A] = {
      val dependencies = scala.collection.mutable.Set.empty[A]
      val nodesToSearch = scala.collection.mutable.Queue(node)
      while (nodesToSearch.nonEmpty) {
        val next = nodesToSearch.dequeue
        if (!dependencies.contains(next)) {
          dependencies += next
          nodesToSearch ++= dependencyGraph.getOrElse(next, Nil)
        }
      }
      dependencies -= node
      return dependencies.toSet
    }
  }
}
