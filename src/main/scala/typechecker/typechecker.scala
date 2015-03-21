package typechecker

import errors.{CompileTimeError, MiscError, TypeError, Warning}
import library._

object BoolexTypeChecker {
  import parser.contexts._
  def check(module: ModuleContext): (Seq[CompileTimeError], Seq[CompileTimeError]) = {
    val blxTypeChecker = new BoolexTypeCheckerImpl()
    val errors = blxTypeChecker.checkModule(module)
    return errors.partition(_.isInstanceOf[Warning])
  }

  final private class BoolexTypeCheckerImpl {
    val scopes = new BoolexScope()

    def checkModule(module: ModuleContext): Seq[CompileTimeError] = {
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
      val noMainError = (!scopes.containsSymbol("main"))
        .optionally(MiscError("Could not find \'main\' circuit")).toList
      if (duplicateCircuitNameErrors.size + noMainError.size > 0) {
        return noMainError ++: duplicateCircuitNameErrors
      } else {
        val dependencyGraph = (for {
          circuit <- module.circuits
          name = circuit.name.name
          dependencies = getCircuitDependencies(circuit)
        } yield (name -> dependencies)).toMap
        val cyclicErrors = checkCyclicDependencies(dependencyGraph)
        if (cyclicErrors.nonEmpty) {
          return cyclicErrors.toList
        } else {
          val circuitsByName = module.circuits.mapBy(_.name.name)
          val errorList = (for {
            name <- topologicalSort(dependencyGraph)
            circuit <- circuitsByName.get(name)
          } yield {
            checkCircuitDeclaration(circuit)
          }).flatten
          return errorList
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
      case BufferExpression(exp) => checkExpression(exp)
      case NotExpression(exp) => checkExpression(exp)
      case AndExpression(exp1, exp2) => checkExpression(exp1) ++: checkExpression(exp2)
      case NandExpression(exp1, exp2) => checkExpression(exp1) ++: checkExpression(exp2)
      case XorExpression(exp1, exp2) => checkExpression(exp1) ++: checkExpression(exp2)
      case XnorExpression(exp1, exp2) => checkExpression(exp1) ++: checkExpression(exp2)
      case OrExpression(exp1, exp2) => checkExpression(exp1) ++: checkExpression(exp2)
      case NorExpression(exp1, exp2) => checkExpression(exp1) ++: checkExpression(exp2)
      case BooleanValue(value) => Nil
      case Variable(name) => if (name == "_") {
        List(TypeError("\'_\' is an invalid expression", Some(expression.pos)))
      } else {
        (for {
          variableType <- scopes.getSymbolType(name)
          if variableType != BooleanType
          if !variableType.isInstanceOf[BooleanPromiseType]
        } yield {
          TypeError("Expected true/false value, but \'" + name + "\' is a circuit", Some(expression.pos))
        }).orElse({
          scopes.addSymbol(name, BooleanPromiseType(expression.pos))
          None
        }).toList
      }
      case Clock(milliseconds) => (try {
        (milliseconds.number.toInt <= 100).optionally(
          MiscError("Clock period must be greater than 100ms", Some(milliseconds.pos))
        )
      } catch {
        case e: NumberFormatException => Some(MiscError(milliseconds.number +
          "ms is an unreasonable amount of time to wait", Some(milliseconds.pos)))
      }).toList
      case CircuitCallContext(name, arguments) => {
        val typeOpt = scopes.getSymbolType(name.name)
        val typeErrors = if (typeOpt.exists(_.isInstanceOf[CircuitType])) {
          val numFormalParameters = typeOpt.map(_.asInstanceOf[CircuitType].inputs).getOrElse(0)
          val numActualParameters = arguments.map(countExpressionOutputs).sum
          (numActualParameters != numFormalParameters).optionally({
            TypeError("Expected " + numFormalParameters + " parameters for " +
              name.name + " but " + numActualParameters + " arguments were found", Some(name.pos))
          }).toList
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

    def topologicalSort[A](dependencyGraph: Map[A, Set[A]]): Seq[A] = {
      val mutableGraph = scala.collection.mutable.Map() ++ (for {
        (node, dependencies) <- dependencyGraph
      } yield {
        val realDependencies = dependencies & dependencyGraph.keys.toSet
        (node -> scala.collection.mutable.Set(realDependencies.toSeq:_*))
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
