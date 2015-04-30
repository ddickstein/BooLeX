package backend

import library._
import itreegen.IRTreeBuilder

/**
 * The Circuit class is a wrapper for sockets and gates that have been
 * assembled and linked together for a given sub-tree of the AST.  As the AST
 * is interpreted from the leaves upward, circuits from each sub-tree are
 * joined together to create larger circuits, and at the end after the entire
 * tree has been processed a circuit object representing the entire connected
 * circuit is generated.
 */
case class Circuit(val inputs: Seq[Socket], val outputs: Seq[Socket]) {
  def this(socket: Socket) = this(List(socket), List(socket))
  def this(socketIdOpt: Option[String]) = this(new Socket(socketIdOpt))
}

/**
 * The CircuitBuilder class contains the construction methods for merging
 * circuits generated from the sub-trees of the AST into larger circuits.
 */
class CircuitBuilder extends IRTreeBuilder[Circuit] {
  private val constantSockets = Map(true -> new ConstantSocket(true), false -> new ConstantSocket(false))
  private val _clocks = scala.collection.mutable.ListBuffer.empty[(Int, Socket)]

  def trueSocket: Socket = constantSockets(true)   // a socket that represents a constant 1 signal
  def falseSocket: Socket = constantSockets(false) // a socket that represents a constant 0 signal
  def clocks: Seq[(Int, Socket)] = _clocks.toList  // get the list of clocked inputs in this circuit

  // generate a new circuit of 1 socket
  def newNode(socketIdOpt: Option[String]): Circuit = new Circuit(socketIdOpt)
  
  // generate a new circuit from one of the constant sockets
  def newConstNode(value: Boolean): Circuit = new Circuit(constantSockets(value))
  
  // generate a circuit from a clocked input
  def clock(milliseconds: Int): Circuit = {
    val socket = new Socket
    _clocks += (milliseconds -> socket)
    return new Circuit(socket)
  }

  // direct the output of this circuit to a buffer
  def buffer(circuit: Circuit): Circuit = connectToGate(Buffer, circuit)
  
  // direct the output of this circuit to a NOT gate
  def not(circuit: Circuit): Circuit = connectToGate(NotGate, circuit)
  
  // direct the output of this circuit to an AND gate
  def and(circuit1: Circuit, circuit2: Circuit): Circuit = connectToGate(AndGate, circuit1, circuit2)
  
  // direct the output of this circuit to an OR gate
  def or(circuit1: Circuit, circuit2: Circuit): Circuit = connectToGate(OrGate, circuit1, circuit2)
  
  // direct the output of this circuit to an XOR gate
  def xor(circuit1: Circuit, circuit2: Circuit): Circuit = connectToGate(XorGate, circuit1, circuit2)
  
  // direct the output of this circuit to a NAND gate
  def nand(circuit1: Circuit, circuit2: Circuit): Circuit = connectToGate(NandGate, circuit1, circuit2)
  
  // direct the output of this circuit to a NOR gate
  def nor(circuit1: Circuit, circuit2: Circuit): Circuit = connectToGate(NorGate, circuit1, circuit2)
  
  // direct the output of this circuit to an XNOR gate
  def xnor(circuit1: Circuit, circuit2: Circuit): Circuit = connectToGate(XnorGate, circuit1, circuit2)
  
  // draw a larger circuit around a set of individual circuits
  def group(circuits: Seq[Circuit]): Circuit = {
    val inputs = circuits.flatMap(_.inputs)
    val outputs = circuits.flatMap(_.outputs)
    return Circuit(inputs, outputs)
  }

  // chain together two circuits by driving the input of the second by the
  // output of the first
  def chain(circuit1: Circuit, circuit2: Circuit): Circuit = {
    for ((output1, input2) <- circuit1.outputs.zip(circuit2.inputs)) {
      output1.addTarget(input2)
    }
    return Circuit(circuit1.inputs, circuit2.outputs)
  }
  
  // draw a large circuit that considers the inputs of the first circuit to be
  // its inputs and the outputs of the second circuit to be its outputs.
  // 
  // NOTE: This method assumes that there already exists some connection
  // between these two circuits such that the output of the latter is driven
  // in some way by the input of the former.
  def blackBox(input: Circuit, output: Circuit, inTopLevel: Boolean): Circuit = {
    if (inTopLevel) {
      for {
        (socket, index) <- output.outputs.zipWithIndex
        if socket.idOpt.isEmpty
      } {
        socket.idOpt = Some("%o" + (index+1))
      }
    }
    return Circuit(input.inputs, output.outputs)
  }

  // connect the outputs of the circuits to the inputs of the gate
  private def connectToGate(gate: Gate, circuits: Circuit*): Circuit = {
    val destination = new Socket()
    gate match {
      case Buffer => Buffer(circuits(0).outputs(0), destination)
      case NotGate => NotGate(circuits(0).outputs(0), destination)
      case AndGate => AndGate(circuits(0).outputs(0), circuits(1).outputs(0), destination)
      case OrGate => OrGate(circuits(0).outputs(0), circuits(1).outputs(0), destination)
      case XorGate => XorGate(circuits(0).outputs(0), circuits(1).outputs(0), destination)
      case NandGate => NandGate(circuits(0).outputs(0), circuits(1).outputs(0), destination)
      case NorGate => NorGate(circuits(0).outputs(0), circuits(1).outputs(0), destination)
      case XnorGate => XnorGate(circuits(0).outputs(0), circuits(1).outputs(0), destination)
    }
    return Circuit(circuits.flatMap(_.inputs), List(destination))
  }
}
