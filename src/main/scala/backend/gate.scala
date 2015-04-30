package backend

sealed trait Gate

/*
 * Although it would seem that the case object wrappers are gratuitous and that
 * we could instead represent gates as case classes, the reason the code is
 * designed in this more cumbersome way is to allow pattern matching on the
 * gate type itself, which is done by the connectToGate method in circuit.scala
 *
 * Normally we could pattern match on the gate type by saying GateType(_,_),
 * but while this would be fine for matching, the connectToGate method still
 * requires a gate type parameter, so we would still need some sort of other
 * enum or a set of default objects (one per gate) that we pass to signal the
 * type.  Neither of these alternate approaches seemed organized.
 *
 */

// A factory for buffers
case object Buffer extends Gate {
  def apply(input: Socket, output: Socket) {
    new BufferImpl(input: Socket, output: Socket)
  }

  private class BufferImpl(input: Socket, output: Socket) extends SignalReceiver {
    input.addTarget(this)
    def receive(signal: Signal, propagate: Signal => Unit) {
      propagate(new Signal(output, input.value, 1))
    }
    override def toString: String = "BUFFER[" + input + "] => " + output
  }
}

// A factory for NOT gates
case object NotGate extends Gate {
  def apply(input: Socket, output: Socket) {
    new NotGateImpl(input: Socket, output: Socket)
  }

  private class NotGateImpl(input: Socket, output: Socket) extends SignalReceiver {
    input.addTarget(this)
    def receive(signal: Signal, propagate: Signal => Unit) {
      propagate(new Signal(output, !input.value, 1))
    }
    override def toString: String = "NOT[" + input + "] => " + output
  }
}

// A factory for AND gates
case object AndGate extends Gate {
  def apply(input1: Socket, input2: Socket, output: Socket) {
    new AndGateImpl(input1: Socket, input2: Socket, output: Socket)
  }

  private class AndGateImpl(input1: Socket, input2: Socket, output: Socket) extends SignalReceiver {
    input1.addTarget(this)
    input2.addTarget(this)
    def receive(signal: Signal, propagate: Signal => Unit) {
      propagate(new Signal(output, input1.value && input2.value, 1))
    }
    override def toString: String = "AND[" + input1 + ", " + input2 + "] => " + output
  }
}

// A factory for OR gates
case object OrGate extends Gate {
  def apply(input1: Socket, input2: Socket, output: Socket) {
    new OrGateImpl(input1: Socket, input2: Socket, output: Socket)
  }

  private class OrGateImpl(input1: Socket, input2: Socket, output: Socket) extends SignalReceiver {
    input1.addTarget(this)
    input2.addTarget(this)
    def receive(signal: Signal, propagate: Signal => Unit) {
      propagate(new Signal(output, input1.value || input2.value, 1))
    }
    override def toString: String = "OR[" + input1 + ", " + input2 + "] => " + output
  }
}

// A factory for XOR gates
case object XorGate extends Gate {
  def apply(input1: Socket, input2: Socket, output: Socket) {
    new XorGateImpl(input1: Socket, input2: Socket, output: Socket)
  }

  private class XorGateImpl(input1: Socket, input2: Socket, output: Socket) extends SignalReceiver {
    input1.addTarget(this)
    input2.addTarget(this)
    def receive(signal: Signal, propagate: Signal => Unit) {
      propagate(new Signal(output, input1.value ^ input2.value, 1))
    }
    override def toString: String = "XOR[" + input1 + ", " + input2 + "] => " + output
  }
}

// A factory for NAND gates
case object NandGate extends Gate {
  def apply(input1: Socket, input2: Socket, output: Socket) {
    new NandGateImpl(input1: Socket, input2: Socket, output: Socket)
  }

  private class NandGateImpl(input1: Socket, input2: Socket, output: Socket) extends SignalReceiver {
    input1.addTarget(this)
    input2.addTarget(this)
    def receive(signal: Signal, propagate: Signal => Unit) {
      propagate(new Signal(output, !(input1.value && input2.value), 1))
    }
    override def toString: String = "NAND[" + input1 + ", " + input2 + "] => " + output
  }
}

// A factory for NOR gates
case object NorGate extends Gate {
  def apply(input1: Socket, input2: Socket, output: Socket) {
    new NorGateImpl(input1: Socket, input2: Socket, output: Socket)
  }

  private class NorGateImpl(input1: Socket, input2: Socket, output: Socket) extends SignalReceiver {
    input1.addTarget(this)
    input2.addTarget(this)
    def receive(signal: Signal, propagate: Signal => Unit) {
      propagate(new Signal(output, !(input1.value || input2.value), 1))
    }
    override def toString: String = "NOR[" + input1 + ", " + input2 + "] => " + output
  }
}

// A factory for XNOR gates
case object XnorGate extends Gate {
  def apply(input1: Socket, input2: Socket, output: Socket) {
    new XnorGateImpl(input1: Socket, input2: Socket, output: Socket)
  }

  private class XnorGateImpl(input1: Socket, input2: Socket, output: Socket) extends SignalReceiver {
    input1.addTarget(this)
    input2.addTarget(this)
    def receive(signal: Signal, propagate: Signal => Unit) {
      propagate(new Signal(output, !(input1.value ^ input2.value), 1))
    }
    override def toString: String = "XNOR[" + input1 + ", " + input2 + "] => " + output
  }
}
