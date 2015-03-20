package backend

import library._

class Socket(var idOpt: Option[String] = None, private var _value: Boolean = false) extends SignalReceiver {
  private var _targets = scala.collection.mutable.Set.empty[SignalReceiver]
  def value: Boolean = _value
  def targets: Set[SignalReceiver] = _targets.toSet
  def addTarget(target: SignalReceiver): Unit = _targets += target
  def receive(signal: Signal, sender: SignalSender) {
    debug2(this + " received a " + signal.value + " signal")
    _value = signal.value
    debug2("propagating " + signal.value + " to: " + targets.mkString(", "))
    targets.foreach(target => sender.fire(new Signal(target, value, 0)))
  }
  override def toString: String = "(" + idOpt.getOrElse("?") + ": " + value + ")"
}

class ConstantSocket(override val value: Boolean) extends Socket(Some("$" + value.toString.toUpperCase + "$"), value)
