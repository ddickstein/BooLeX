package backend

import library._

class Socket(var idOpt: Option[String] = None, private var _value: Boolean = false) extends SignalReceiver {
  private var _targets = scala.collection.mutable.Set.empty[SignalReceiver]
  def value: Boolean = _value
  def targets: Set[SignalReceiver] = _targets.toSet
  def addTarget(target: SignalReceiver): Unit = _targets += target
  def receive(signal: Signal, propagate: Signal => Unit) {
    if (value != signal.value) {
      _value = signal.value
      debug2("Set " + this)
    }
    targets.foreach(target => propagate(new Signal(target, value, 0)))
  }
  override def toString: String = "(" + idOpt.getOrElse("?") + ": " + value + ")"
}

class ConstantSocket(override val value: Boolean) extends Socket(Some("$" + value.toString.toUpperCase + "$"), value)
