package backend

import java.util.concurrent.LinkedBlockingQueue

object PoisonPill extends Signal(null, false, -1)

class EventRunner(delayTime: Int, callback: Set[Socket] => Unit) extends Runnable {
  private val signals = new LinkedBlockingQueue[Signal]
  private val queue = new SignalQueue(delayTime, callback)

  def run: Unit = {
    try {
      while (!Thread.interrupted) {
        val signal = signals.take
        if (signal == PoisonPill) {
          throw new InterruptedException()
        }
        queue.fire(signal)
      }
    } catch {
      case e: InterruptedException => this.stop
    } finally {
      this.stop
    }
  }

  def update(socket: Socket, value: Boolean) {
    signals.put(new Signal(socket, value, 1))
  }

  private def stop: Unit = {
    signals.clear
    signals.put(PoisonPill)
    queue.stop
  }
}

class EventManager(delayTime: Int, callback: Set[Socket] => Unit) {
  private val runner = new EventRunner(delayTime, callback)
  private val runnerThread = new Thread(runner)

  def update(socket: Socket, value: Boolean): Unit = runner.update(socket, value)
  
  def start(trueSocketOpt: Option[Socket] = None, falseSocketOpt: Option[Socket] = None) {
    runnerThread.start
    trueSocketOpt.foreach(socket => update(socket, true))
    falseSocketOpt.foreach(socket => update(socket, false))
  }

  def stop: Unit = runnerThread.interrupt
}
