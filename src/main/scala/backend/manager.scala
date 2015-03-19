package backend

import java.util.concurrent.LinkedBlockingQueue
import library._

// object PoisonPill extends Signal(null, false, -1)
object FinishedInitializationPill extends Signal(null, false, Int.MaxValue)

class EventRunner(delayTime: Int, callback: Set[(String, Boolean)] => Unit) extends Runnable {
  private val signals = new LinkedBlockingQueue[Signal]
  private val queue = new SignalQueue(delayTime, callback)
  private var initializing = true

  def run: Unit = {
    try {
      while (!Thread.interrupted) {
        val signal = signals.take
        if (signal == FinishedInitializationPill) {
          initializing = false
        } else {
          debug("Runner: Passing " + signal.value + " -> " + signal.target + " to SignalQueue")
          queue.fire(signal, initializing)
        }
        // else if (signal == PoisonPill) {
        //   throw new InterruptedException()
        // }
      }
    } catch {
      case e: InterruptedException => this.stop
    } finally {
      this.stop
    }
  }

  def update(socket: Socket, value: Boolean): Unit = signals.put(new Signal(socket, value, 1))

  def finishInitialization: Unit = signals.put(FinishedInitializationPill)

  private def stop: Unit = {
    signals.clear
    // signals.put(PoisonPill)
    queue.stop
  }
}

class EventManager(delayTime: Int, callback: Set[(String, Boolean)] => Unit) {
  private val runner = new EventRunner(delayTime, callback)
  private val runnerThread = new Thread(runner)

  def update(socket: Socket, value: Boolean): Unit = runner.update(socket, value)

  def start(inputSockets: Seq[Socket], trueSocketOpt: Option[Socket] = None, falseSocketOpt: Option[Socket] = None) {
    inputSockets.foreach(socket => update(socket, false)) // initialize inputs to false
    trueSocketOpt.foreach(socket => update(socket, true))
    falseSocketOpt.foreach(socket => update(socket, false))
    runner.finishInitialization
    runnerThread.start
  }

  def stop: Unit = runnerThread.interrupt
}
