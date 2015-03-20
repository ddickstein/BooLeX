package backend

import java.util.concurrent.ConcurrentSkipListSet
import java.util.concurrent.ConcurrentLinkedQueue

import scala.collection.JavaConverters._

import library._

class CircuitRunner(delayTime: Int, callback: Set[(String, Boolean)] => Unit) {
  private val stabilizer = new Stabilizer[Signal]
  private val waitingDock = new ConcurrentLinkedQueue[Signal] // waiting for the loading dock to clear
  private val loadingDock = new ConcurrentLinkedQueue[Signal] // loaded into the signal queue for firing
  private val signalQueue = new ConcurrentSkipListSet[stabilizer.entry] // stable priority queue
  private var finished = false

  private val loader = new Thread(new Runnable {
    def run {
      while (!Thread.interrupted) {
        CircuitRunner.this.synchronized {
          try {
            while (waitingDock.isEmpty && loadingDock.isEmpty) {
              CircuitRunner.this.wait
            }
          } catch {
            case e: InterruptedException => Thread.currentThread.interrupt // restore interrupted signal
          }
        }
        if (loadingDock.isEmpty) {
          var transferCapacity = 100
          while (!Thread.currentThread.isInterrupted && transferCapacity > 0 && !waitingDock.isEmpty) {
            Option(waitingDock.poll).foreach(loadingDock.add)
            transferCapacity -= 1
          }
        }
        Option(loadingDock.poll).foreach(signal => {
          signalQueue.synchronized {
            debug("Loading " + signal)
            signalQueue.add(signal)
            signalQueue.notifyAll
          }
        })
      }
      debug("Loader: All done!")
    }
  })

  private val launcher = new Thread(new Runnable {
    private val signalSender = new SignalSender {
      def fire(signal: Signal) {
        debug2("*** It's a propagation ***")
        signalQueue.synchronized {
          signalQueue.add(signal)
          signalQueue.notifyAll
        }
      }
    }

    def run {
      while (!Thread.interrupted) {
        signalQueue.synchronized {
          try {
            while (signalQueue.isEmpty) {
              signalQueue.wait
            }
          } catch {
            case e: InterruptedException => Thread.currentThread.interrupt // restore interrupted signal
          }
        }
        val receivers = scala.collection.mutable.Set.empty[(String, Boolean)]
        while (!signalQueue.isEmpty && signalQueue.first.delay == 0) {
          val signal = signalQueue.pollFirst
          debug("Firing " + signal)
          signal.fire(signalSender)
          if (signal.target.isInstanceOf[Socket]) {
            signal.target.asInstanceOf[Socket].idOpt.foreach(name => receivers += (name -> signal.value))
          }
        }
        signalQueue.forEach((signal: stabilizer.entry) => signal.decrement)
        try {
          Thread.sleep(delayTime)
        } catch {
          case e: InterruptedException => Thread.currentThread.interrupt // restore interrupted signal
        } finally {
          callback(receivers.toSet)
        }
      }
      try {
        loader.join
      } catch {
        case e: InterruptedException => e.printStackTrace
      }
      waitingDock.clear
      loadingDock.clear
      signalQueue.clear
      debug("Launcher: All done!")
    }
  })

  def start(inputSockets: Seq[Socket], trueSocketOpt: Option[Socket] = None, falseSocketOpt: Option[Socket] = None) {
    loadingDock.addAll(inputSockets.map(socket => new Signal(socket, false, 0)).asJava) // initialize inputs to false
    trueSocketOpt.foreach(socket => loadingDock.add(new Signal(socket, true, 0)))
    falseSocketOpt.foreach(socket => loadingDock.add(new Signal(socket, false, 0)))
    loader.start
    launcher.start
  }

  def update(socket: Socket, value: Boolean) {
    this.synchronized {
      if (!finished) {
        val signal = new Signal(socket, value, 1)
        debug("Queueing " + signal)
        waitingDock.add(signal)
        this.notifyAll
      }
    }
  }

  def stop {
    this.synchronized {
      finished = true
      loader.interrupt
      launcher.interrupt
    }
  }
}
