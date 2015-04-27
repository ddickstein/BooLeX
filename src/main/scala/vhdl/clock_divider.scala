package vhdl;

import scala.collection.mutable.SortedSet

import library._

object ClockDivider {
  private val PERIOD_APPROXIMATIONS = List(100, 200, 400, 500, 800, 1000, 2000, 4000, 5000, 8000, 10000, 20000, 40000,
    50000, 100000, 200000, 500000, 1000000)

  def approximateClock(period: Int): (Int, String) = {
    val approxPeriod = PERIOD_APPROXIMATIONS.map(approx => (Math.abs(period - approx), approx)).min._2
    val approxFrequency = 1000000 / approxPeriod
    val approxClock = if (approxFrequency % 1000 == 0) {
      "clk_" + (approxFrequency / 1000) + "Hz"
    } else {
      "clk_" + approxFrequency + "mHz"
    }
    return (approxFrequency, approxClock)
  }
}

class ClockDivider {
  private val clocks = SortedSet.empty[(Int, String)](Ordering.Tuple2(Ordering.Int.reverse, Ordering.String.reverse))

  def addClock(period: Int) = clocks += ClockDivider.approximateClock(period)

  def toVhdlComponent: String = {
    val sb = new StringBuilder
    sb ++= "component ClockDivider is\n"
    sb ++= "  port (\n"
    sb ++= "    clk_50MHz, reset : in std_logic;\n"
    sb ++= clocks.toList.map(_._2).mkString("    ", ", ", " : out std_logic\n")
    sb ++= "  );\n"
    sb ++= "end component;\n\n"
    return sb.toString
  }

  def getPorts: (Seq[String], Seq[String]) = {
    val inputs = List("clk_50MHz", "reset")
    val outputs = clocks.toList.map(_._2)
    return (inputs, outputs)
  }

  def toVhdlEntity: String = {
    val sb = new StringBuilder
    sb ++= "-- The ClockDivider is used to simulate clock events in your circuit.\n"
    sb ++= "-- The frequencies here are approximations of the original specification.\n\n"
    sb ++= "library ieee;\n"
    sb ++= "use ieee.std_logic_1164.all;\n\n"
    sb ++= "entity ClockDivider is\n"
    sb ++= "  port (\n"
    sb ++= "    clk_50MHz, reset : in std_logic;\n"
    sb ++= clocks.toList.map(_._2).mkString("    ", ", ", " : out std_logic\n")
    sb ++= "  );\n"
    sb ++= "end ClockDivider;\n\n"
    sb ++= "architecture ClockDivider_Architecture of ClockDivider is\n"
    sb ++= "  signal clk_10Hz_signal : std_logic;\n"
    sb ++= "  signal clk_10Hz_counter : integer range 0 to 2499999 := 0;\n"
    for {
      (frequency, name) <- (clocks - (10000 -> "clk_10Hz")).toList
      max = 5000 / frequency - 1
    } {
      sb ++= "  signal " + name + "_signal : std_logic;\n"
      if (max > 0) {
        sb ++= "  signal " + name + "_counter : integer range 0 to " + max + " := 0;\n"
      }
    }
    sb ++= "\nbegin\n\n"
    sb ++= divideClock("clk_50MHz", "clk_10Hz", 2499999)
    for {
      (frequency, name) <- clocks.toList
      if frequency < 10000
      max = 5000 / frequency - 1
    } {
      sb ++= divideClock("clk_10Hz", name, max)
    }
    for((_, name) <- clocks.toList) {
      sb ++= "  " + name + " <= " + name + "_signal;\n"
    }
    sb ++= "\nend ClockDivider_Architecture;\n\n"
    return sb.toString
  }

  private def divideClock(inClock: String, outClock: String, counterMax: Int): String = {
    val sb = new StringBuilder
    sb ++= "  proc_" + outClock + " : process (reset, " + inClock + ")\n"
    sb ++= "  begin\n"
    if (counterMax > 0) {
      sb ++= "    if (reset = '1') then\n"
      sb ++= "      " + outClock + "_counter <= 0;\n"
      sb ++= "      " + outClock + "_signal <= '0';\n"
      sb ++= "    elsif rising_edge(" + inClock + ") then\n"
      sb ++= "      if (" + outClock + "_counter = " + counterMax + ") then\n"
      sb ++= "        " + outClock + "_counter <= 0;\n"
      sb ++= "        " + outClock + "_signal <= NOT " + outClock + "_signal;\n"
      sb ++= "      else\n"
      sb ++= "        " + outClock + "_counter <= " + outClock + "_counter + 1;\n"
      sb ++= "      end if;\n"
      sb ++= "    end if;\n"
    } else {
      sb ++= "    if (reset = '1') then\n"
      sb ++= "      " + outClock + "_signal <= '0';\n"
      sb ++= "    elsif rising_edge(" + inClock + ") then\n"
      sb ++= "      " + outClock + "_signal <= NOT " + outClock + "_signal;\n"
      sb ++= "    end if;\n"
    }
    sb ++= "  end process;\n\n"
    return sb.toString
  }
}
