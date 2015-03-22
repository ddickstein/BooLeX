object CircuitDemo { val specification = """
# This RS latch is properly handled.
circuit RSLatch(r,s)
  P = r nor Q
  Q = s nor P
  out P, Q
end

circuit DLatch(d,clk)
  out RSLatch(d' and clk, d and clk)
end

circuit main(d)
  clk = foobar
  X, _ = DLatch(d,clk)
  out X
end

circuit foobar
  out clock(500)
end
"""

val testInputs = List(
  List(false),
  List(true),
  List(false)
)}
