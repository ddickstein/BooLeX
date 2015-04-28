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
  clk = clock(500)
  X, _ = DLatch(d,clk)
  out X
end

circuit foo
  out clock(2000)
end

circuit bar
  out foo
end

circuit baz
  out bar
end

"""

val testInputs = List(
  List(false),
  List(true),
  List(false)
)}
