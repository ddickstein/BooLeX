object CircuitDemo {
  val specification = """

# This RS latch is properly handled.
circuit RSLatch(r,s)
  P = r nor Q
  Q = s nor P
  out P, Q
end

circuit DLatch(d,clk)
  out RSLatch(d' and clk, d and clk)
end

circuit main(d,clk)
  X, Y = DLatch(d,clk)
  out X
end


"""
  
  val testInputs = List(
    List(false, false),
    List(true, false),
    List(true, true),
    List(true, false),
    List(false, false)
  )
}
