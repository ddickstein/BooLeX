object CircuitDemo {
  val specification = """

circuit main(a)
  b = a and d
  c = b or e
  d = c or e
  e = not a
  out c
end


"""
  
  val testInputs = List(
    List(true), List(false), List(true)
  )
}
