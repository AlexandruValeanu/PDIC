object IntegralMain {
  def main(args: Array[String]): Unit = {
    def f: Integral.Function = x => x * Math.sin(x) * Math.cos(x * x)
    Integral.time(Integral.sequentialComputation(f, 0, 10000000))
    Integral.time(Integral.parallelComputation(f, 0, 10000000))

    def g: Integral.Function = x => Math.PI * x * Math.sin(x)
    Integral.time(Integral.sequentialComputation(g, 0, 10000000))
    Integral.time(Integral.parallelComputation(g, 0, 10000000))
  }
}
