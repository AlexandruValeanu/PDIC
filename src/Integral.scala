import io.threadcso._

/*
  farm - architectural pattern for data parallelism

  * Split the interval [a, b] into numberWorkers equal size ranges;
  * Have numberWorkers Worker processes (one per range);
  * A controller process tells each worker which range to work on;
  * Each worker runs the trapezium rule (sequential code) over its range,
  * and returns the result to the controller;
  * The controller sums the sub-results to give the overall result.
 */
object Integral {
  type Function = Double => Double

  /*
    * the function f
    * the range [l, r] to work on
    * the number taskSize of intervals in its range
    * the size delta of each interval (so taskSize * delta = r - l).
   */
  private type Task = (Function, Double, Double, Int, Double)

  private def worker(fromController: ?[Task], toController: ![Double]): PROC = proc{
    val (f, l, r, taskSize, delta) = fromController?()
    val result = integral(f, l, r, taskSize, delta)
    toController!result
  }

  private def controller(f: Function, a: Double, b: Double, n: Int, numberWorkers: Int,
                        toWorkers: ![Task], fromWorkers: ?[Double]): PROC = proc{
    assert(n % numberWorkers == 0)

    val delta = (b - a) / n
    val taskSize = n / numberWorkers
    val taskRange = (b - a) / numberWorkers

    var left = a

    for (_ <- 0.until(numberWorkers)){
      val right = left + taskRange
      toWorkers!(f, left, right, taskSize, delta)
      left = right
    }

    var result: Double = 0.0

    for (_ <- 0.until(numberWorkers))
      result += fromWorkers?()

    printf("Result: %f\n", result)
  }

  private def buildSystem(f: Function, a:Double, b:Double, n:Int, numberWorkers:Int): PROC = {
    val toWorkers = N2N[Task](1, numberWorkers, "toWorkers")
    val toController = N2N[Double](numberWorkers, 1, "toController")
    val workers = || (for (i <- 0.until(numberWorkers)) yield worker(toWorkers,toController))

    workers || controller(f, a, b, n, numberWorkers, toWorkers, toController)
  }

  private def integral(f: Function, a: Double, b: Double, taskSize: Int, delta: Double): Double = {
    var sum: Double = (f(a) + f(b)) / 2.0

    for (i <- 1.until(taskSize))
      sum += f(a + i * delta)

    sum * delta
  }

  def sequentialComputation(f: Function, a: Double, b: Double, numberIntervals: Int = 10000): Unit ={
    printf("Result: %f\n", integral(f, a, b, numberIntervals, (b - a) / numberIntervals))
  }

  def parallelComputation(f: Function, a: Double, b: Double, numberIntervals: Int = 10000): Unit ={
    var numberWorkers = Math.min(20, numberIntervals)

    while (numberIntervals % numberWorkers != 0)
      numberWorkers += 1

    run(buildSystem(f, a, b, numberIntervals, numberWorkers))
  }

  def time[R](block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block    // call-by-name
    val t1 = System.currentTimeMillis()
    println("Elapsed time: " + (t1 - t0) + "ms")
    result
  }
}
