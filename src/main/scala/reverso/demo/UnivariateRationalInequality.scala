package reverso.demo

import org.chocosolver.solver.variables.IntVar
import org.chocosolver.solver.{Model, Solver}

import scala.collection.mutable.ListBuffer

/**
  * Spike: Using Choco to solve univariate rational (in)equalities.
  *
  * Solving univariate rational inequalities is fairly well-documented, so combined with our approach for consistency
  * checking of inequalities by rearranging to all permutations of subjects, forming a directed graph, and checking for
  * cycles, we _could_ have done this part ourselves... multivariate seemed much harder, and required reading papers :(
  *
  * In either case, Choco makes this _so much simpler_ for us -- we're in a much better place now we have it!
  */
object UnivariateRationalInequality extends App {
  val model: Model = new Model()
  val min          = -6 // IntVar.MIN_INT_BOUND
  val max          = 6  // IntVar.MAX_INT_BOUND
  val x: IntVar    = model.intVar("x", min, max, true)
  x.add(3).mul(x.sub(2)).div(x.sub(4)).ge(0).post()

  val solver: Solver = model.getSolver

  val resultBuffer = ListBuffer.empty[Int]
  while (solver.solve())
    resultBuffer.addOne(x.getValue)

  val results = resultBuffer.toList

  println("Done!")
  println(results.sorted)
}
