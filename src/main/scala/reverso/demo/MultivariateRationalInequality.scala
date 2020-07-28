package reverso.demo

import org.chocosolver.solver.variables.IntVar
import org.chocosolver.solver.{Model, Solver}

import scala.collection.mutable.ListBuffer

/**
  * Spike: Using Choco to solve multivariate rational (in)equalities.
  *
  * This type of equation plays a crucial part in checking whether a stack frame's expressions are consistent with
  * all the preceding stack frames' expressions: it turns out the simplest representation of the most complex
  * relationships that can be expressed between variables across stack frames is a "system of multivariate rational
  * (in)equalities": this is what all the <>=+-*\ statements using a shared set of variables boil down to.
  *
  * This is also where we became stuck.
  *
  * Before learning about CP solvers, the next step was to ~read~ try and understand papers written on solving systems
  * of multivariate rational (in)equalities (at least there were papers)... but then came Choco to save the day!
  *
  * Lo and behold: a multivariate rational inequality...
  */
object MultivariateRationalInequality extends App {
  val model: Model = new Model()
  val min          = -6 // IntVar.MIN_INT_BOUND
  val max          = 6  // IntVar.MAX_INT_BOUND
  val x: IntVar    = model.intVar("x", min, max, true)
  val y: IntVar    = model.intVar("y", min, max, true)
  x.add(y).mul(x.sub(2)).div(x.sub(4)).ge(0).post()

  val solver: Solver = model.getSolver

  val resultBuffer = ListBuffer.empty[(Int, Int)]
  while (solver.solve())
    resultBuffer.addOne(x.getValue -> y.getValue)

  val results = resultBuffer.toList

  println("Done!")
  println(results.sorted)
}
