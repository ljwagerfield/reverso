package reverso.demo

import cats.effect.{ExitCode, IO, IOApp}
import reverso.PredicateCompiler
import reverso.demo.ExamplePredicates._
import cats.implicits._
import reverso.PredicateAST.PredicateDefinition
import reverso.common.UInt

object Application extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    printSolutions(nonEmptyAlternatingBinary, variableLimit = 1000)

  private def printSolutions(function: PredicateDefinition, variableLimit: UInt): IO[ExitCode] =
    for {
      model <- new PredicateCompiler[IO]().compile(function, variableLimit)
      _     <- model.solutions.use(_.map(_.toString).showLinesStdOut.compile.drain)
    } yield ExitCode.Success

}
