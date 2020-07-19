package reverso.demo

import cats.effect.{ExitCode, IO, IOApp}
import reverso.FunctionCompiler
import reverso.demo.ExampleFunctions._
import cats.implicits._
import reverso.FunctionAST.FunctionDefinition
import reverso.common.UInt

object Application extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    printSolutions(nonEmptyAlternatingBinary, variableLimit = 1000)

  private def printSolutions(function: FunctionDefinition, variableLimit: UInt): IO[ExitCode] =
    for {
      model <- new FunctionCompiler[IO]().compile(function, variableLimit)
      _     <- model.solutions.use(_.map(_.toString).showLinesStdOut.compile.drain)
    } yield ExitCode.Success

}
