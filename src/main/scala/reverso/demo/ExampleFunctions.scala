package reverso.demo

import reverso.FunctionAST.Assignment.{Assign, Unassign}
import reverso.FunctionAST.Constant.{BooleanConstant, IntConstant}
import reverso.FunctionAST.Predicate._
import reverso.FunctionAST.Terminal.{Continue, Success}
import reverso.FunctionAST.Variable.{Field, Head, Tail}
import reverso.FunctionAST.{FieldName, FunctionBody, FunctionDefinition, FunctionSignature}

object ExampleFunctions {

  /**
    * Outputs: { "Input": [1,0,1,0,...] }
    *          { "Input": [0,1,0,1,...] }
    */
  val nonEmptyAlternatingBinary: FunctionDefinition = {
    val input: Field = Field(None, FieldName("Input"))
    object Registers {
      val nonEmpty: Field = Field(None, FieldName("NonEmpty"))
      val zero: Field     = Field(None, FieldName("Zero"))
      val one: Field      = Field(None, FieldName("One"))
    }
    FunctionDefinition(
      FunctionSignature(
        Set(
          input.name
        )
      ),
      FunctionBody(
        Map(
          (
            List(
              IsDefined(input),
              IsArray(input),
              IsEmpty(input),
              IsDefined(Registers.nonEmpty),
              IsEqual(Registers.nonEmpty, BooleanConstant(true))
            ),
            Success
          ),
          (
            List(
              IsDefined(input),
              IsArray(input),
              NotEmpty(input),
              IsInt(Head(input)),
              IsEqual(Head(input), IntConstant(0)),
              IsDefined(Registers.zero),
              IsEqual(Registers.zero, BooleanConstant(true))
            ),
            Continue(
              List(
                Assign(input, Tail(input)),
                Assign(Registers.one, BooleanConstant(true)),
                Unassign(Registers.zero) // Try: Assign(Registers.zero, BooleanConstant(false))
              )
            )
          ),
          (
            List(
              IsDefined(input),
              IsArray(input),
              NotEmpty(input),
              IsInt(Head(input)),
              IsEqual(Head(input), IntConstant(0)),
              NotDefined(Registers.nonEmpty)
            ),
            Continue(
              List(
                Assign(input, Tail(input)),
                Assign(Registers.one, BooleanConstant(true)),
                Assign(Registers.nonEmpty, BooleanConstant(true))
              )
            )
          ),
          (
            List(
              IsDefined(input),
              IsArray(input),
              NotEmpty(input),
              IsInt(Head(input)),
              IsEqual(Head(input), IntConstant(0)),
              IsDefined(Registers.one),
              IsEqual(Registers.one, BooleanConstant(true))
            ),
            Continue(
              List(
                Assign(input, Tail(input)),
                Assign(Registers.zero, BooleanConstant(true)),
                Unassign(Registers.one) // Try: Assign(Registers.one, BooleanConstant(false))
              )
            )
          ),
          (
            List(
              IsDefined(input),
              IsArray(input),
              NotEmpty(input),
              IsInt(Head(input)),
              IsEqual(Head(input), IntConstant(0)),
              NotDefined(Registers.nonEmpty)
            ),
            Continue(
              List(
                Assign(input, Tail(input)),
                Assign(Registers.zero, BooleanConstant(true)),
                Assign(Registers.nonEmpty, BooleanConstant(true))
              )
            )
          )
        )
      )
    )
  }
}
