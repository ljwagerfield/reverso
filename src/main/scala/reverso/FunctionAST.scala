package reverso

import reverso.FunctionAST.Variable.Field

object FunctionAST {
  case class FunctionDefinition(signature: FunctionSignature, body: FunctionBody)
  case class FunctionSignature(inputs: Set[FieldName])
  case class FunctionBody(statements: Map[List[Predicate], Terminal])

  case class ParameterName(value: String)
  case class FieldName(value: String)

  sealed trait Relatable

  sealed trait Constant extends Relatable

  object Constant {
    case class IntConstant(value: Int)       extends Constant
    case class DoubleConstant(value: Double) extends Constant
//    case class CharConstant(value: Char)       extends Constant
//    case class ByteConstant(value: Byte)       extends Constant
    case class BooleanConstant(value: Boolean) extends Constant
    case object Null                           extends Constant
//    case class StringConstant(value: String)   extends Constant
  }

  sealed trait Assignable
  sealed trait Variable extends Relatable

  object Variable {
    case class Field(parent: Option[Assignable], name: FieldName) extends Variable with Assignable
    case class Head(variable: Variable)                           extends Variable with Assignable
    case class Tail(variable: Variable)                           extends Variable
    case class ObjectEntries(variable: Assignable)                extends Variable // Array of {"key":_, "value":_}
  }

  sealed trait Arithmetic extends Relatable

  object Arithmetic {
    case class Add(left: Relatable, right: Relatable)      extends Arithmetic
    case class Subtract(left: Relatable, right: Relatable) extends Arithmetic
    case class Multiply(left: Relatable, right: Relatable) extends Arithmetic
    case class Divide(left: Relatable, right: Relatable)   extends Arithmetic
  }

  sealed trait Predicate

  // Denormalized to a flat structure by excluding Not() from the AST.
  // This prevents us from having to recursively simplify and translate negations.
  object Predicate {
    case class IsDefined(field: Field)                      extends Predicate
    case class NotDefined(field: Field)                     extends Predicate
    case class IsNull(field: Assignable)                    extends Predicate
    case class NotNull(field: Assignable)                   extends Predicate
    case class IsInt(field: Assignable)                     extends Predicate
    case class NotInt(field: Assignable)                    extends Predicate
    case class IsDouble(field: Assignable)                  extends Predicate
    case class NotDouble(field: Assignable)                 extends Predicate
    case class IsBoolean(field: Assignable)                 extends Predicate
    case class NotBoolean(field: Assignable)                extends Predicate
    case class IsComplex(field: Assignable)                 extends Predicate
    case class NotComplex(field: Assignable)                extends Predicate
    case class IsArray(field: Assignable)                   extends Predicate
    case class NotArray(field: Assignable)                  extends Predicate
    case class IsEmpty(field: Variable)                     extends Predicate // Can only be used on arrays, not objects.
    case class NotEmpty(field: Variable)                    extends Predicate
    case class IsEqual(left: Assignable, right: Relatable)  extends Predicate
    case class NotEqual(left: Assignable, right: Relatable) extends Predicate
    case class LT(left: Assignable, right: Relatable)       extends Predicate
    case class LTE(left: Assignable, right: Relatable)      extends Predicate
    case class GT(left: Assignable, right: Relatable)       extends Predicate
    case class GTE(left: Assignable, right: Relatable)      extends Predicate
  }

  sealed trait Assignment
  object Assignment {
    case class Prepend(array: Variable, element: Relatable) extends Assignment
    case class Assign(target: Field, newValue: Relatable)   extends Assignment
    case class Unassign(target: Field)                      extends Assignment
  }

  sealed trait Terminal

  object Terminal {
    case class Continue(assignments: List[Assignment]) extends Terminal
    case object Success                                extends Terminal
  }
}
