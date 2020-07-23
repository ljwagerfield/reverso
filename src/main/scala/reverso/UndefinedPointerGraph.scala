package reverso

import reverso.PredicateAST.FieldName
import reverso.UndefinedPointerGraph.MemoryLocation.ComplexObject
import reverso.Variables.{BooleanVariable, DoubleVariable, IntVariable}

/**
  * An 'if' statement asserts that one or more variable definitions must have taken place in an ancestor stack frame.
  *
  * E.g. 'if (x = 5)' asserts that 'x' was defined in an ancestor stack frame (and that it was set to '5').
  *
  * [[UndefinedPointerGraph]] contains all the assertions made by such 'if' statements where the assertion has not yet
  * been matched against an 'assign' statement in an ancestor stack frame, thus leaving said variable undefined.
  *
  * A stack frame is considered an 'initial' stack frame when all the keys in 'root' that are not 'input keys' are
  * undefined: since no 'internal variables' have yet been defined, it must be a possible starting frame. Inputs are
  * allowed to be undefined, since the predicate's signature defines them (and the caller provides their values).
  */
case class UndefinedPointerGraph(root: ComplexObject)

object UndefinedPointerGraph {
  val empty: UndefinedPointerGraph = UndefinedPointerGraph(ComplexObject(Map.empty))

  sealed trait FieldValue

  object FieldValue {
    case object Undefined                       extends FieldValue
    case object Defined                         extends FieldValue
    case class DefinedAs(value: MemoryLocation) extends FieldValue
  }

  sealed trait MemoryLocation

  object MemoryLocation {

    /**
      * Keys not in [[fields]] may exist but are currently unknown.
      */
    case class ComplexObject(fields: Map[FieldName, FieldValue]) extends MemoryLocation

    sealed trait Scalar extends MemoryLocation

    object Scalar {
      sealed trait ScalarValue[+C, +V]

      object ScalarValue {
        case object UnknownValue              extends ScalarValue[Nothing, Nothing]
        case class ConstantValue[C](value: C) extends ScalarValue[C, Nothing]
        case class VariableValue[V](value: V) extends ScalarValue[Nothing, V]
      }

      case class IntScalar(value: ScalarValue[Int, IntVariable])             extends Scalar
      case class DoubleScalar(value: ScalarValue[Double, DoubleVariable])    extends Scalar
      case class BooleanScalar(value: ScalarValue[Boolean, BooleanVariable]) extends Scalar
    }

    sealed trait Array extends MemoryLocation

    object Array {
      case object EmptyArray                                      extends Array
      case object UnknownArray                                    extends Array
      case class NonEmptyArray(head: MemoryLocation, tail: Array) extends Array
    }
  }
}
