package reverso

import reverso.PredicateAST.Variable.Field

/**
  * Abstract syntax tree (AST) for defining predicates (functions that return a boolean).
  *
  * Design Focus:
  *
  *   The AST has been designed to be evaluated in reverse, meaning the code that consumes it first looks at the end
  *   stack frame (i.e. 'return true') and steps backwards through all the possible stack frames that may have lead
  *   to that state, until it reaches a viable starting stack frame for the predicate.
  *
  * Features:
  *
  *   The AST is therefore primitive in order to make reverse-engineering it easier: there is a single outer loop and a
  *   sequence of inner IF statements. There are no ELSE statements. An IF statement can contain equalities (==),
  *   inequalities (<,>,<=,>=,!=), basic algebra (+,-,*,/) and AND operators, but not OR operators. For working with
  *   arrays, you can get the head or tail of an array, and check if it's empty, but that is all. You must implement
  *   all other array operations yourself (e.g. length, sum, filter, exists, etc.). You cannot define your own
  *   functions: everything must be written inside one loop as a sequence of IF statements. The body of an IF statement
  *   is either a 'return true' (see 'Success' terminal) or an assignment of variables (see 'Continue' terminal). The
  *   outer loop continues looping until either a 'return true' is met, or none of the IF statements match, in which
  *   case a 'return false' is applied automatically.
  *
  * Pseudocode:
  *
  *   Example of a predicate that validates a sequence of alternating binary (implemented using the AST's concepts):
  *
  * {{{
  * // system functions used:          isEmpty, nonEmpty, head, set
  * // variables defined by predicate: input, last
  * loop {
  *   if (isEmpty(input))
  *     return true
  *   if (nonEmpty(input) && head(input) != last && head(input) == 0)
  *     set({ input: tail(input), last: head(input) })
  *   if (nonEmpty(input) && head(input) != last && head(input) == 1)
  *     set({ input: tail(input), last: head(input) })
  * }
  * }}}
  */
object PredicateAST {
  case class PredicateDefinition(signature: PredicateSignature, body: PredicateBody)
  case class PredicateSignature(inputs: Set[FieldName])
  case class PredicateBody(statements: Map[List[Constraint], Terminal])

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

  sealed trait BasicAlgebra extends Relatable

  object BasicAlgebra {
    case class Add(left: Relatable, right: Relatable)      extends BasicAlgebra
    case class Subtract(left: Relatable, right: Relatable) extends BasicAlgebra
    case class Multiply(left: Relatable, right: Relatable) extends BasicAlgebra
    case class Divide(left: Relatable, right: Relatable)   extends BasicAlgebra
  }

  sealed trait Constraint

  // Note: This type hierarchy was intentionally flattened/denormalized to make it easier to reason with.
  //       Thus a 'Not()' type does not exist.
  object Constraint {

    /**
      * Constrains whether an object exists or not, and if it does, what type it exists as.
      */
    sealed trait Existential extends Constraint

    object Existential {
      case class IsDefined(field: Field)       extends Existential
      case class NotDefined(field: Field)      extends Existential
      case class IsNull(field: Assignable)     extends Existential
      case class NotNull(field: Assignable)    extends Existential
      case class IsInt(field: Assignable)      extends Existential
      case class NotInt(field: Assignable)     extends Existential
      case class IsDouble(field: Assignable)   extends Existential
      case class NotDouble(field: Assignable)  extends Existential
      case class IsBoolean(field: Assignable)  extends Existential
      case class NotBoolean(field: Assignable) extends Existential
      case class IsComplex(field: Assignable)  extends Existential
      case class NotComplex(field: Assignable) extends Existential
      case class IsArray(field: Assignable)    extends Existential
      case class NotArray(field: Assignable)   extends Existential
      case class IsEmpty(field: Variable)      extends Existential // Can only be used on arrays, not objects.
      case class NotEmpty(field: Variable)     extends Existential
    }

    /**
      * Constrains two or more objects to have values that relate in some way.
      */
    sealed trait Relational extends Constraint

    object Relational {
      case class IsEqual(left: Assignable, right: Relatable)  extends Relational
      case class NotEqual(left: Assignable, right: Relatable) extends Relational
      case class LT(left: Assignable, right: Relatable)       extends Relational
      case class LTE(left: Assignable, right: Relatable)      extends Relational
      case class GT(left: Assignable, right: Relatable)       extends Relational
      case class GTE(left: Assignable, right: Relatable)      extends Relational
    }
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
