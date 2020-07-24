package reverso

import cats.Eq

/**
  * Describes which partition of the pool a variable exists in.
  *
  * Each partition of the pool holds an independent set of variables: there are no common variables between partitions.
  *
  * Partitioning is necessary to ensure we don't reuse variables across meta and stack frames (e.g. we don't ever want
  * the 'currentStackIndex' meta variable to be reused as a stack frame variable).
  *
  * See [[VariablePool]] to learn how variable reuse works.
  */
sealed trait VariablePoolPartition

object VariablePoolPartition {
  implicit val eq: Eq[VariablePoolPartition] = Eq.fromUniversalEquals

  /**
    * Meta variables declared by our code.
    */
  case object MetaVariables extends VariablePoolPartition

  /**
    * Variables declared within the stack frames of the user's predicate.
    */
  case object StackFrameVariables extends VariablePoolPartition

}
