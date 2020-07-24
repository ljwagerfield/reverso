package reverso

import cats.implicits._
import org.chocosolver.solver.constraints.Constraint
import org.chocosolver.solver.variables.IntVar
import reverso.VariableRef.IntVariable

/**
  * Defines the structure each [[VariableMask]] will be projected to:
  *
  * - Variables in [[VariableMaskStructure]] that intersect the [[VariableMask]] will be set to * (i.e. enabled).
  *
  * - Variables in [[VariableMaskStructure]] that don't intersect the [[VariableMask]] will be set to 0 (i.e. disabled).
  */
case class VariableMaskStructure(
  callStackIndexVariable: Option[IntVariable],
  allFrames: List[StackFrame],
  allVariables: VariableList
) {

  /**
    * Doubles cannot be added to table constraints (else we would), so we zero doubles individually.
    */
  def doubleConstraints(choco: ChocoState, mask: VariableMask): List[Constraint] = {
    val disabled = (allVariables.doubleSet -- mask.callStack.allocations.variables.doubleSet).toList
    val doubles  = disabled.collect(choco.doubles)
    doubles.map(_.eq(0).ibex(0D))
  }

  def tableConstraintConstants(mask: VariableMask, wildcard: Int): List[Int] = {
    def variableMask[A](
      list: VariableList => List[VariableRef[A]],
      set: VariableList => Set[VariableRef[A]]
    ): List[Int] = {
      val all     = list(allVariables)
      val enabled = set(mask.callStack.allocations.variables)
      all.map(variable => if (enabled.contains(variable)) wildcard else 0)
    }

    val index         = callStackIndexVariable.as(mask.callStackIndex).toList
    val enabledFrames = mask.callStack.frameSet
    val frames        = allFrames.map(frame => if (enabledFrames.contains(frame)) 1 else 0)
    val booleans      = variableMask(_.booleans, _.booleanSet)
    val ints          = variableMask(_.ints, _.intSet)

    index ::: frames ::: booleans ::: ints
  }

  def tableConstraintVariables(choco: ChocoState): List[IntVar] = {
    val index    = callStackIndexVariable.toList.collect(choco.ints)
    val frames   = allFrames.map(_.constraintSwitch).collect(choco.booleans)
    val booleans = allVariables.booleans.collect(choco.booleans)
    val ints     = allVariables.ints.collect(choco.ints)

    index ::: frames ::: booleans ::: ints
  }

}
