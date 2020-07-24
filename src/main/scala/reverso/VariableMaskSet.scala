package reverso

import cats.data.NonEmptyList
import reverso.VariableRef.IntVariable
import cats.implicits._
import com.github.ghik.silencer.silent
import org.chocosolver.solver.constraints.Constraint
import org.chocosolver.solver.constraints.extension.Tuples
import reverso.VariablePoolPartition.StackFrameVariables

import scala.collection.mutable

/**
  * A set of variable masks: each solution will have exactly one of these masks applied.
  */
case class VariableMaskSet(masks: NonEmptyList[VariableMask], structure: VariableMaskStructure) {

  def toConstraints(choco: ChocoState): List[Constraint] =
    buildTableConstraint(choco) :: buildDoubleConstraints(choco)

  private def buildTableConstraint(choco: ChocoState): Constraint = {
    val table          = new Tuples(true)
    val wildcard       = -1 // Table cells contain one of: 0,1,-1 (former two are absolute, latter is wildcard)
    val tableConstants = masks.map(structure.tableConstraintConstants(_, wildcard).toArray).toList
    val tableVariables = structure.tableConstraintVariables(choco)
    table.allowUniversalValue()
    table.setUniversalValue(wildcard)
    table.add(tableConstants: _*)
    choco.model.table(tableVariables.toArray, table)
  }

  /**
    * Doubles cannot be added to table constraints (else we would), so we zero doubles individually.
    */
  private def buildDoubleConstraints(choco: ChocoState): List[Constraint] =
    structure.callStackIndexVariable match {
      case Some(callStackIndex) =>
        val indexVar = choco.ints(callStackIndex)

        @silent
        val constraints =
          masks.toList.flatMap { mask =>
            val maskConstraints = structure.doubleConstraints(choco, mask)
            val maskConstraintsReified = maskConstraints.map { constraint =>
              choco.model.ifThen(
                indexVar.ne(mask.callStackIndex).decompose(),
                constraint
              )
            }
            maskConstraintsReified
          }

        // Todo: We need to find out how to return the constraints from a half-reification so that they can be unposted!
        //       The limitation is in place to prevent you from reifying a reification, which is fair, but we need them for unposting.
        ???

      case None =>
        // Todo: see below Todo! It's a bit presumptuous to only work on head here...
        structure.doubleConstraints(choco, masks.head)
    }

}

object VariableMaskSet {

  def from(
    choco: ChocoState,
    // Todo: replace with Either[CallStack, (NonEmptyList[CallStack], IntVariable)] and replace funniness in 'buildDoubleConstraints'
    callStacks: NonEmptyList[CallStack],
    callStackIndexVariable: Option[IntVariable]
  ): VariableMaskSet = {
    val filterVariables    = (_: VariableRef[_]).definition.partition === StackFrameVariables
    def selectVariables[A] = (_: mutable.Map[VariableRef[A], _]).keys.filter(filterVariables).toList
    VariableMaskSet(
      callStacks.zipWithIndex.map(VariableMask.tupled),
      VariableMaskStructure(
        callStackIndexVariable,
        callStacks.toList.flatMap(_.frames).distinct, // Call stacks will share some common frames.
        VariableList(
          selectVariables(choco.booleans),
          selectVariables(choco.ints),
          selectVariables(choco.doubles)
        )
      )
    )
  }
}
