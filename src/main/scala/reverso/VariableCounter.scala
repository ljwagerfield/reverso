package reverso

import reverso.VariableType.{BooleanVariableType, DoubleVariableType, IntVariableType}

import scala.reflect.ClassTag

case class VariableCounter(variableCounts: Map[VariableDefinition[VariableType], Int]) {
  def allocateVariable[A <: VariableType](definition: VariableDefinition[A]): (VariableCounter, VariableRef[A]) = {
    val nextIndex   = variableCounts.getOrElse(definition, 0)
    val updatedPool = VariableCounter(variableCounts + (definition -> (nextIndex + 1)))
    updatedPool -> VariableRef(definition, nextIndex)
  }

  lazy val variables: VariableList =
    VariableList(
      expandCountsToVariableRefs[BooleanVariableType.type],
      expandCountsToVariableRefs[IntVariableType],
      expandCountsToVariableRefs[DoubleVariableType]
    )

  private def expandCountsToVariableRefs[A <: VariableType: ClassTag]: List[VariableRef[A]] =
    variableCounts.collect {
      case (VariableDefinition(p, t: A), count) =>
        (0 until count).map(VariableRef(VariableDefinition(p, t), _)).toList
    }.toList.flatten
}

object VariableCounter {
  val empty: VariableCounter = VariableCounter(Map.empty)
}
