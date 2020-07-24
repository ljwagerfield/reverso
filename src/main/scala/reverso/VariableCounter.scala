package reverso

case class VariableCounter(variableCounts: Map[VariableDefinition[_], Int]) {
  def allocateVariable[A](definition: VariableDefinition[A]): (VariableCounter, VariableRef[A]) = {
    val nextIndex   = variableCounts.getOrElse(definition, 0)
    val updatedPool = VariableCounter(variableCounts + (definition -> (nextIndex + 1)))
    updatedPool -> VariableRef(definition, nextIndex)
  }
}

object VariableCounter {
  val empty: VariableCounter = VariableCounter(Map.empty)
}
