package reverso

import reverso.VariableType.{BooleanVariableType, DoubleVariableType, IntVariableType}

case class VariableRef[A](definition: VariableDefinition[A], index: Int)

object VariableRef {
  type BooleanVariable = VariableRef[BooleanVariableType.type]
  type IntVariable     = VariableRef[IntVariableType]
  type DoubleVariable  = VariableRef[DoubleVariableType]
}
