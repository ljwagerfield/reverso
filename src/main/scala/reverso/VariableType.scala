package reverso

sealed trait VariableType

object VariableType {
  case object BooleanVariableType extends VariableType

  case class IntVariableType(
    lowerBound: Int,
    upperBound: Int
  ) extends VariableType

  case class DoubleVariableType(
    lowerBound: Double,
    upperBound: Double,
    precision: Double
  ) extends VariableType
}
