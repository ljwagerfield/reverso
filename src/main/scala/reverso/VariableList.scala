package reverso

import reverso.VariableRef.{BooleanVariable, DoubleVariable, IntVariable}

case class VariableList(booleans: List[BooleanVariable], ints: List[IntVariable], doubles: List[DoubleVariable]) {
  lazy val booleanSet: Set[BooleanVariable] = booleans.toSet
  lazy val intSet: Set[IntVariable]         = ints.toSet
  lazy val doubleSet: Set[DoubleVariable]   = doubles.toSet
}
