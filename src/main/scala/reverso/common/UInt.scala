package reverso.common

case class UInt(value: Int) {
  require(value >= 0, "Pseudo-unsigned integer must be 0 or greater.")

  def +(other: UInt): UInt     = UInt(value + other.value)
  def -(other: UInt): UInt     = UInt(value - other.value)
  def <(other: UInt): Boolean  = value < other.value
  def <=(other: UInt): Boolean = value <= other.value
  def >(other: UInt): Boolean  = value > other.value
  def >=(other: UInt): Boolean = value >= other.value
}

object UInt {
  implicit def toUnsigned(value: Int): UInt = UInt(value)
}
