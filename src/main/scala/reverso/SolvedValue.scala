package reverso

sealed trait SolvedValue

object SolvedValue {
  sealed trait SolvedScalar extends SolvedValue

  object SolvedScalar {
    case class SolvedInt(value: Int)       extends SolvedScalar
    case class SolvedDouble(value: Double) extends SolvedScalar
    case class SolvedBool(value: Boolean)  extends SolvedScalar
  }

  case class SolvedObject(fields: Map[String, SolvedValue]) extends SolvedValue

  case class SolvedVector(elements: List[SolvedValue]) extends SolvedValue
}
