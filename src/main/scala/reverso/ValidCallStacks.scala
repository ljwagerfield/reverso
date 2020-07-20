package reverso

/**
  * Valid call stacks where the head stack frame represents a valid starting stack frame for the function, the last
  * stack frame represents a 'return true', and the intermediate stack frames are valid transitions between the two.
  */
case class ValidCallStacks(values: List[CallStack]) {
  def add(callStack: CallStack): ValidCallStacks =
    copy(values = callStack :: values)
}

object ValidCallStacks {
  val empty: ValidCallStacks = ValidCallStacks(Nil)
}
