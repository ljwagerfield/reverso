package reverso

/**
  * Completed call stacks: the head stack frame represents a valid starting stack frame for the function, the last stack
  * frame represents a 'return true', and the intermediate stack frames are valid transitions between the two.
  */
case class CompletedCallStacks(values: List[CallStack]) {
  def add(callStack: CallStack): CompletedCallStacks =
    copy(values = callStack :: values)
}

object CompletedCallStacks {
  val empty: CompletedCallStacks = CompletedCallStacks(Nil)
}
