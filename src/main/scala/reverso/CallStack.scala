package reverso

case class CallStack(frames: List[StackFrame], pointers: UndefinedPointerGraph, allocations: VariableCounter) {
  lazy val frameSet: Set[StackFrame] = frames.toSet
}

object CallStack {
  val empty: CallStack = CallStack(Nil, UndefinedPointerGraph.empty, VariableCounter.empty)
}
