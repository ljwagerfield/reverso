package reverso

case class CallStack(frames: List[StackFrame], pointers: UndefinedPointerGraph, allocations: VariableCounter)

object CallStack {
  val empty: CallStack = CallStack(Nil, UndefinedPointerGraph.empty, VariableCounter.empty)
}
