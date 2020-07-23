package reverso

case class CallStack(frames: List[StackFrame], pointers: UndefinedPointerGraph)

object CallStack {
  val empty: CallStack = CallStack(Nil, UndefinedPointerGraph.empty)
}
