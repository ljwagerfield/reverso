package reverso

case class CallStack(frames: List[StackFrame], pointers: PointerGraph)

object CallStack {
  val empty: CallStack = CallStack(Nil, PointerGraph.empty)
}
