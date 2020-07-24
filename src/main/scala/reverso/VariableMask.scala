package reverso

/**
  * All variables not represented by this mask will be zeroed (if the mask is applied).
  */
case class VariableMask(callStack: CallStack, callStackIndex: Int)
