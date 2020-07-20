package reverso

import reverso.Variables.BooleanVariable

/**
  * @param constraintSwitch Used to enable/disable constraints introduced by this stack frame to the single shared
  *                         Choco model. The process of conditionally activating a constraint is referred to as
  *                         'half reification', or making the constraint a 'half-reified constraint'.
  */
case class StackFrame(constraintSwitch: BooleanVariable)
