package reverso

case class VariableDefinition[+A](partition: VariablePoolPartition, variableType: A)
