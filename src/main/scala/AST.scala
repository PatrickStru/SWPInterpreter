sealed trait Node

case class Program(functions: List[FunctionDeclaration], main: Node) extends Node
case class FunctionDeclaration(name: String, params: List[String], body: Node) extends Node

// TODO Add case classes to represent the AST.
case class ID(s: String) extends Node
case class Number(i: Int) extends Node
case class Bool(bool: Boolean) extends Node