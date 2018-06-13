sealed trait Node

case class Program(functions: List[FunctionDeclaration], main: Node) extends Node
case class FunctionDeclaration(name: String, params: List[Node], body: Node) extends Node

// TODO Add case classes to represent the AST.
case class ID(s: String) extends Node
case class Number(i: Int) extends Node
case class Bool(bool: Boolean) extends Node

case class Cond(l: Node, m: Node, r: Node) extends Node

case class Braces(exp: Node) extends Node

case class Block(exp: List[Node]) extends Node

case class varAssign(l: String, r: Node) extends Node

case class varDec1(l: String, r: Node) extends Node

case class recordValueDec1(l: String, r: Node) extends Node

case class recordDef(exp: List[Node]) extends Node

case class list(exp: List[Node]) extends Node

case class recordAccess(l: Node, r: String) extends Node

case class Call(name: String, params: List[Node]) extends Node

case class Var(name: String) extends Node
//case class Call(params: List[Node]) extends Node