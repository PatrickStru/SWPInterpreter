import com.sun.tracing.dtrace.FunctionName

class Interpreter(reader: () => String, writer: String => _) {

  // Type aliases to make the signatures more readable
  type FunctionName = String
  type VariableName = String

  // It is not required to use our datastructure to implement built in functions,
  // you are also allowed to implement it in your own way.
  //
  // How to use the builtinFunctions Map:
  // The call
  // builtinFunctions("eq")(List(ExpBoolean(true), ExpBoolean(false)))
  // will return
  // ExpBoolean(false)
  val builtinFunctions: Map[FunctionName, List[ExpValue] => ExpValue] =
    Map(
      "eq?" -> { case List(a, b) => ExpBoolean(a == b) },
      //arithmetic
      "add" -> { case List(ExpInteger(a), ExpInteger(b)) => ExpInteger(a + b)},
      "sub" -> { case List(ExpInteger(a), ExpInteger(b)) => ExpInteger(a - b)},
      "lt?" -> { case List(ExpInteger(a), ExpInteger(b)) => ExpBoolean(a < b)},
      //lists
      "first" -> { case List(ExpList(xs)) => xs.head},
      "rest" -> { case List(ExpList(xs)) => ExpList(xs.tail)},
      "build" -> { case List(x, ExpList(xs)) => ExpList(x :: xs)},
      //Logik
      "and" -> { case List(ExpBoolean(a), ExpBoolean(b)) => ExpBoolean(a && b)},
      "or" -> { case List(ExpBoolean(a), ExpBoolean(b)) => ExpBoolean(a || b)},
      "not" -> { case List(ExpBoolean(a)) => ExpBoolean(!a)}
  )

  // TODO
  // Use this function to evaluate and execute the given program. The function signature must
  // remain unchanged. You can of course define other helper functions to evaluate the AST for
  // example.
  //
  // If you choose to implement strings and console I/O, you have to use the given reader and
  // writer to interact with the console, because we can test your solution much more easily
  // this way.
  def interpret(program: Program): ExpValue = {
    // writer("Hello, " + reader() + "!")
    helper(program.main)
  }

  def helper(node: Node): ExpValue = node match{
    case ID(i) => ExpString(i)
    case Number(i) => ExpInteger(i)
    case Bool(i) => ExpBoolean(i)
    case list(i) => ExpList(helpbuildlist(node,i))
    case Call(name, params) => builtinFunctions(name)(helpbuildlist(node,params))
    case Cond(x, y, z) => helper(x) match{
      case ExpBoolean(true) => helper(y)
      case ExpBoolean(false) => helper(z)
    }

  }

  def helpbuildlist(node: Node, list: List[Node]): List[ExpValue] = list match{
    case Nil => Nil
    case i::is => helper(list.head)::helpbuildlist(node, list.tail)
  }
}
