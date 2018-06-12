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
      "eq?" -> { case List(a, b) => ExpBoolean(a == b) }
      // TODO
      // Implement the other built in functions here.
    )

  // TODO
  // Use this function to evaluate and execute the given program. The function signature must
  // remain unchanged. You can of course define other helper functions to evaluate the AST for
  // example.
  //
  // If you choose to implement strings and console I/O, you have to use the given reader and
  // writer to interact with the console, because we can test your solution much more easily
  // this way.
  def interpret(program: Node): ExpValue = {
    // writer("Hello, " + reader() + "!")
    ???
  }
}
