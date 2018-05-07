import scala.collection.mutable._

sealed trait EvaluationResult
case class EvaluationResultSuccess(value: ExpValue, output: String) extends EvaluationResult
case class EvaluationResultError(output: String) extends EvaluationResult

object SWPInterpreter {

  def main(args: Array[String]) {
    val Array(file) = args
    val source = scala.io.Source.fromFile(file)
    val fileString = source.mkString
    source.close()
    val parseResult = ParseProgram.parse(fileString)
    if (parseResult.successful) {
      val result = new Interpreter(() => scala.io.StdIn.readLine(), Console.print).interpret(parseResult.get)
      Console.println("\n\nResult: " + PrettyPrinter.print(result))
    } else {
      Console.println("\n\nError: " + parseResult.toString)
    }
  }

  // DO NOT CHANGE anything that's below here, otherwise we can't test your solution.
  // --------------------------------------------------------------------------------
  def evaluateProgram(program: String, input: Queue[String]): EvaluationResult = {
    val parseResult = ParseProgram.parse(program)
    val output: StringBuilder = new StringBuilder();
    if (parseResult.successful) {
      EvaluationResultSuccess(new Interpreter(input.dequeue, (str: String) => output ++= str).interpret(parseResult.get), output.toString)
    } else {
      EvaluationResultError(parseResult.toString)
    }
  }

  def checkProgramGrammar(program: String): Boolean = {
    ParseProgram.parse(program).successful
  }

  def checkProgramGrammarStringResult(program: String): String = {
    ParseProgram.parse(program).toString
  }
}
