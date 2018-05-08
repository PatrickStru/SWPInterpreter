import scala.util.parsing.combinator._

class ExpParser extends JavaTokenParsers {

  // TODO Implement the expression parser and add additional parsers for terminal and non terminal symbols, where necessary.
  def program: Parser[Program] = ??? //{function, ";"}, expr

  //private val id : Parser[ID] = "/(a-z)(A-Z|a-z|0-9|?|_)*/".r ^^ {s => ID(s.toString)}

  private val int : Parser[Number] = "[1-9][0-9]*|0|-[1-9][0-9]*".r ^^ {i => Number(i.toInt)}

  private val expr = int | boolean

  //private val variable = id

  private val boolean : Parser[Bool] = ("True"|"False") ^^ {bool => Bool(bool.toBoolean)}

  //private val function : Parser[FunctionDeclaration] = ("fun", "id", "(", [{id, ","}, id], ")", "=", expr)


  //private val id : Parser[ID] = "/(a-z)(A-Z|a-z|0-9|?|_)*/".r ^^ {s => ID(s.toString)}


}

object ParseProgram extends ExpParser {
  def parse(s: String): ParseResult[Program] = {
    parseAll(program, s)
  }
}
