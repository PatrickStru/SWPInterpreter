import scala.util.parsing.combinator._

class ExpParser extends JavaTokenParsers {

  // TODO Implement the expression parser and add additional parsers for terminal and non terminal symbols, where necessary.
  def program: Parser[Program] = ???
}

object ParseProgram extends ExpParser {
  def parse(s: String): ParseResult[Program] = {
    parseAll(program, s)
  }
}
