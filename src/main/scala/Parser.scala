import scala.util.parsing.combinator._

class ExpParser extends JavaTokenParsers {

  // TODO Implement the expression parser and add additional parsers for terminal and non terminal symbols, where necessary.
  //def program: Parser[Program] = prog

  def program: Parser[Node] = function | block | cond | call | function | record_def |var_ass | int | braces | block | var_dec | liste | record_dec
  //def program: Parser[Program] = prog

  // working
  private def my_var = id

  // working
  private val id : Parser[ID] = "[a-z]([A-Z]|[a-z]|[0-9]|[?]|_)*".r ^^ {s => ID(s.toString)}

  // working
  private val int : Parser[Number] = "[1-9][0-9]*|0|-[1-9][0-9]*".r ^^ {i => Number(i.toInt)}

  // working
  private val bool : Parser[Bool] = ("True"|"False") ^^ {bool => Bool(bool.toBoolean)}

  // working
  private def cond : Parser[Cond] = ("if" ~ expr ~ "then" ~ expr ~ "else" ~ expr) ^^ {case (_~ l ~_~ m ~_~ r) => Cond(l, m, r)}

  // working
  private def braces : Parser[Braces] = "(" ~> expr <~ ")" ^^ {case (param) => Braces(param)}

  // working
  private def var_ass : Parser[varAssign] = (id ~ "=" ~ expr <~ ";") ^^ {case (l ~_~ r) => varAssign(l, r)}

  // working
  private def block : Parser[Block] = "{" ~> rep(expr) <~ opt(";") <~ "}" <~ ";" ^^ {case (l) => Block(l)}
    //case None => Block()
    //case Some(expression) => varDec1(expression)}

   // case (r) => Block(r)}

  // working
  private def var_dec : Parser[varDec1] = "$" ~> id ~ "=" ~ expr <~ ";" ^^ {case(l ~_~ r) => varDec1(l, r)}

  // TODO should work
  private def record_dec : Parser[recordValueDec1] = "$" ~> id ~ "=" ~ expr ^^ {case(l ~_~ r) => recordValueDec1(l, r)}

  // TODO
  private def record_def : Parser[recordDef] = "object" ~ "{" ~> rep1sep(record_dec, ";") <~ "}" ^^ {case(l) => recordDef(l)}

  // TODO should work
  private def liste : Parser[list] = "[" ~> repsep(expr, ", ") <~ "]" ^^ {case(l) => list(l)}

  // TODO
  private def recordRef : Parser[Node] = block | record_def | braces | my_var | call

  // TODO should work
  private def record_access : Parser[recordAccess] = recordRef ~ "." ~ id ^^ {case(l ~_~ r) => recordAccess(l, r)}

  // TODO
  private def expr : Parser[Node] = block | var_dec | var_ass | cond | liste | my_var | int | bool | braces | call | record_def | record_access

  // TODO not working
  private def call : Parser[Call] = id ~ "(" ~ rep(expr <~ opt(", ")) ~ ")" ^^ {case(name ~_~ params ~_) => Call(name, params)}
  //private val callit : Parser[Call] = "(" ~> repsep(expr, ",") <~ ")" ^^ {case(params) => Call(params)}

  // TODO not working
  private def function : Parser[FunctionDeclaration] = "fun" ~ id ~ "(" ~ rep(id <~ opt(", ")) ~ ")" ~ "=" ~ expr ^^
    {case (_~ name ~_~ params ~_~_~ body) => FunctionDeclaration(name, params, body)}

  // TODO not working
  private def prog : Parser[Program] = rep(function <~ ";") ~ expr ^^ {case(f ~ e) => Program(f, e)}

}

object ParseProgram extends ExpParser {
  def parse(s: String): ParseResult[Node] = {
    parseAll(program, s)
  }

//  def parse(s: String): ParseResult[Program] = {
//    parseAll(program, s)
//  }
}
