import com.sun.org.apache.xpath.internal.operations.Variable

import scala.util.parsing.combinator._

class ExpParser extends JavaTokenParsers {

  // TODO Implement the expression parser and add additional parsers for terminal and non terminal symbols, where necessary.
  //def program: Parser[Program] = prog

  //def program: Parser[Node] = function | block | cond | call | function | record_def |var_ass | int | braces | block | var_dec | liste | record_dec
  def program: Parser[Program] = prog

  // working
  def my_var: Parser[Variable] = id ^^ {case(id) => Variable(id)}

  // working
  val id : Parser[ID] = "[a-z]([A-Z]|[a-z]|[0-9]|[?]|_)*".r ^^ {s => ID(s.toString)}

  // working
  val int : Parser[Number] = "[1-9][0-9]*|0|-[1-9][0-9]*".r ^^ {i => Number(i.toInt)}

  // working
  val bool : Parser[Bool] = ("True"|"False") ^^ {bool => Bool(bool.toBoolean)}

  // working
  def cond : Parser[Cond] = "if" ~> expr ~ "then" ~ expr ~ "else" ~ expr ^^ {case (l ~_~ m ~_~ r) => Cond(l, m, r)}

  // working
  def braces : Parser[Braces] = "(" ~> expr <~ ")" ^^ {case (param) => Braces(param)}

  // working
  def var_ass : Parser[varAssign] = (id ~ "=" ~ expr <~ ";") ^^ {case (l ~_~ r) => varAssign(l, r)}

  // working
  def block : Parser[Block] = "{" ~> rep(expr) <~ opt(";") <~ "}" ^^ {case (l) => Block(l)}
  //private def block : Parser[Block] = "{" ~> rep(expr <~ ";") <~ "}" ^^ {case (l) => Block(l)}
    //case None => Block()
    //case Some(expression) => varDec1(expression)}

   // case (r) => Block(r)}

  /*
  def block: Parser[Block] = "{" ~> rep(expr <~ ";") <~ "}" ^^
  {
    case b => Block(b)
  }

  def recordDef: Parser[RecordDef] = "object" ~ "{" ~> rep(recordValueDecl <~ opt(";")) <~ "}" ^^
  {
    case expr => RecordDef(expr)
  }
   */

  // working
  def var_dec : Parser[varDec1] = "$" ~> id ~ "=" ~ expr <~ ";" ^^ {case(l ~_~ r) => varDec1(l, r)}

  // TODO should work
  def record_dec : Parser[recordValueDec1] = "$" ~> id ~ "=" ~ expr ^^ {case(l ~_~ r) => recordValueDec1(l, r)}

  // TODO
  def record_def : Parser[recordDef] = "object" ~> "{" ~> rep1sep(record_dec, ";") <~ "}" ^^ {case(l) => recordDef(l)}

  // TODO should work
  def liste : Parser[list] = "[" ~> repsep(expr, ",") <~ "]" ^^ {case(l) => list(l)}

  // TODO
  def recordRef : Parser[Node] = block | record_def | braces | my_var | call

  // TODO should work
  def record_access : Parser[recordAccess] = recordRef ~ "." ~ id ^^ {case(l ~_~ r) => recordAccess(l, r)}

  // TODO
  def expr : Parser[Node] = call | block | var_dec | var_ass | cond | liste | my_var | int | bool | braces | record_def | record_access

  // TODO not working
  //def call : Parser[Call] = id ~ "(" ~ rep(expr <~ opt(",")) <~ ")" ^^ {case(name ~_~ params) => Call(name, params)}
  private val call : Parser[Call] = id ~ "(" ~ repsep(expr, ",") <~ ")" ^^ {case(name ~_~ params) => Call(name, params)}

  // TODO not working
  def function : Parser[FunctionDeclaration] = "fun" ~> id ~ "(" ~ rep(id <~ opt(",")) ~ ")" ~ "=" ~ expr ^^
    {case (name ~_~ params ~_~_~ body) => FunctionDeclaration(name, params, body)}

  // TODO not working
  def prog : Parser[Program] = rep(function <~ ";") ~ expr ^^ {case(f ~ e) => Program(f, e)}

}

object ParseProgram extends ExpParser {
//  def parse(s: String): ParseResult[Node] = {
//    parseAll(program, s)
//  }

  def parse(s: String): ParseResult[Program] = {
    parseAll(program, s)
  }
}