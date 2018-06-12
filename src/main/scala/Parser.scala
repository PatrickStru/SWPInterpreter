import com.sun.org.apache.xpath.internal.operations.Variable

import scala.util.parsing.combinator._

class ExpParser extends JavaTokenParsers {

  def program : Parser[Program] = prog

  // working
  def variable : Parser[ID] = id

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
  def block : Parser[Block] = "{" ~> rep(expr <~ opt(";")) <~ "}" ^^ {case (l) => Block(l)}

  // working
  def var_dec : Parser[varDec1] = "$" ~> id ~ "=" ~ expr <~ ";" ^^ {case(l ~_~ r) => varDec1(l, r)}

  // TODO should work
  def record_dec : Parser[recordValueDec1] = "$" ~> id ~ "=" ~ expr ^^ {case(l ~_~ r) => recordValueDec1(l, r)}

  // TODO should work
  def record_def : Parser[recordDef] = "object" ~ "{" ~> rep(record_dec <~ ";") <~ "}" ^^ {case(l) => recordDef(l)}

  // TODO should work
  def liste : Parser[list] = "[" ~> repsep(expr, ",") <~ "]" ^^ {case(l) => list(l)}

  // TODO should work
  def recordRef : Parser[Node] = call | block | record_def | braces | variable

  // TODO ???
  def record_access : Parser[recordAccess] = recordRef ~ "." ~ id ^^ {case(l ~_~ r) => recordAccess(l, r)}

  // TODO should work
  def expr : Parser[Node] = call | block | var_dec | var_ass | cond | liste | record_def | int | bool | braces | record_access | variable

  // TODO should work
  def call : Parser[Call] = id ~ "(" ~ repsep(expr, ",") <~ ")" ^^ {case(name ~_~ params) => Call(name, params)}

  // TODO should work
  def function : Parser[FunctionDeclaration] = "fun" ~> id ~ "(" ~ repsep(id, ",") ~ ")" ~ "=" ~ expr ^^
    {case (name ~_~ params ~_~_~ body) => FunctionDeclaration(name, params, body)}

  // TODO should work
  def prog : Parser[Program] = rep(function <~ ";") ~ expr ^^ {case(f ~ e) => Program(f, e)}
}

object ParseProgram extends ExpParser {

  def parse(s: String): ParseResult[Program] = {
    parseAll(program, s)
  }
}