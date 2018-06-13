import com.sun.org.apache.xpath.internal.operations.Variable

import scala.util.parsing.combinator._

class ExpParser extends JavaTokenParsers {

  def program : Parser[Program] = prog

  val id : Parser[String] = "[a-z]([A-Z]|[a-z]|[0-9]|[?]|_)*".r

  val int : Parser[Number] = "[1-9][0-9]*|0|-[1-9][0-9]*".r ^^ {i => Number(i.toInt)}

  val bool : Parser[Bool] = ("True"|"False") ^^ {bool => Bool(bool.toBoolean)}

  def variable : Parser[Var] = id ^^ {case(v) => Var(v)}

  def liste : Parser[list] = "[" ~> repsep(expr, ",") <~ "]" ^^ {case(l) => list(l)}

  def cond : Parser[Cond] = "if" ~> expr ~ "then" ~ expr ~ "else" ~ expr ^^ {case (l ~_~ m ~_~ r) => Cond(l, m, r)}

  def braces : Parser[Braces] = "(" ~> expr <~ ")" ^^ {case (param) => Braces(param)}

  //def var_ass : Parser[varAssign] = (id ~ "=" ~ expr <~ opt(";")) ^^ {case (l ~_~ r) => varAssign(l, r)}
  def var_ass : Parser[varAssign] = id ~ "=" ~ expr ^^ {case (l ~_~ r) => varAssign(l, r)}

  //def var_dec : Parser[varDec1] = "$" ~> id ~ "=" ~ expr <~ ";" ^^ {case(l ~_~ r) => varDec1(l, r)}
  def var_dec : Parser[varDec1] = "$" ~> id ~ "=" ~ expr ^^ {case(l ~_~ r) => varDec1(l, r)}

  //def block : Parser[Block] = "{" ~> rep(expr <~ opt(";")) <~ "}" ^^ {case (l) => Block(l)}
  def block : Parser[Block] = "{" ~> rep(expr <~ ";") <~ "}" ^^ {case (l) => Block(l)}

  def record_dec : Parser[recordValueDec1] = "$" ~> id ~ "=" ~ expr ^^ {case(l ~_~ r) => recordValueDec1(l, r)}

  def record_def : Parser[recordDef] = "object" ~ "{" ~> rep(record_dec <~ ";") <~ "}" ^^ {case(l) => recordDef(l)}

  def recordRef : Parser[Node] = block | braces | call | record_def | variable

  def record_access : Parser[recordAccess] = recordRef ~ "." ~ id ^^ {case(l ~_~ r) => recordAccess(l, r)}

  def call : Parser[Call] = id ~ "(" ~ repsep(expr, ",") <~ ")" ^^ {case(name ~_~ params) => Call(name, params)}

  def expr : Parser[Node] = block | record_access | call | var_ass | record_def | var_dec | braces| cond | liste | int | bool | variable

  def function : Parser[FunctionDeclaration] = "fun" ~> id ~ "(" ~ repsep(id, ",") ~ ")" ~ "=" ~ expr ^^
    {case (name ~_~ params ~_~_~ body) => FunctionDeclaration(name, params, body)}

  def prog : Parser[Program] = rep(function <~ ";") ~ expr ^^ {case(f ~ e) => Program(f, e)}
}

object ParseProgram extends ExpParser {

  def parse(s: String): ParseResult[Program] = {
    parseAll(program, s)
  }
}