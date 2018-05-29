import scala.util.parsing.combinator._

class ExpParser extends JavaTokenParsers {

  // TODO Implement the expression parser and add additional parsers for terminal and non terminal symbols, where necessary.
  def program: Parser[Program] = prog

  //def program: Parser[Node] = record_def //var_ass | int | braces | cond_expr | block | var_dec | liste

  // working
  private val variable = id

  // working
  private val id : Parser[ID] = "[a-z]([A-Z]|[a-z]|[0-9]|[?]|_)*".r ^^ {s => ID(s.toString)}

  // working
  private val int : Parser[Number] = "[1-9][0-9]*|0|-[1-9][0-9]*".r ^^ {i => Number(i.toInt)}

  // working
  private val bool : Parser[Bool] = ("True"|"False") ^^ {bool => Bool(bool.toBoolean)}

  // working
  private val cond : Parser[Cond] = ("if" ~ expr ~ "then" ~ expr ~ "else" ~ expr) ^^ {case (_~ l ~_~ m ~_~ r) => Cond(l, m, r)}

  // working
  private val braces : Parser[Braces] = "(" ~> expr <~ ")" ^^ {case (param) => Braces(param)}

  // working
  private val var_ass : Parser[varAssign] = (id ~ "=" ~ expr <~ ";") ^^ {case (l ~_~ r) => varAssign(l, r)}

  // working
  private val block : Parser[Block] = "{" ~> repsep(expr, ";") <~ "}" ^^ {case (r) => Block(r)}

  // working
  private val var_dec : Parser[varDec1] = "$" ~> id ~ "=" ~ expr <~ ";" ^^ {case(l ~_~ r) => varDec1(l, r)}

  // TODO should work
  private val record_dec : Parser[recordValueDec1] = "$" ~> id ~ "=" ~ expr ^^ {case(l ~_~ r) => recordValueDec1(l, r)}

  // TODO
  private val record_def : Parser[recordDef] = "{" ~> repsep(record_dec, ";") <~ "}" ^^ {case(l) => recordDef(l)}

  // TODO should work
  private val liste : Parser[list] = "[" ~> repsep(expr, ",") <~ "]" ^^ {case(l) => list(l)}

  // TODO
  private val recordRef = block | record_def | braces | variable | call

  // TODO should work
  private val record_access : Parser[recordAccess] = recordRef ~ "." ~ id ^^ {case(l ~_~ r) => recordAccess(l, r)}

  // TODO not sure
  private val call : Parser[Call] = id ~ "(" ~ repsep(expr, ",") <~ ")" ^^ {case(name ~_~ params) => Call(name, params)}

  // TODO not sure
  private val function : Parser[FunctionDeclaration] = "fun" ~ id ~ "(" ~ repsep(id, ",") ~ ")" ~ "=" ~ expr ^^ {case (_~ name ~_~ params ~_~_~ body) => FunctionDeclaration(name, params, body)}

  // TODO not sure
  private val prog : Parser[Program] = repsep(function, ";") ~ "" ~ expr ^^ {case(f ~_~ e) => Program(f, e)}

  // TODO
  private val expr = block | record_def | record_access | var_dec | var_ass | cond | liste | variable | int | bool | braces | call

}

object ParseProgram extends ExpParser {
//  def parse(s: String): ParseResult[Node] = {
//    parseAll(program, s)
//  }

  def parse(s: String): ParseResult[Program] = {
    parseAll(program, s)
  }
}
