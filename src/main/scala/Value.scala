// DO NOT CHANGE!
// These datatypes will be used to test your solution!
sealed trait ExpValue
case class ExpInteger(i: Int) extends ExpValue
case class ExpList(list: List[ExpValue]) extends ExpValue
case class ExpBoolean(b: Boolean) extends ExpValue
case class ExpRecord(data: collection.mutable.Map[String, ExpValue]) extends ExpValue
case class ExpString(str: String) extends ExpValue
// TODO you can add new datatypes if you want

object PrettyPrinter {

  // You can use this function to produce readable output.
  // There is no required format; we will use ExpValue to test your solution.
  def print(value: ExpValue): String = value match {
    case ExpBoolean(true) => "True"
    case ExpBoolean(false) => "False"
    case ExpInteger(i) => i.toString
    case ExpList(list) => list.map(print).mkString("[", ", ", "]")
    case ExpRecord(data) => data.keys.map(name => name + "=" + print(data(name))).mkString("object {", ", ", "}")
    case ExpString(str) => "\"" + str + "\""
  }
}
