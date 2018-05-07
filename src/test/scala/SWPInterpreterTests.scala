import org.scalatest.FunSuite
import scala.util.parsing.combinator._
import scala.collection.mutable._

class SWPInterpreterTests extends FunSuite {

  // TODO write your own testcases if you want and feel free to share them in the newsgroup!

  def expectValidGrammar(prog: String) = {
    if(!SWPInterpreter.checkProgramGrammar(prog)) {
      fail(SWPInterpreter.checkProgramGrammarStringResult(prog))
    }
  }

  def expectInvalidGrammar(prog: String) = {
    assert(!SWPInterpreter.checkProgramGrammar(prog))
  }

  def expectResult(prog: String, expect: ExpValue) = {
    assertResult(EvaluationResultSuccess(expect, "")) {
      SWPInterpreter.evaluateProgram(prog, new Queue())
    }
  }

  def expectResult(prog: String, expectedResult: ExpValue, input: Queue[String], expectedOutput: String) = {
    assertResult(EvaluationResultSuccess(expectedResult, expectedOutput)) {
      SWPInterpreter.evaluateProgram(prog, input)
    }
  }

  test("Parser minimal example") {
    expectValidGrammar("""
      42
    """)
  }

  test("Parser short program") {
    expectValidGrammar("""
      fun ascending(i, l) = {
        if eq?(i, 0) then
          l
        else {
          $j = add(i, 1);
          build(j, l);
        };
      };
      ascending(5, [])
    """)
  }

  test("Parser defect program") {
    expectInvalidGrammar("""
      fun odd(l) = if eq?(l, []) then False else even(rest(l);
      fun even(l) = if eq?(l, []) then True else odd(rest(l)));
      even([1, 2,])
    """)
  }

  test("Interpreter program with only built in functions") {
    expectResult("""
      if eq?([1], build(1, [])) then add(3, sub(-2, -1)) else add(4, 2)
    """,
    ExpInteger(2))
  }

  test("Interpreter program with user defined functions") {
    expectResult("""
      fun sum(a) = if lt?(a, 1) then 0 else add(sum(add(a, -1)), a);
      fun toList(a) = [sum(a), a];
      toList(4)
    """,
    ExpList(List(ExpInteger(10), ExpInteger(4))))
  }

  test("Interpreter variables") {
    expectResult("""
      {
        $a = 5;
        $b = 4;
        {
          $a = 2;
          b = add(a, b);
        };
        add(b, { a; });
      }
    """,
    ExpInteger(11))
  }

  test("Interpreter records") {
    expectResult("""
      fun addValues(obj) = add(add(obj.left, (obj.right).left), (obj.right).right);
      addValues(object { $left = 1; $right = object { $left = 2; $right = 3; }; })
    """,
    ExpInteger(6))
  }

/*
  test("Interpreter string basic") {
    expectResult("""
      "1: Hello World!"
    """,
    ExpString("1: Hello World!"))
  }

  test("Interpreter hello world") {
    expectResult("""
      { print("Hello"); print(" World!"); 0; }
    """,
    ExpInteger(0),
    new Queue(),
    "Hello World!")
  }

  test("Interpreter read") {
    expectResult("""
      read()
    """,
    ExpString("Echo"),
    Queue("Echo"),
    "")
  }

  test("Parser comments") {
    expectValidGrammar("""
      add(#)
      1, 2)##
      # 0
    """)
  }

  test("Parser comments invalid") {
    expectInvalidGrammar("""
      add(1#, 2)
    """)
  }

  test("Parser multiline comments") {
    expectValidGrammar("""
      add(#*
      +#)*#1,#*
      swp*#2
      )
    """)
  }
*/
}
