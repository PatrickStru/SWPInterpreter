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

  // custom test case for variable assignment with number
  test("Variable assignment") {
    expectValidGrammar("""
      {a = 1;}
    """)
  }

  // custom test case for variable assignment with boolean
  test("Variable assignment with boolean") {
    expectValidGrammar("""
      {a = True;}
    """)
  }

  // custom test case for braces
  test("Braces") {
    expectValidGrammar("""
      (True)
    """)
  }

  // custom test case for if else then
  test("if else then") {
    expectValidGrammar("""
      if True then True else False
    """)
  }

  // custom test case for block
  test("Block") {
    expectValidGrammar("""
      {b = True;a = False;}
    """)
  }

  // custom test case for variable declaration
  test("Variable Declaration") {
    expectValidGrammar("""
      {$yolo = 1;}
    """)
  }

  // custom test case for list
  test("List") {
    expectValidGrammar("""
      {$yolo = [1, 2, 3];}
    """)
  }

  // custom test case for record dec
  test("Record with 1 Dec") {
    expectValidGrammar("""
      object{$a=1;}
    """)
  }

  // custom test case for record def
  test("Record with 2 Dec") {
    expectValidGrammar("""
      object{$a=1;$yolo=5;}
    """)
  }

  // custom test case for function call
  test("Function call") {
    expectValidGrammar("""
      foo?(param1, param2)
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

  test("Parser long program") {
    expectValidGrammar("""
      fun fib(i) = if lt?(i, 2) then 1 else add(fib(sub(i, 1)), fib(sub(i, 2)));

     fun buildList(i, list) = {
       if eq?(i, 0) then
         list
       else {
         $num = fib(i);
         {
           $i = 1;
         };
         list = build(num, list);
         buildList(sub(i, 1), list);
       };
     };

     fun getData(i) = {
       $obj = object {
         $l = buildList(i, []);
         $i = i;
       };
     };
     getData(10).l
    """)
  }

  test("Parser record access in all ways") {
    expectValidGrammar("""
       fun create(x, y) = object {
         $x = x;
         $y = y;
       };

       fun useless(x) = x;

       fun addVectors(a, b) = create(add(useless(a).x, b.x), add(a.y, b.y));

       fun foo(x) = (if eq?(x, 0) then create(1, 1) else create(2, x)).y;

       fun bar(x) = {
         $result = 0;
         if (eq?(x, 0)) then {
           result = create(1, 1);
         } else
           result = create(2, x);
         result;
       }.y;

       add(foo(0), bar(3))
      """)
  }

  test("Parser test2.exp") {
    expectValidGrammar("""
      fun getResult(list) = {
       $var = first(list);
       if eq?(var, 0) then {
               $var = build(1, list);
               list = if eq?([], rest(list)) then [1, 2, 3] else var;
           }
           else
               var = list = [];
      };
      getResult([0, 2, 4])
    """)
  }

  test("Wrong test") {
    expectInvalidGrammar("""
      fun test() = {
        $assp = 2
      };
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

  test("Interpreter program with user defined functions EASY") {
    expectResult("""
      fun toList(a) = [a];
      toList(4)
    """,
      ExpList(List(ExpInteger(4))))
  }
  test("Interpreter program with user defined functions") {
    expectResult("""
      fun sum(a) = if lt?(a, 1) then 0 else add(sum(add(a, -1)), a);
      fun toList(a) = [sum(a), a];
      toList(4)
    """,
    ExpList(List(ExpInteger(10), ExpInteger(4))))
  }

  test("Var Dec 1") {
    expectResult("""
                {
      $a = 1;
      $b = 2;
      add(a,b);
      }

    """,
      ExpInteger(3))
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

  test("Parser comments") {
    expectValidGrammar("""
      add(#)
      1, 2)##
      # 0
    """)
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
