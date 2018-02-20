/*
 * CSCI 3155: Lab 3 Worksheet
 *
 * This worksheet demonstrates how you could experiment
 * interactively with your implementations in Lab3.scala.
 */

// Imports the parse function from jsy.lab1.Parser
import jsy.lab3.Parser.parse

// Imports the ast nodes
import jsy.lab3.ast._

// Imports all of the functions form jsy.student.Lab2 (your implementations in Lab2.scala)
import jsy.student.Lab3._

// Parse a function
parse("x => x")
parse("(x) => x")
parse("")
parse("function id(x) { return x }; id(1)")
parse("x => y => x + y")
parse("x => { const z = 3; return x + z }")
parse("function (x) { const z = 3; return x + z }")

Binary(Plus, N(0), N(2)) match {
  case Binary(_, N(1)|N(2)|N(3), _) => true
  case _ => false
}

//val s = Set(Minus, Times, Div)
//val bop = Minus
//s contains bop

//iterateStep(parse(
//  "const cons = x => y => z => (z === 0 ? x : y);" +
//    "const car = x=>x(0);" +
//    "const cdr = x=>x(1);" +
//    "const p = cons(1)(2);" +
//    "console.log(car(p));" +
//    "console.log(cdr(p));"
//))
//
//val program = parse(
//  "const a = 0;" +
//  "const apple  = (dummy) => console.log(a);" +
//  "const banana = (a) => apple(0);" +
//  "apple(0), banana(5);"
//)
//
//eval(empty, program)
//iterateStep(program)
//extend(extend(empty, "x", N(1)), "x", N(2))


eval(empty, parse(
  "const a = 1; const a = 2; console.log(a);"
))

parse("const a = 1")

iterateStep(parse("const g = function g(x) { return (x===0) ? 1 : x * g(x-1) }; g(3)"))


iterateStep(parse("console.log(3)+console.log(5)+console.log(4)"))

def f(x:Int): Int = {
  def f(x: Int): Int = 1
  f(1)
}

f(2)

/*
* function f(x) {
*   console.log(x);
*   const f = f;
*
* }(1)
* */