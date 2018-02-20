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

val s = Set(Minus, Times, Div)
val bop = Minus
s contains bop

iterateStep(parse(
  "const cons = x => y => z => (z === 0 ? x : y);" +
    "const car = x=>x(0);" +
    "const cdr = x=>x(1);" +
    "const p = cons(1)(2);" +
    "console.log(car(p));" +
    "console.log(cdr(p));"
))

val program = parse(
  "const a = 0;" +
  "const apple  = (dummy) => console.log(a);" +
  "const banana = (a) => apple(0);" +
  "apple(0), banana(5);"
)

eval(empty, program)
iterateStep(program)
extend(extend(empty, "x", N(1)), "x", N(2))
