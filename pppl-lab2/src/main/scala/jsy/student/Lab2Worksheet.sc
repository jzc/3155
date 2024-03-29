/*
 * CSCI 3155: Lab 2 Worksheet
 *
 * This worksheet demonstrates how you could experiment
 * interactively with your implementations in Lab2.scala.
 */

// Imports the parse function from jsy.lab1.Parser
import jsy.lab2.Parser.parse

// Imports the ast nodes
import jsy.lab2.ast._

// Imports all of the functions form jsy.student.Lab2 (your implementations in Lab2.scala)
import jsy.student.Lab2._

// Call the JavaScripty parser (from the provided library) on a string
val negFourAST = parse("-4")

// Evaluate that JavaScripty expression.
//eval(negFourAST)

// For convenience, we also have an eval function that takes a string,
// which calls the parser and then delegates to your eval function.
//eval("undefined + 1")
(1.1).toInt == 1
(1.0).toInt.toString

S("1") == S("1")

eval(Binary(And, Print(N(1)), Print(N(2))))

if ((1 << 3 - 2) == ((1 << 3) - 2))
  println("Left shift higher")
else if ((1 << 3 - 2) == (1 << (3 - 2)))
  println("Minus higher")
