/*
 * CSCI 3155: Lab 4 Worksheet
 *
 * This worksheet demonstrates how you could experiment
 * interactively with your implementations in Lab4.scala.
 */

// Imports the parse function from jsy.lab1.Parser
import jsy.lab4.Parser.parse

// Imports the ast nodes
import jsy.lab4.ast._

// Imports all of the functions form jsy.student.Lab2 (your implementations in Lab2.scala)
import jsy.student.Lab4._

// Try compressRec
//val cr1 = compressRec(List(1, 2, 2, 3, 3, 3))

// Parse functions with possibly multiple parameters and type annotations.
parse("function fst(x: number, y: number): number { return x }")
parse("function (x: number) { return x }")
parse("function (f: (y: number) => number, x: number) { return f(x) }")

// Parse objects
parse("{ f: 0, g: true }")
parse("x.f")

val l1 = List(1, 2, 2, 3, 3, 3)
compressRec(l1)
treeFromList(List(1,2))

val c = 1
val (acc, prev) = (true, Some(2))


strictlyOrdered(treeFromList(List(1,1,2)))

strictlyOrdered(treeFromList(List(1,2)))

parse("''")


//List(("a",1),("b",2)).foldLeft(empty[Int]){(a, v) =>
//  val (x, t) = v
//  extend(a, x, t)
//}
//
//MTyp(MConst, TNumber).t

parse("()=>1")

List() == Nil