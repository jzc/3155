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

step(parse ("{f:3}.f"))


//val l1 = List(1, 2, 2, 3, 3, 3)
//compressRec(l1)
//treeFromList(List(1,2))
//
//val c = 1
//val (acc, prev) = (true, Some(2))
//
//
//strictlyOrdered(treeFromList(List(1,1,2)))
//
//strictlyOrdered(treeFromList(List(1,2)))
//
//parse("''")


//List(("a",1),("b",2)).foldLeft(empty[Int]){(a, v) =>
//  val (x, t) = v
//  extend(a, x, t)
//}
//
//MTyp(MConst, TNumber).t
//
//parse("()=>1")
//
//List() == Nil
//
//Map(("a",N(1)), ("b", N(2)), ("c", Binary(Plus,N(3),N(2)))).find { case (_, ei) => !isValue(ei) }
//
////typeof(empty, Binary(Le, N(1), N(2)))
////
//val f:MTyp=>Typ = { case MTyp(_,t) => t }; f(MTyp(MConst, TNumber))
//
//
//val g:MTyp=>Typ = t => t match { case MTyp(_,t) => t }
//
//val h:((Int, Int), Int)=>Int = t => {
//  val ((e1, e2), e3) = t
//  e1+e2+e3
//}
//
//val k:((Int, Int), Int)=>Int = { case ((e1, e2), e3) => e1+e2+e3 }
//def f(x:Int) = if (x==0) 0 else x*f(x-1)

step(parse ("{f:3}.f"))

