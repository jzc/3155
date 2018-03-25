/*
 * CSCI 3155: Lab 5 Worksheet
 *
 * This worksheet demonstrates how you could experiment
 * interactively with your implementations in Lab5.scala.
 */

// Imports the parse function from jsy.lab1.Parser
import jsy.lab5.Parser.parse

// Imports the ast nodes
import jsy.lab5.ast._

// Imports all of the functions form jsy.student.Lab2 (your implementations in Lab2.scala)
import jsy.student.Lab5._

// Parse code with assignments
parse("var x = 1; x = 2; console.log(x)")
parse("const x = {f: 1}; x.f = 2; console.log(x.f)")

// Parse code with null
parse("null")
parse("<Null>null")
parse("<{f: number}>null")

// Parse functions
parse("(x: number) => x")
parse("function (x: var number) { x = 0; return x }")
parse("var y = 1; (function (x: ref number) { x = 0; return x })(y)")
parse("((x: name number) => 0)(console.log(100))")

// Aliasing example
val aliasingex = parse("""
  const x = { f: 1 }
  const y = x
  x.f = 2
  console.log(y.f)
""")
//iterateStep(aliasingex) // uncomment when you are ready to test your step function.
//
//def compress(s: List[Char]):String = {
//  def f(s: List[Char], c: Option[Char], n: Int): String = s match {
//    case h1 :: (t @ (h2 :: _)) => if (h1 == h2) f(t, Some(h1), n+1) else
//  }
//}

def strToCharList(s: String): List[Char] = s.foldRight(Nil:List[Char]) { case (c, acc) => c :: acc }

def compress(s: String): String = {
  strToCharList(s).foldLeft((None:Option[(Char,Int)],"")) {
    case ((prev, acc), c) =>  prev match {
      case None => (Some(c, 1), "")
      case Some((pc, n)) =>
        if (pc == c)
          (Some((pc, n+1)), acc)
        else
          (Some((c, 1)), acc+pc+n.toString)
    }
  } match {
    case (Some((pc, n)), s) => s+pc+n.toString
  }
}
//
def removeDuplicates[A](xs: List[A]):List[A] = {
  def removeX(xs:List[A], x:A) = xs match {
    case Nil => Nil
    case h :: t => if (h == x) t else h :: t
  }
}

//compress("abcdddddddddefg")
val a = List(1,1,1,2,2,2,3,3,3)

//strToCharList("aaabbc")

//strToCharList("aaabbc").foldLeft((None:Option[(Char,Int)],"")) {
//  case ((prev, acc), c) =>  prev match {
//    case None => (Some(c, 1), "")
//    case Some((pc, n)) =>
//      if (pc == c)
//        (Some((c, n+1)), acc)
//      else
//        (Some((c, 1)), acc+pc+n.toString)
//  }
//}