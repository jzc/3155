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

parse("function (x:{a:number,b:number}) { return x.a+x.b } ")

//parse("1=1")

//val tf1 = Map("x"->MTyp(MConst, TNumber), "y"->MTyp(MConst, TString))
//val tf2 = Map("x"->MTyp(MConst, TNumber), "z"->MTyp(MConst, TString))

step(parse("1+1"))
//parse("*1;")