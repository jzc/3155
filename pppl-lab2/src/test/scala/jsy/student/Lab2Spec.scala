package jsy.student

import jsy.lab2.Lab2Like
import jsy.lab2.ast._
import jsy.tester.JavascriptyTester
import org.scalatest._

/*
 * This file contains a number of *Spec classes that define a set of
 * tests.
 *
 * All of the tests are gathered together in Lab2Suite.
 */

class Lab2Spec(lab2: Lab2Like) extends FlatSpec {
  import lab2._

  "ToNumber" should "convert numbers correctly" in {
    assert(toNumber(N(1)) === 1)
    assert(toNumber(S("100")) === 100)
    assert(toNumber(S("bogus")).isNaN)

  }

  "toBoolean" should "convert to boolean correctly" in {
    assert(toBoolean(N(0)) === false)
    assert(toBoolean(N(-0)) === false)
    assert(toBoolean(N(Double.NaN)) === false)
    assert(toBoolean(Undefined) === false)
    assert(toBoolean(S("")) === false)
    assert(toBoolean(S("false")) === true)
    assert(toBoolean(B(false)) === false)
    assert(toBoolean(B(true)) === true)
    assert(toBoolean(N(4)) === true)
  }

  "And" should "return true only if both expressions are true" in {
    val t = B(true)
    val f = B(false)
    assert(eval(Binary(And,t,t)) === t)
    assert(eval(Binary(And,t,f)) === f)
    assert(eval(Binary(And,f,t)) === f)
    assert(eval(Binary(And,f,f)) === f)
  } 
 
  it should "return non-intuitive results from differing types" in {
    val e1 = N(0)
    val e2 = B(true)
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === N(0))
  }
 
  "Or" should "return true if either or both expressions are true" in {
    val t = B(true)
    val f = B(false)
    assert(eval(Binary(Or,t,t)) === t)
    assert(eval(Binary(Or,f,t)) === t)
    assert(eval(Binary(Or,t,f)) === t)
    assert(eval(Binary(Or,f,f)) === f)
  }

  it should "return non-intuitive results from differing types" in {
    val e1 = N(5)
    val e2 = B(false)
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === N(5))
  }
  
  "Plus" should "add two number values and return a number" in {
    val e1 = N(1)
    val e2 = N(2)
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === N(3))
  }

  it should "return the concatenation of two string with at least one string" in {
    assert(eval(Binary(Plus, N(5), S("foo"))) === S("5foo"))
    assert(eval(Binary(Plus, S("foo"), B(false))) === S("foofalse"))
    assert(eval(Binary(Plus, S("foo"), S("bar"))) === S("foobar"))
    assert(eval(Binary(Plus, N(5.1), S(""))) === S("5.1"))
  }

  "Minus" should "subtract two number values and return a number" in {
    val e1 = N(3)
    val e2 = N(1)
    val e3 = eval(Binary(Minus, e1, e2))
    assert(e3 === N(2))
  }

  "Times" should "multiply two number values and return a number" in {
    val e1 = N(3)
    val e2 = N(2)
    val e3 = eval(Binary(Times, e1, e2))
    assert(e3 === N(6))
  }

  "Div" should "divide two number values and return a number" in {
    val e1 = N(8)
    val e2 = N(5)
    val e3 = eval(Binary(Div, e1, e2))
    assert(e3 === N(1.6))
    assert(eval(Binary(Div, N(10), N(0))) === N(Double.PositiveInfinity))
    assert(eval(Binary(Div, N(-10), N(0))) === N(Double.NegativeInfinity))
  }

  "Arithmetic Operators" should "produce non-intuitive solutions given differing expression types" in {
    val e1 = B(true)
    val e2 = N(7)
    val z = 0
    assert(eval(Binary(Plus,e1,e2)) == N(8))
    assert(eval(Binary(Minus,S("10"),N(5))) === N(5))
  }

  "Eq" should "return true if two numerical values are the same" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(true))
  } 
  
  it should "return false if two numerical values are not the same" in {
    val e1 = N(5)
    val e2 = N(7)
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  "Ne" should "return true if two numerical values are different" in {
    val e1 = N(5)
    val e2 = N(7)
    val e3 = eval(Binary(Ne, e1, e2))
    assert(e3 === B(true))
  } 
  
  it should "return false if two numerical values are the same" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = eval(Binary(Ne, e1, e2))
    assert(e3 === B(false))
  }

  "Lt" should "return true if the first expression is less than the second" in {
    val e1 = N(5)
    val e2 = N(7)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(true))
  } 
  
  it should "return false if the first expression is not strictly less than the second" in {
    val e1 = N(7)
    val e2 = N(5)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  } 
  
  it should "return false if two number values are the same" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  "Le" should "return true if the first expression is less than the second" in {
    val e1 = N(5)
    val e2 = N(7)
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(true))
  } 
  
  it should "return false if the first expression is greater than the second" in {
    val e1 = N(7)
    val e2 = N(5)
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(false))
  } 
  
  it should "return true if two number values are the same" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(true))
  } 

  "Gt" should "return true if the first expression is greater than the second" in {
    val e1 = N(8)
    val e2 = N(7)
    val e3 = eval(Binary(Gt, e1, e2))
    assert(e3 === B(true))
  } 
  
  it should "return false if the first expression is not strictly greater than the second" in {
    val e1 = N(4)
    val e2 = N(5)
    val e3 = eval(Binary(Gt, e1, e2))
    assert(e3 === B(false))
  } 
  
  it should "return false if two number values are the same" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = eval(Binary(Gt, e1, e2))
    assert(e3 === B(false))
  } 

  "Ge" should "return true if the first expression is greater than the second" in {
    val e1 = N(8)
    val e2 = N(7)
    val e3 = eval(Binary(Ge, e1, e2))
    assert(e3 === B(true))
  } 
  
  it should "return false if the first expression is less than the second" in {
    val e1 = N(4)
    val e2 = N(5)
    val e3 = eval(Binary(Ge, e1, e2))
    assert(e3 === B(false))
  } 
  
  it should "return true if two number values are the same" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = eval(Binary(Ge, e1, e2))
    assert(e3 === B(true))
  }

  "Comparisons" should "produce non-intuitive results given the expressions given" in {
    val e1 = N(5)
    val e2 = Undefined
    assert(eval(Binary(Eq,e1,e2)) === B(false))
  }

  it should "compare strings based on lexiographical order" in {
    assert(eval(Binary(Gt, S("10"), S("100"))) === B(false))
    assert(eval(Binary(Ge, S("10"), S("100"))) === B(false))
    assert(eval(Binary(Ge, S("10"), S("10"))) === B(true))
    assert(eval(Binary(Lt, S("10"), S("1"))) === B(false))
    assert(eval(Binary(Le, S("10"), S("1"))) === B(false))
  }

  "ConstDecl" should "extend the environment with the first expression results bound to the identifier, and then eval the second expression" in {
    val e1 = N(3)
    val e2 = Binary(Plus, Var("x"), N(1))
    val e3 = eval(ConstDecl("x", e1, e2)) 
    assert(e3 === N(4))
  }
  
  "If" should "eval the first expression if the conditional is true" in {
    val e1 = Binary(Plus, N(3), N(2))
    val e2 = Binary(Plus, N(1), N(1))
    val e3 = eval(If(B(true), e1, e2)) 
    assert(e3 === N(5))
  } 
  
  it should "eval the second expression if the conditional is false" in {
    val e1 = Binary(Plus, N(3), N(2))
    val e2 = Binary(Plus, N(1), N(1))
    val e3 = eval(If(B(false), e1, e2)) 
    assert(e3 === N(2))
  } 
  
  "Seq" should "execute the first expression, followed by the second, and should eval to the second expression" in {
    val e1 = Binary(Plus, N(3), N(2))
    val e2 = Binary(Plus, N(1), N(1))
    val e3 = eval(Binary(Seq, e1, e2)) 
    assert(e3 === N(2))
  } 
  
  "Neg" should "return the negation of a number value" in {
    val e1 = N(5)
    val e2 = eval(Unary(Neg, e1))
    assert(e2 === N(-5))
  } 
  
  "Not" should "return the compliment of a boolean value" in {
    val e1 = B(true)
    val e2 = B(false)
    val e3 = eval(Unary(Not, e1))
    val e4 = eval(Unary(Not, e2))
    assert(e3 === B(false))
    assert(e4 === B(true))
  }
}

// An adapter class to pass in your Lab2 object.
class Lab2SpecRunner extends Lab2Spec(Lab2)

// The next bit of code runs a test for each .jsy file in src/test/resources/lab2.
// The test expects a corresponding .ans file with the expected result.
class Lab2JsyTests extends JavascriptyTester(None, "lab2", Lab2)

class Lab2Suite extends Suites(
  new Lab2SpecRunner,
  new Lab2JsyTests
)

