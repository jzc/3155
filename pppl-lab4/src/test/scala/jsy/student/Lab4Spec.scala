package jsy.student

import jsy.lab4.Lab4Like
import jsy.lab4.ast._
import jsy.tester.JavascriptyTester
import jsy.lab4.Parser.parse
import org.scalatest._

class Lab4Spec(lab4: Lab4Like) extends FlatSpec {
  import lab4._

  /***** Higher-Function Exercises Tests *****/

  "compressRec/compressFold" should "compress List(1, 2, 2, 3, 3, 3)" in {
    val l1 = List(1, 2, 2, 3, 3, 3)
    val gold1 = List(1, 2, 3)
    assertResult(gold1) { compressRec(l1) }
    assertResult(gold1) { compressFold(l1) }
  } 
  
  "mapFirst" should "map the first element where f returns Some" in {
     val l1 = List(1, 2, -3, 4, -5)
     val gold1 = List(1, 2, 3, 4, -5)
     assertResult(gold1) {
       mapFirst(l1) { (i: Int) => if (i < 0) Some(-i) else None }
     }
  }
  
  "foldLeft" should "enable implementing treeFromList and sum" in {
    assertResult(6){
      sum(treeFromList(List(1, 2, 3)))
    }
  }

  "strictlyOrdered" should "check strict ordering of a binary search tree" in {
    assert(!strictlyOrdered(treeFromList(List(1,1,2))))
    assert(strictlyOrdered(treeFromList(List(1,2))))
  }


  /***** Interpreter Tests *****/

  {
    val xtype = TNumber
    val tenvx = extend(empty, "x", xtype)

    val numbers = List(
      parse("1"),
      parse("1+2*3/4"),
      parse("(1<2),3"),
      parse("true ? 1 : 2")
    )

    "TypeVar" should "perform TypeVar" in {
      assertResult(xtype) {
        typeof(tenvx, Var("x"))
      }
    }

    "TypeNumber" should "perform TypeNumber" in {
      assertResult(TNumber) {
        typeof(empty, N(5))
      }
    }

    "TypeBool" should "perform TypeBool" in {
      assertResult(TBool) {
        typeof(empty, B(false))
      }
    }

    "TypeString" should "perform TypeString" in {
      assertResult(TString) {
        typeof(empty, S("hello world"))
      }
    }

    "TypeUndefined" should "perform TypeUndefined" in {
      assertResult(TUndefined) {
        typeof(empty, Undefined)
      }
    }

    "TypeNeg" should "perform TypeNeg" in {
      numbers.foreach((e1) => {
        assertResult(TNumber) {
          typeof(empty, Unary(Neg,e1))
        }
      })
    }

    // Probably want to write some more tests for typeInfer, substitute, and step.

  }

}

// An adapter class to pass in your Lab4 object.
class Lab4SpecRunner extends Lab4Spec(jsy.student.Lab4)

// The next bit of code runs a test for each .jsy file in src/test/resources/lab4.
// The test expects a corresponding .ans file with the expected result.
class Lab4JsyTests extends JavascriptyTester(None, "lab4", jsy.student.Lab4)

class Lab4Suite extends Suites(
  new Lab4SpecRunner,
  new Lab4JsyTests
)