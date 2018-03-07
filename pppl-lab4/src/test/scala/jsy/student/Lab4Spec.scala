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
      parse("1+2*3/4")
//      parse("(1<2),3"),
//      parse("true ? 1 : 2")
    )

    val strings = List(
      parse("'1'"),
      parse("'1'+'1'")
    )

    val bools = List(
      parse("true"),
      parse("false")
    )

    var undefineds = List(
      parse("undefined"),
      parse("console.log(1)")
    )

    var notnumbers = strings ::: bools ::: undefineds
    var notfuncs = numbers ::: strings ::: bools ::: undefineds

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

    it should "throw ste when e1 is not a number" in {
      notnumbers.foreach(e1 => {
        intercept[StaticTypeError] {
          typeof(empty, Unary(Neg, e1))
        }
      })
    }

    "TypeNot" should "perform TypeNot" in {

    }

    "TypeSeq" should "perform TypeSeq" in {

    }

    val arith = List(Plus, Minus, Times, Div)

    "TypeArith" should "perform TypeArith" in {
      numbers.zip(numbers).foreach(t => {
        val (e1, e2) = t
        arith.foreach(bop => {
          assertResult(TNumber) {
            typeof(empty, Binary(bop, e1, e2))
          }
        })
      })
    }

    it should "throw ste when not both numbers" in {
      List(
        numbers.zip(strings),
        numbers.zip(bools),
        numbers.zip(undefineds)
      ).foreach(c => c.foreach(t => {
        val (e1, e2) = t
        arith.foreach(bop => {
          intercept[StaticTypeError] {
            typeof(empty, Binary(bop, e1, e2))
          }
        })
      }))
    }

    "TypePlusString" should "perform TypePlusString" in {
      strings.zip(strings).foreach(t => {
        val (e1, e2) = t
        assertResult(TString) {
          typeof(empty, Binary(Plus, e1, e2))
        }
      })
    }

    "TypeInequalityNumber" should "perform TypeInequalityNumber" in {
      numbers.zip(numbers).foreach(t => {
        val (e1, e2) = t
        List(Lt, Le, Gt, Ge).foreach(bop => {
          assertResult(TBool) {
            typeof(empty, Binary(bop, e1, e2))
          }
        })
      })
    }

    "TypeInequalityString" should "perform TypeInequalityString" in {
      strings.zip(strings).foreach(t => {
        val (e1, e2) = t
        List(Lt, Le, Gt, Ge).foreach(bop => {
          assertResult(TBool) {
            typeof(empty, Binary(bop, e1, e2))
          }
        })
      })
    }

    "TypeEquality" should "perform TypeEquality" in {
      notfuncs.zip(notfuncs).foreach(t => {
        val (e1, e2) = t
        List(Eq, Ne).foreach(bop => {
          assertResult(TBool) {
            typeof(empty, Binary(bop, e1, e2))
          }
        })
      })
    }

    it should "throw ste when there is a function" in {

    }

    "TypeAndOr" should "perform TypeAndOr" in {
      bools.zip(bools).foreach(t => {
        val (e1, e2) = t
        List(And, Or).foreach(bop => {
          assertResult(TBool) {
            typeof(empty, Binary(bop, e1, e2))
          }
        })
      })
    }

    "TypeIf" should "perform TypeIf" in {
      intercept[StaticTypeError] {
        typeof(empty, If(N(1), N(2), N(3)))
      }
      intercept[StaticTypeError] {
        typeof(empty, If(B(false), N(1), S("2")))
      }
      intercept[StaticTypeError] {
        typeof(empty, If(B(false), S("1"), N(2)))
      }
    }

    "TypePrint" should "perform TypePrint" in {
      assertResult(TUndefined) {
        typeof(empty, Print(N(1)))
      }
    }

    "TypeDecl" should "perform TypeDecl" in {
      assertResult(TNumber) {
        typeof(empty, Decl(MConst, "x", N(1), Var("x")))
      }
      assertResult(TString) {
        typeof(empty, Decl(MConst, "x", N(1), S("hello")))
      }
    }

    "TypeCall" should "perform TypeCall" in {

    }

    "TypeGetField" should "perform TypeGetField" in {

    }

    "TypeFunctinon" should "throw ste if no type annotation and named" in {
      intercept[StaticTypeError] {
        typeof(empty, Function(Some("f"), Nil, None, N(1)))
      }
    }

    it should "bind the parameters correctly" in {
      val params = List(("a",MTyp(MConst, TNumber)), ("b", MTyp(MName, TString)))
      val correct = Map(("a", TNumber), ("b", TString))
      assertResult(correct) {
        bindParams(params, empty)
      }
    }

    "TypeCall" should "performTypeCall" in {

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