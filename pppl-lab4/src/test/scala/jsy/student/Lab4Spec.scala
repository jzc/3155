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

    it should "throw ste if e1 is not bool"

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

    it should "throw ste if e1 and e2 are not string"

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

    it should "throw ste if e1 and e2 are not number"

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

    it should "throw ste if e1 and e2 are not string"

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

    it should "throw ste when there is a function types" in {

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

    it should "throw ste if e1 and e2 are not bools"

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

    val obj = Obj(Map(
      ("a", Binary(Plus, N(1), N(2))),
      ("b", Binary(Eq, N(1), N(2))),
      ("c", Print(S("Hello"))),
      ("d", S("World"))
    ))
    "TypeGetField" should "perform TypeGetField" in {
      assertResult(TNumber) {
        typeof(empty, GetField(obj, "a"))
      }
      assertResult(TBool) {
        typeof(empty, GetField(obj, "b"))
      }
      assertResult(TUndefined) {
        typeof(empty, GetField(obj, "c"))
      }
      assertResult(TString) {
        typeof(empty, GetField(obj, "d"))
      }
    }

    it should "throw ste if e1 is not obj" in {
      intercept[StaticTypeError] {
        typeof(empty, GetField(N(1), "a"))
      }
    }

    it should "throw ste if field does not exist" in {
      intercept[StaticTypeError] {
        typeof(empty, GetField(obj, "x"))
      }
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
        Lab4.bindParams(params, empty)
      }
    }

    it should "throw ste if type annotation is different from infered" in {
      intercept[StaticTypeError] {
        typeof(empty, Function(None, Nil, Some(TNumber), S("hello")))
      }
    }

    it should "infer type of anonymous function" in {
      assertResult(TFunction(Nil, TNumber)) {
        typeof(empty, Function(None, Nil, None, N(1)))
      }
      assertResult(TFunction(List(("x", MTyp(MConst, TString))), TString)) {
        typeof(empty, Function(None, List(("x", MTyp(MConst, TString))), None, Var("x")))
      }
    }

    it should "perform TypeFunction" in {
      assertResult(TFunction(Nil, TNumber)) {
        typeof(empty, Function(None, Nil, Some(TNumber), N(1)))
      }
    }

    {
      val f = Function(
        None,
        List(("x", MTyp(MConst, TNumber)), ("y", MTyp(MConst, TNumber)), ("z", MTyp(MConst, TNumber))),
        None,
        Binary(Plus, Binary(Plus, Var("x"), Var("y")), Var("z"))
      )
      "TypeCall" should "should throw ste if incorrect number of params" in {
        intercept[StaticTypeError] {
          typeof(empty, Call(f, Nil))
        }
      }

      it should "throw ste if param types dont match" in {
        intercept[StaticTypeError] {
          typeof(empty, Call(f, List(N(1),S("2"),N(3))))
        }
        intercept[StaticTypeError] {
          typeof(empty, Call(f, List(S("1"),N(2),N(3))))
        }
        intercept[StaticTypeError] {
          typeof(empty, Call(f, List(N(1),N(2),S("3"))))
        }
      }

      it should "perform TypeCall" in {
        assertResult(TNumber) {
          typeof(empty, Call(f, List(N(1),N(2),N(3))))
        }
      }
    }


    "TypeObject" should "perform TypeObject" in {
      val fields = Map(("a", TNumber), ("b", TBool), ("c", TUndefined), ("d", TString))
      assertResult(TObj(fields)) {
        typeof(empty, obj)
      }
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