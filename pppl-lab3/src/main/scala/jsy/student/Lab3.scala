package jsy.student

import jsy.lab3.Lab3Like
import jsy.util.JsyApplication

object Lab3 extends JsyApplication with Lab3Like {
  import jsy.lab3.ast._
  
  /*
   * CSCI 3155: Lab 3 
   * <Your Name>
   * 
   * Partner: <Your Partner's Name>
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace the '???' expression with your code in each function.
   *
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your lab will not be graded if it does not compile.
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert. Simply put in a
   * '???' as needed to get something  that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   */
  
  /*
   * The implementations of these helper functions for conversions can come
   * Lab 2. The definitions for the new value type for Function are given.
   */
  
  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case B(b) => if (b) 1 else 0
      case S(s) => try s.toDouble catch {
        case _: Throwable => Double.NaN
      }
      case Undefined => Double.NaN
      case Function(_, _, _) => Double.NaN
    }
  }
  
  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      case N(n) => !n.isNaN && n != 0
      case S("") => false
      case S(s) => true
      case Undefined => false
      case Function(_, _, _) => true
    }
  }
  
  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => if (n.toInt == n) n.toInt.toString else n.toString
      case B(b) => b.toString
      case S(s) => s
      case Undefined => "undefined"
      case Function(_, _, _) => "function"
    }
  }

  /*
   * Helper function that implements the semantics of inequality
   * operators Lt, Le, Gt, and Ge on values.
   *
   * We suggest a refactoring of code from Lab 2 to be able to
   * use this helper function in eval and step.
   */
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean = {
    require(isValue(v1))
    require(isValue(v2))
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    (v1, v2) match {
      case _ => ??? // delete this line when done
    }
  }


  /* Big-Step Interpreter with Dynamic Scoping */
  
  /*
   * Start by copying your code from Lab 2 here.
   */
  def eval(env: Env, e: Expr): Expr = {
    e match {
      /* Base Cases */
      case N(_) | B(_) | S(_) | Undefined | Function(_, _, _) => e
      case Var(x) => lookup(env, x)
      case Undefined => Undefined

      /* And and Or (seperate from other bops because they don't always eval both expressions */
      case Binary(And, e1, e2) => {
        val ee1 = eval(env, e1)
        if (toBoolean(ee1) == false)
          ee1
        else
          eval(env, e2)
      }
      case Binary(Or, e1, e2) => {
        val ee1 = eval(env, e1)
        if (toBoolean(ee1) == true)
          ee1
        else
          eval(env, e2)
      }

      /* All other bops (evaluate both subexpressions) */
      case Binary(bop, e1, e2) => {
        val ee1 = eval(env, e1)
        val ee2 = eval(env, e2)
        (bop: @unchecked) match {
          case Plus  => (ee1, ee2) match {
            case (S(_), _) | (_, S(_)) => S(toStr(ee1) + toStr(ee2))
            case _ => N(toNumber(ee1) + toNumber(ee2))
          }
          case Minus => N(toNumber(ee1) - toNumber(ee2))
          case Times => N(toNumber(ee1) * toNumber(ee2))
          case Div   => N(toNumber(ee1) / toNumber(ee2))
          case Eq => B(ee1 == ee2)
          case Ne => B(ee1 != ee2)
          case Lt => (ee1, ee2) match {
            case (S(s1), S(s2)) => B(s1 < s2)
            case _ =>   B(toNumber(ee1) < toNumber(ee2))
          }
          case Le => (ee1, ee2) match {
            case (S(s1), S(s2)) => B(s1 <= s2)
            case _ =>   B(toNumber(ee1) <= toNumber(ee2))
          }
          case Gt => (ee1, ee2) match {
            case (S(s1), S(s2)) => B(s1 > s2)
            case _ =>   B(toNumber(ee1) > toNumber(ee2))
          }
          case Ge => (ee1, ee2) match {
            case (S(s1), S(s2)) => B(s1 >= s2)
            case _ =>   B(toNumber(ee1) >= toNumber(ee2))
          }
          case Seq => ee2
        }
      }

      /* Uops */
      case Unary(uop, e1) => {
        val ee1 = eval(env, e1)
        uop match {
          case Neg => N(-toNumber(ee1))
          case Not => B(!toBoolean(ee1))
        }
      }
      case ConstDecl(x, e1, e2) => {
        val newEnv = extend(env, x, eval(env, e1))
        eval(newEnv, e2)
      }
      case If(e1, e2, e3) =>
        if (toBoolean(eval(env, e1)))
          eval(env, e2)
        else
          eval(env, e3)
      case Print(e1) =>
        println(pretty(eval(env, e1)))
        Undefined
      case Call(e1, e2) => {
        val v1 = eval(env, e1)
        val v2 = eval(env, e2)
        v1 match {
          case Function(Some(x1), x2, eprime) => eval(extend(extend(env, x1, v1), x2, v2), eprime)
          case Function(None, x, eprime) => eval(extend(env, x, v2), eprime)
        }
      }
    }
  }
    

  /* Small-Step Interpreter with Static Scoping */

  def iterate(e0: Expr)(next: (Expr, Int) => Option[Expr]): Expr = {
    def loop(e: Expr, n: Int): Expr = next(e, n) match {
      case None => e
      case Some(e) => loop(e, n+1)
    }
    loop(e0, 0)
  }
  
  def substitute(e: Expr, v: Expr, x: String): Expr = {
    require(isValue(v))
    e match {
      case N(_) | B(_) | Undefined | S(_) => e
      case Print(e1) => Print(substitute(e1, v, x))
      case Unary(uop, e1) => Unary(uop, substitute(e1, v, x))
      case Binary(bop, e1, e2) => Binary(bop, substitute(e1, v, x), substitute(e2, v, x))
      case If(e1, e2, e3) => ???
      case Call(e1, e2) => ???
      case Var(y) => if (y==x) v else Var(y)
      case Function(None, y, e1) => ???
      case Function(Some  (y1), y2, e1) => ???
      case ConstDecl(y, e1, e2) => ???
    }
  }
    
  def step(e: Expr): Expr = {
    e match {
      /* Base Cases: Do Rules */

      //DoPrint
      case Print(v1) if isValue(v1) => println(pretty(v1)); Undefined

      //DoNeg
      case Unary(Neg, v1) if isValue(v1) => N(-toNumber(v1))

      //DoNot
      case Unary(Not, v1) if isValue(v1) => B(!toBoolean(v1))

      //DoSeq
      case Binary(Seq, v1, e2) if isValue(v1) => e2

      //DoPlusNumber
      case Binary(Plus, v1, v2) if isValue(v1) && !v1.isInstanceOf[S] && isValue(v2) && !v2.isInstanceOf[S] =>
        N(toNumber(v1) + toNumber(v2))

      //DoPlusString
      case Binary(Plus, S(s1), v2) if isValue(v2) => S(s1+toStr(v2))
      case Binary(Plus, v1, S(s2)) if isValue(v1) => S(toStr(v1)+s2)

      //DoArith
      case Binary(bop, v1, v2) if isValue(v1) && isValue(v2) => bop match {
        case Minus => N(toNumber(v1) - toNumber(v2))
        case Times => N(toNumber(v1) * toNumber(v2))
        case Div   => N(toNumber(v1) / toNumber(v2))
      }

      //DoInequalityString
      case Binary(Lt, S(s1), S(s2)) => B(s1 <  s2)
      case Binary(Le, S(s1), S(s2)) => B(s1 <= s2)
      case Binary(Gt, S(s1), S(s2)) => B(s1 >  s2)
      case Binary(Gt, S(s1), S(s2)) => B(s1 >= s2)

      //DoInequalityNumber
      case Binary(Lt, v1, v2) if isValue(v1) && isValue(v2) => B(toNumber(v1) <  toNumber(v2))
      case Binary(Le, v1, v2) if isValue(v1) && isValue(v2) => B(toNumber(v1) <= toNumber(v2))
      case Binary(Gt, v1, v2) if isValue(v1) && isValue(v2) => B(toNumber(v1) >  toNumber(v2))
      case Binary(Ge, v1, v2) if isValue(v1) && isValue(v2) => B(toNumber(v1) >= toNumber(v2))

      //DoEquality
      case Binary(Eq, v1, v2)
        if isValue(v1) && !v1.isInstanceOf[Function] && isValue(v2) && !v2.isInstanceOf[Function] =>
        B(v1 == v2)

      case Binary(Ne, v1, v2)
        if isValue(v1) && !v1.isInstanceOf[Function] && isValue(v2) && !v2.isInstanceOf[Function] =>
        B(v1 != v2)

      //DoAndTrue
      case Binary(And, v1, e2) if isValue(v1) && toBoolean(v1) => e2

      //DoAndFalse
      case Binary(And, v1, _)  if isValue(v1) && !toBoolean(v1) => v1

      //DoOrTrue
      case Binary(Or, v1, _)  if isValue(v1) && toBoolean(v1) => v1

      //DoOrFalse
      case Binary(Or, v1, e2) if isValue(v1) && !toBoolean(v1) => e2

      //DoIfTrue
      case If(v1, e2, _) if isValue(v1) && toBoolean(v1) => e2

      //DoIfFalse
      case If(v1, _, e3) if isValue(v1) && !toBoolean(v1) => e3

      /* Inductive Cases: Search Rules */
      case Print(e1) => Print(step(e1))
      case Unary(uop, e1) => Unary(uop, step(e1))


      // ****** Your cases here

      /* Cases that should never match. Your cases above should ensure this. */
      case Var(_) => throw new AssertionError("Gremlins: internal error, not closed expression.")
      case N(_) | B(_) | Undefined | S(_) | Function(_, _, _) => throw new AssertionError("Gremlins: internal error, step should not be called on values.");
    }
  }


  /* External Interfaces */
  
  //this.debug = true // uncomment this if you want to print debugging information
  this.keepGoing = true // comment this out if you want to stop at first exception when processing a file

}
