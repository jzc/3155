package jsy.student

import jsy.lab2.Lab2Like

object Lab2 extends jsy.util.JsyApplication with Lab2Like {
  import jsy.lab2.Parser
  import jsy.lab2.ast._

  /*
   * CSCI 3155: Lab 2
   * Justin Cai
   * 
   * Partner: <Your Partner's Name>
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace the '???' expression with  your code in each function.
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
   *
   */

  /* We represent a variable environment as a map from a string of the
   * variable name to the value to which it is bound.
   * 
   * You may use the following provided helper functions to manipulate
   * environments, which are just thin wrappers around the Map type
   * in the Scala standard library.  You can use the Scala standard
   * library directly, but these are the only interfaces that you
   * need.
   */



  /* Some useful Scala methods for working with Scala values include:
   * - Double.NaN
   * - s.toDouble (for s: String)
   * - n.isNaN (for n: Double)
   * - n.isWhole (for n: Double)
   * - s (for n: Double)
   * - s format n (for s: String [a format string like for printf], n: Double)
   *
   * You can catch an exception in Scala using:
   * try ... catch { case ... => ... }
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
    }
  }

  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n.toString
      case B(b) => b.toString
      case S(s) => s
      case Undefined => "undefined"
    }
  }

  def eval(env: Env, e: Expr): Expr = {
    e match {
      /* Base Cases */
      case N(n) => N(n)
      case B(b) => B(b)
      case S(s) => S(s)
      case Var(x) => lookup(env, x)
      case Undefined => Undefined
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
      case Binary(bop, e1, e2) => {
        val ee1 = eval(env, e1)
        val ee2 = eval(env, e2)
        (bop: @unchecked) match {
          case Plus  => N(toNumber(ee1) + toNumber(ee2))
          case Minus => N(toNumber(ee1) - toNumber(ee2))
          case Times => N(toNumber(ee1) * toNumber(ee2))
          case Div   => N(toNumber(ee1) / toNumber(ee2))
          case Eq => (ee1, ee2) match {
            case (N(n1), N(n2)) => B(n1==n2)
            case (B(b1), B(b2)) => B(b1==b2)
            case (S(s1), S(s2)) => B(s1==s2)
            case (Undefined, Undefined) => B(true)
            case _ => B(false)
          }
          case Ne => (ee1, ee2) match {
            case (N(n1), N(n2)) => B(n1!=n2)
            case (B(b1), B(b2)) => B(b1!=b2)
            case (S(s1), S(s2)) => B(s1!=s2)
            case (Undefined, Undefined) => B(false)
            case _ => B(true)
          }
          case Lt => B(toNumber(ee1) <  toNumber(ee2))
          case Le => B(toNumber(ee1) <= toNumber(ee2))
          case Gt => B(toNumber(ee1) >  toNumber(ee2))
          case Ge => B(toNumber(ee1) >= toNumber(ee2))
          case Seq => ee2
        }
      }
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
      /* Inductive Cases */
      case Print(e1) => println(pretty(eval(env, e1))); Undefined
    }
  }



  /* Interface to run your interpreter from the command-line.  You can ignore what's below. */
  def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }

    val expr = Parser.parseFile(file)

    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }

    if (debug) { println("Evaluating ...") }

    val v = eval(expr)

     println(pretty(v))
  }

}
