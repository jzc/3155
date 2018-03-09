package jsy.student

import jsy.lab4.Lab4Like

object Lab4 extends jsy.util.JsyApplication with Lab4Like {
  import jsy.lab4.ast._
  import jsy.lab4.Parser
  
  /*
   * CSCI 3155: Lab 4
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
   * '???' as needed to get something that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   */
  
  /* Collections and Higher-Order Functions */
  
  /* Lists */
  
  def compressRec[A](l: List[A]): List[A] = l match {
    case Nil | _ :: Nil => l
    case h1 :: (t1 @ (h2 :: _)) => if (h1 == h2) compressRec(t1) else h1 :: compressRec(t1)
  }
  
  def compressFold[A](l: List[A]): List[A] = l.foldRight(Nil: List[A]){
    (h, acc) => acc match{
      case Nil => h :: acc
      case h1 :: _ => if (h == h1) acc else h :: acc
    }
  }
  
  def mapFirst[A](l: List[A])(f: A => Option[A]): List[A] = l match {
    case Nil => Nil
    case h :: t => f(h) match {
      case Some(a) => a :: t
      case None => h :: mapFirst(t)(f)
    }
  }
  
  /* Trees */

  def foldLeft[A](t: Tree)(z: A)(f: (A, Int) => A): A = {
    def loop(acc: A, t: Tree): A = t match {
      case Empty => acc
      case Node(l, d, r) => loop(f(loop(acc,l),d), r)
    }
    loop(z, t)
  }

  // An example use of foldLeft
  def sum(t: Tree): Int = foldLeft(t)(0){ (acc, d) => acc + d }

  // Create a tree from a list. An example use of the
  // List.foldLeft method.
  def treeFromList(l: List[Int]): Tree =
    l.foldLeft(Empty: Tree){ (acc, i) => acc insert i }

  def strictlyOrdered(t: Tree): Boolean = {
    foldLeft(t)((true, None: Option[Int])) { case ((acc, prev), c) =>
      prev match {
        case Some(p) => (acc && (c > p), Some(c))
        case _ => (acc, Some(c))
      }
    }._1
  }

  /* Type Inference */

  // While this helper function is completely given, this function is
  // worth studying to see how library methods are used.
  def hasFunctionTyp(t: Typ): Boolean = t match {
    case TFunction(_, _) => true
      //ask about
    case TObj(fields) if (fields exists { case (_, t) => hasFunctionTyp(t) }) => true
    case _ => false
  }

  def typeof(env: TEnv, e: Expr): Typ = {
    def err[T](tgot: Typ, e1: Expr): T = throw StaticTypeError(tgot, e1, e)

    e match {
      case Print(e1) => typeof(env, e1); TUndefined
      case N(_) => TNumber
      case B(_) => TBool
      case Undefined => TUndefined
      case S(_) => TString
      case Var(x) => lookup(env, x)
      case Decl(mode, x, e1, e2) => typeof(extend(env, x, typeof(env, e1)), e2)
      case Unary(Neg, e1) => typeof(env, e1) match {
        case TNumber => TNumber
        case tgot => err(tgot, e1)
      }
      case Unary(Not, e1) => typeof(env, e1) match {
        case TBool => TBool
        case tgot => err(tgot, e1)
      }
      case Binary(Plus, e1, e2) => (typeof(env, e1), typeof(env, e2)) match {
        case (TNumber, TNumber) => TNumber
        case (TString, TString) => TString
        case (tgot, _) => err(tgot, e1)
      }
      case Binary(Minus|Times|Div, e1, e2) => (typeof(env, e1), typeof(env, e2)) match {
        case (TNumber, TNumber) => TNumber
        case (tgot, _) => err(tgot, e1)
      }
      case Binary(Eq|Ne, e1, e2) => {
        val t1 = typeof(env, e1)
        val t2 = typeof(env, e2)
        if (t1 == t2 && !hasFunctionTyp(t1) && !hasFunctionTyp(t2))
          TBool
        else
          err(t1, e1)
      }
      case Binary(Lt|Le|Gt|Ge, e1, e2) => (typeof(env, e1), typeof(env, e2)) match {
        case (TNumber, TNumber) | (TString, TString) => TBool
        case (_, tgot) => err(tgot, e2)
      }
      case Binary(And|Or, e1, e2) => (typeof(env, e1), typeof(env, e2)) match {
        case (TBool, TBool) => TBool
        case (_, tgot) => err(tgot, e2)
      }
      case Binary(Seq, e1, e2) => typeof(env, e1); typeof(env, e2)
      case If(e1, e2, e3) => typeof(env, e1) match {
        case TBool => {
          val t1 = typeof(env, e2)
          val t2 = typeof(env, e3)
          if (t1 == t2) t1 else err(t2, e3)
        }
        case tgot => err(tgot, e1)
      }
      case Function(p, params, tann, e1) => {

        // Bind to env1 an environment that extends env with an appropriate binding if
        // the function is potentially recursive.
        val env1 = (p, tann) match {
          /***** Add cases here *****/
          case (None, _) => env
          case (Some(p), Some(tann)) => extend(env, p, TFunction(params, tann))
          case _ => err(TUndefined, e1)
        }
        // Bind to env2 an environment that extends env1 with bindings for params.
        val env2 = params.foldLeft(env1) { case (acc, (x, MTyp(_, t))) => extend(acc, x, t) }
        // Infer the type of the function body
        val t1 = typeof(env2, e1)
        // Check with the possibly annotated return type
        tann match {
          case Some(tann) if tann != t1 => err(t1, e1)
          case _ => ()
        }
        TFunction(params, t1)
      }
      case Call(e1, args) => typeof(env, e1) match {
        case TFunction(params, tret) if params.length == args.length =>
          (params zip args).foreach {
              case ((_, MTyp(_,p)), a) => {
                val at = typeof(env, a)
                if (p != at) err(at, e1)
              }
          }
          tret
        case tgot => err(tgot, e1)
      }
      case Obj(fields) => TObj(fields.mapValues(typeof(env, _)))
      case GetField(e1, f) => typeof(env, e1) match {
        case t1 @ TObj(tfields) => tfields.get(f) match {
          case Some(t) => t
          case _ => err(t1, e1)
        }
        case tgot => err(tgot, e1)
      }
    }
  }
  
  
  /* Small-Step Interpreter */

  /*
   * Helper function that implements the semantics of inequality
   * operators Lt, Le, Gt, and Ge on values.
   *
   * We suggest a refactoring of code from Lab 2 to be able to
   * use this helper function in eval and step.
   *
   * This should the same code as from Lab 3.
   */
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean = {
    require(isValue(v1), s"inequalityVal: v1 ${v1} is not a value")
    require(isValue(v2), s"inequalityVal: v2 ${v2} is not a value")
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    (v1, v2) match {
      case _ => ??? // delete this line when done
    }
  }

  /* This should be the same code as from Lab 3 */
  def iterate(e0: Expr)(next: (Expr, Int) => Option[Expr]): Expr = {
    def loop(e: Expr, n: Int): Expr = next(e, n) match {
      case None => e
      case Some(ep) => loop(ep, n+1)
    }
    loop(e0, 0)
  }

  /* Capture-avoiding substitution in e replacing variables x with esub. */
  def substitute(e: Expr, esub: Expr, x: String): Expr = {
    def subst(e: Expr): Expr = e match {
      case N(_) | B(_) | Undefined | S(_) => e
      case Print(e1) => Print(substitute(e1, esub, x))
        /***** Cases from Lab 3 */
      case Unary(uop, e1) => Unary(uop, subst(e1))
      case Binary(bop, e1, e2) => Binary(bop, subst(e1), subst(e2))
      case If(e1, e2, e3) => If(subst(e1), subst(e2), subst(e3))
      case Var(y) => if (y == x) esub else e
      case Decl(mode, y, e1, e2) =>
        if (y == x)
          Decl(mode, y, subst(e1), e2)
        else
          Decl(mode, y, subst(e1), subst(e2))
        /***** Cases needing adapting from Lab 3 */
      case Function(p, params, tann, e1) => {
        val bound = p match {
          case Some(p) => p :: params.map { case (xi, _) => xi}
          case None => params.map { case (xi, _) => xi }
        }
        if (bound.contains(x))
          Function(p, params, tann, e1)
        else
          Function(p, params, tann, subst(e1))
      }
      case Call(e1, args) => Call(subst(e1), args.map(ei => subst(ei)))
        /***** New cases for Lab 4 */
      case Obj(fields) => Obj(fields.mapValues(ei => subst(ei)))
      case GetField(e1, f) => GetField(subst(e1), f)
    }

    val fvs = freeVars(esub)
    def fresh(x: String): String = if (fvs contains x) fresh(x + "$") else x
    subst(rename(e)(fresh))
  }

  /* Rename bound variables in e */
  def rename(e: Expr)(fresh: String => String): Expr = {
    def ren(env: Map[String,String], e: Expr): Expr = {
      e match {
        case N(_) | B(_) | Undefined | S(_) => e
        case Print(e1) => Print(ren(env, e1))

        case Unary(uop, e1) => Unary(uop, ren(env, e1))
        case Binary(bop, e1, e2) => Binary(bop, ren(env, e1), ren(env, e2))
        case If(e1, e2, e3) => If(ren(env, e1), ren(env, e2), ren(env, e3))

        case Var(y) => Var(env.get(y) match {
          case None => y
          case Some(yp) => yp
        })

        case Decl(mode, y, e1, e2) => {
          val yp = fresh(y)
          Decl(mode, yp, e1, ren(extend(env, y, yp), e2))
        }

        case Function(p, params, retty, e1) => {
          val (pp, envp): (Option[String], Map[String,String]) = p match {
            case None => (p, env)
            case Some(p) => {
              val pp = fresh(p)
              (Some(pp), extend(env, p, pp))
            }
          }
          val (paramsp, envpp) = params.foldRight( (Nil: List[(String,MTyp)], envp) ) {
            case ((xi, mt), (paramsi, envi)) => {
              val xip = fresh(xi)
              ((xip, mt) :: paramsi, extend(envi, xi, xip))
            }
          }
          Function(pp, paramsp, retty, ren(envpp, e1))
        }

        case Call(e1, args) => Call(ren(env, e1), args.map(ei => ren(env, ei)))

        case Obj(fields) => Obj(fields.mapValues(ei => ren(env, ei)))
        case GetField(e1, f) => GetField(ren(env, e1), f)
      }
    }
    ren(empty, e)
  }

  /* Check whether or not an expression is reduced enough to be applied given a mode. */
  def isRedex(mode: Mode, e: Expr): Boolean = mode match {
    case MConst => !isValue(e)
    case MName => false
  }

  def step(e: Expr): Expr = {
    require(!isValue(e), s"step: e ${e} to step is a value")
    e match {
      /* Base Cases: Do Rules */
      //DoPrint
      case Print(v1) if isValue(v1) => println(pretty(v1)); Undefined

      //DoNeg
      case Unary(Neg, N(n1)) => N(-n1)

      //DoNot
      case Unary(Not, B(b1)) => B(!b1)

      //DoSeq
      case Binary(Seq, v1, e2) if isValue(v1) => e2

      //DoArith
      case Binary(Plus , N(n1), N(n2)) => N(n1 + n2)
      case Binary(Minus, N(n1), N(n2)) => N(n1 - n2)
      case Binary(Times, N(n1), N(n2)) => N(n1 * n2)
      case Binary(Div  , N(n1), N(n2)) => N(n1 / n2)

      //DoPlusString
      case Binary(Plus, S(s1), S(s2)) => S(s1 + s2)

      //DoInequalityNumber
      case Binary(Lt, N(n1), N(n2)) => B(n1 <  n2)
      case Binary(Le, N(n1), N(n2)) => B(n1 <= n2)
      case Binary(Gt, N(n1), N(n2)) => B(n1 >  n2)
      case Binary(Ge, N(n1), N(n2)) => B(n1 >= n2)

      //DoInequalityString
      case Binary(Lt, S(s1), S(s2)) => B(s1 <  s2)
      case Binary(Le, S(s1), S(s2)) => B(s1 <= s2)
      case Binary(Gt, S(s1), S(s2)) => B(s1 >  s2)
      case Binary(Ge, S(s1), S(s2)) => B(s1 >= s2)

      //DoEquality
      case Binary(Eq, v1, v2) if isValue(v1) && isValue(v2) => B(v1 == v2)
      case Binary(Ne, v1, v2) if isValue(v1) && isValue(v2) => B(v1 != v2)

      //DoAndTrue
      case Binary(And, B(true), e2) => e2

      //DoAndFalse
      case Binary(And, B(false), e2) => B(false)

      //DoOrTrue
      case Binary(Or, B(true), e2) => B(true)

      //DoOrFalse
      case Binary(Or, B(false), e2) => e2

      //DoIfTrue
      case If(B(true), e2, _) => e2

      //DoIfFalse
      case If(B(false), _, e3) => e3

      //DoDecl
      case Call(v @ Function(p, params, _, e1), args) if params.zip(args).forall { case ((_, MTyp(m, _)), ei) => !isRedex(m, ei) } =>
        params.zip(args).foldLeft(p match {
          //DoCall - substitute all in one step?
          case None => e1
          //DoCallRec
          case Some(x) => substitute(e1, v, x)
        }){ case (acc, ((xi, _), ei)) => substitute(acc, ei, xi) }

      //DoGetField
      case GetField(Obj(fields), f) => fields(f)

      //SearchUnary
      case Unary(uop, e1) => Unary(uop, step(e1))

      //SearchBinary2
      case Binary(bop, v1, e2) if isValue(v1) => Binary(bop, v1, step(e2))

      //SearchBinary1
      case Binary(bop, e1, e2) => Binary(bop, step(e1), e2)

      //SearchPrint
      case Print(e1) => Print(step(e1))

      //SearchIf
      case If(e1, e2, e3) => If(step(e1), e2, e3)

      //SearchDecl
      //SearchCall1
      //SearchCall2
      case Call(v1, args) if isValue(v1) =>
        v1 match {
          case Function(p, params, _, e1) => {
            val pazip = params zip args
            if (???) {
              val e1p = pazip.foldRight(e1) {
                ???
              }
              p match {
                case None => ???
                case Some(x1) => ???
              }
            }
            else {
              val pazipp = mapFirst(pazip) {
                ???
              }
              ???
            }
          }
          case _ => throw StuckError(e)
        }

      //SearchObject
      case Obj(fields) => {
        val t = fields.find { case (_, ei) => !isValue(ei) }
        t match {
          case Some((fi, ei)) => Obj(extend(fields, fi, step(ei)))
          case _ => throw StuckError(e)
        }
      }

      //SearchGetFields
      case GetField(e1, f) => GetField(step(e1), f)

      //        /***** Cases needing adapting from Lab 3. */
//      case Unary(Neg, v1) if isValue(v1) => ???
//        /***** More cases here */
//      case Call(v1, args) if isValue(v1) =>
//        v1 match {
//          case Function(p, params, _, e1) => {
//            val pazip = params zip args
//            if (???) {
//              val e1p = pazip.foldRight(e1) {
//                ???
//              }
//              p match {
//                case None => ???
//                case Some(x1) => ???
//              }
//            }
//            else {
//              val pazipp = mapFirst(pazip) {
//                ???
//              }
//              ???
//            }
//          }
//          case _ => throw StuckError(e)
//        }
//        /***** New cases for Lab 4. */

      /* Inductive Cases: Search Rules */
//      case Print(e1) => Print(step(e1))
        /***** Cases from Lab 3. */
//      case Unary(uop, e1) => ???
        /***** More cases here */
        /***** Cases needing adapting from Lab 3 */
//      case Call(v1 @ Function(_, _, _, _), args) => ???
//      case Call(e1, args) => ???
        /***** New cases for Lab 4. */

      /* Everything else is a stuck error. Should not happen if e is well-typed.
       *
       * Tip: you might want to first develop by comment out the following line to see which
       * cases you have missing. You then uncomment this line when you are sure all the cases
       * that you have left the ones that should be stuck.
       */
      case _ => throw StuckError(e)
    }
  }
  
  
  /* External Interfaces */
  
  //this.debug = true // uncomment this if you want to print debugging information
  this.keepGoing = true // comment this out if you want to stop at first exception when processing a file
}

