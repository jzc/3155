package jsy.student

import jsy.lab5.Lab5Like

object Lab5 extends jsy.util.JsyApplication with Lab5Like {
  import jsy.lab5.ast._
  import jsy.util.DoWith
  import jsy.util.DoWith._

  /*
   * CSCI 3155: Lab 5
   * Justin Cai
   *
   * Partner: Jarrod Raine
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

  /*** Exercise with DoWith ***/

  def rename[W](env: Map[String,String], e: Expr)(fresh: String => DoWith[W,String]): DoWith[W,Expr] = {
    def ren(env: Map[String,String], e: Expr): DoWith[W,Expr] = e match {
      case N(_) | B(_) | Undefined | S(_) | Null | A(_) => doreturn(e)
      case Print(e1) => ren(env,e1) map { e1p => Print(e1p) }

      case Unary(uop, e1) => ren(env, e1) map { e1p => Unary(uop, e1p)}
      case Binary(bop, e1, e2) => ren(env, e1) flatMap { e1p => ren(env, e2) map { e2p => Binary(bop, e1p, e2p) } }
      case If(e1, e2, e3) => ren(env, e1) flatMap { e1p => ren(env, e2) flatMap { e2p => ren(env, e3) map { e3p => If(e1p, e2p, e3p) } } }

      case Var(x) => doreturn(Var(env.getOrElse(x,x)))

//      case Decl(m, x, e1, e2) => fresh(x) flatMap { xp =>
//        ren(env ++ freeVars(e1).map { x => (x,x) }, e1) flatMap { e1p => ren(env + (x->xp), e2) map { e2p => Decl(m, xp, e1p, e2p)}}
////        ren(env, e1) flatMap { e1p => ren(env + (x->xp), e2) map { e2p => Decl(m, xp, e1p, e2p) } }
////        ren(env + (x->xp), e2) flatMap { e2p => doreturn(Decl(m, xp, e1, e2p)) }
//      }
      case Decl(m, x, e1, e2) => fresh(x) flatMap {
        xp => ren(env, e1) flatMap {
          e1p => ren(extend(env, x, xp), e2) map {
            e2p => Decl(m, xp, e1p, e2p)
          }
        }
      }
      case Function(p, params, retty, e1) => {
        val w: DoWith[W,(Option[String], Map[String,String])] = p match {
          case None => doreturn((None, env))
          case Some(x) => fresh(x) flatMap { xp => doreturn((Some(xp), env + (x->xp))) }
        }
        w flatMap { case (pp, envp) =>
          params.foldRight[DoWith[W,(List[(String,MTyp)],Map[String,String])]]( doreturn((Nil, envp)) ) {
            case ((x,mty), acc) => acc flatMap {
              case (params_acc, env_acc) => fresh(x) flatMap {
                xp => doreturn((xp, mty) :: params_acc, env_acc + (x->xp))
              }
            }
          } flatMap {
            case (paramsp, envpp) => ren(envpp, e1) flatMap {
              e1p => doreturn(Function(pp, paramsp, retty, e1))
            }
          }
        }
      }

      case Call(e1, args) => ren(env, e1) flatMap {
        e1p => mapWith(args) { ei => ren(env, ei) } flatMap { argsp => doreturn(Call(e1p, argsp)) }
      }

      case Obj(fields) => mapWith(fields) { case (fi, ei) => ren(env, ei) map { eip => (fi, eip) } } map { fieldsp => Obj(fieldsp) }
      case GetField(e1, f) => ren(env, e1) map {
        e1p => GetField(e1p, f)
      }

      case Assign(e1, e2) => ren(env, e1) flatMap { e1p => ren(env, e2) map { e2p => Assign(e1p, e2p) } }

      /* Should not match: should have been removed */
      case InterfaceDecl(_, _, _) => throw new IllegalArgumentException("Gremlins: Encountered unexpected expression %s.".format(e))
    }
    ren(env, e)
  }

  def myuniquify(e: Expr): Expr = {
    val fresh: String => DoWith[Int,String] = { _ =>
      doget flatMap { x => domodify {(x:Int)=>x+1} flatMap { _ => doreturn("x"+x.toString) } }
    }
    val (_, r) = rename(empty, e)(fresh)(0)
    r
  }

  /*** Helper: mapFirst to DoWith ***/

  // List map with an operator returning a DoWith
  def mapWith[W,A,B](l: List[A])(f: A => DoWith[W,B]): DoWith[W,List[B]] = {
    l.foldRight[DoWith[W,List[B]]]( doreturn(Nil) ) {
      case (a, dwb) => f(a) flatMap { ap => dwb flatMap { dwbp => doreturn(ap :: dwbp) }} //dwb.flatMap { bs => f(a)}
    }
  }

  // Map map with an operator returning a DoWith
  def mapWith[W,A,B,C,D](m: Map[A,B])(f: ((A,B)) => DoWith[W,(C,D)]): DoWith[W,Map[C,D]] = {
    m.foldRight[DoWith[W,Map[C,D]]]( doreturn(Map()) ) {
      case ((a,b), acc) => f((a,b)) flatMap { case (ap, bp) => acc flatMap { m => doreturn(m + (ap->bp)) } }
    }
  }

  // Just like mapFirst from Lab 4 but uses a callback f that returns a DoWith in the Some case.
  def mapFirstWith[W,A](l: List[A])(f: A => Option[DoWith[W,A]]): DoWith[W,List[A]] = l match {
    case Nil => doreturn(Nil)
    case h :: t => f(h) match {
      case None => doreturn(h).flatMap { h => mapFirstWith(t)(f) flatMap { tp => doreturn(h :: tp) } }//{ h => mapFirstWith(t) flatMap { tp => h :: tp } }
      case Some(p) => p flatMap { h => doreturn(t) flatMap { tp => doreturn(h :: tp) } }
    }
  }

  // There are better ways to deal with the combination of data structures like List, Map, and
  // DoWith, but we won't tackle that in this assignment.

  /*** Casting ***/

  def castOk(t1: Typ, t2: Typ): Boolean = (t1, t2) match {
      /***** Make sure to replace the case _ => ???. */
    case (t1, t2) if t1 == t2 => true
    case (TNull, TObj(_)) => true
    case (TObj(tf1), TObj(tf2)) =>
      val (small, large) = if (tf1.size <= tf2.size) (tf1, tf2) else (tf2, tf1)
      small.forall { case (fi, ti) => (large contains fi) && (large(fi) == ti) }
      /***** Cases for the extra credit. Do not attempt until the rest of the assignment is complete. */
    case (TInterface(tvar, t1p), _) => ???
    case (_, TInterface(tvar, t2p)) => ???
      /***** Otherwise, false. */
    case _ => false
  }

  /*** Type Inference ***/

  // A helper function to check whether a jsy type has a function type in it.
  // While this is completely given, this function is worth studying to see
  // how library functions are used.
  def hasFunctionTyp(t: Typ): Boolean = t match {
    case TFunction(_, _) => true
    case TObj(fields) if (fields exists { case (_, t) => hasFunctionTyp(t) }) => true
    case _ => false
  }

  def isBindex(m: Mode, e: Expr): Boolean = m match {
    case MConst | MName | MVar => true
    case MRef => isLExpr(e)
  }

  def typeof(env: TEnv, e: Expr): Typ = {
    def err[T](tgot: Typ, e1: Expr): T = throw StaticTypeError(tgot, e1, e)

    e match {
      case Print(e1) => typeof(env, e1); TUndefined
      case N(_) => TNumber
      case B(_) => TBool
      case Undefined => TUndefined
      case S(_) => TString
      case Var(x) => env(x).t
      case Unary(Neg, e1) => typeof(env, e1) match {
        case TNumber => TNumber
        case tgot => err(tgot, e1)
      }
        /***** Cases directly from Lab 4. We will minimize the test of these cases in Lab 5. */
      case Unary(Not, e1) => typeof(env, e1) match {
        case TBool => TBool
        case tgot => err(tgot, e1)
      }
      case Binary(Plus, e1, e2) => typeof(env, e1) match {
        case TNumber => typeof(env, e2) match {
          case TNumber => TNumber
          case tgot => err(tgot, e2)
        }
        case TString => typeof(env, e2) match {
          case TString => TString
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      case Binary(Minus|Times|Div, e1, e2) => typeof(env, e1) match {
        case TNumber => typeof(env, e2) match {
          case TNumber => TNumber
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      case Binary(Eq|Ne, e1, e2) => typeof(env, e1) match {
        case t1 if !hasFunctionTyp(t1) => typeof(env, e2) match {
          case t2 if !hasFunctionTyp(t2) && (t1 == t2) => TBool
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      case Binary(Lt|Le|Gt|Ge, e1, e2) => typeof(env, e1) match {
        case TNumber => typeof(env, e2) match {
          case TNumber => TBool
          case tgot => err(tgot, e2)
        }
        case TString => typeof(env, e2) match {
          case TString => TBool
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      case Binary(And|Or, e1, e2) => typeof(env, e1) match {
        case TBool => typeof(env, e2) match {
          case TBool => TBool
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      case Binary(Seq, e1, e2) => typeof(env, e1); typeof(env, e2)
      case If(e1, e2, e3) => typeof(env, e1) match {
        case TBool =>
          val t1 = typeof(env, e2)
          val t2 = typeof(env, e3)
          if (t1 == t2) t1 else err(t2, e3)

        case tgot => err(tgot, e1)
      }

      case Obj(fields) => TObj(fields.mapValues { ei => typeof(env, ei) })
      case GetField(e1, f) => typeof(env, e1) match {
        case t1 @ TObj(tfields) => tfields.get(f) match {
          case Some(t) => t
          case _ => err(t1, e1)
        }
        case tgot => err(tgot, e1)
      }

        /***** Cases from Lab 4 that need a small amount of adapting. */
      case Decl(m, x, e1, e2) =>
        //order matter?
        val t1 = typeof(env, e1)
        val t2 = typeof(env + (x->MTyp(m, t1)), e2)
        if (isBindex(m, e1))
          t2
        else
          err(t1, e1)

      case Function(p, params, tann, e1) =>
        // Bind to env1 an environment that extends env with an appropriate binding if
        // the function is potentially recursive.
        val env1 = (p, tann) match {
          case (Some(f), Some(tret)) =>
            val tprime = TFunction(params, tret)
            env + (f->MTyp(MConst, tprime))
          case (None, _) => env
          case _ => err(TUndefined, e1)
        }
        // Bind to env2 an environment that extends env1 with bindings for params.
        val env2 = env1 ++ params
        // Infer the type of the function body
        val t1 = typeof(env2, e1)
        // Check with the possibly annotated return type
        tann match {
          case Some(tann) if tann != t1 => err(tann, e1)
          case _ => TFunction(params, t1)
        }


      case Call(e1, args) => typeof(env, e1) match {
        case TFunction(params, tret) if (params.length == args.length) =>
          (params, args).zipped.foreach {
            case ((xi, MTyp(mi, ti)), ei) =>
              if (typeof(env, ei) == ti && isBindex(mi, ei)) () else err(ti, ei)
          }
          tret
        case tgot => err(tgot, e1)
      }

        /***** New cases for Lab 5. ***/
      case Assign(Var(x), e) => env(x) match {
        case MTyp(MVar | MRef, t) => typeof(env, e) match {
          case `t` => t
          case tgot => err(tgot, e)
        }
        case MTyp(_, tgot) => err(tgot, Var(x))
      }

      case Assign(GetField(e1, f), e2) => typeof(env, e1) match {
        case t @ TObj(tfields) =>
          if (tfields contains f)
            typeof(env, e2) match {
              case t if t == tfields(f) => t
              case tgot => err(tgot, e2)
            }
          else
            err(t, e1)
        case tgot => err(tgot, e1)
      }

      case Assign(_, _) => err(TUndefined, e)

      case Null => TNull

      case Unary(Cast(t), e1) => typeof(env, e1) match {
        case tgot if castOk(tgot, t) => t
        case tgot => err(tgot, e1)
      }

      /* Should not match: non-source expressions or should have been removed */
      case A(_) | Unary(Deref, _) | InterfaceDecl(_, _, _) => throw new IllegalArgumentException("Gremlins: Encountered unexpected expression %s.".format(e))
    }
  }

  /*** Small-Step Interpreter ***/

  /*
   * Helper function that implements the semantics of inequality
   * operators Lt, Le, Gt, and Ge on values.
   *
   * We suggest a refactoring of code from Lab 2 to be able to
   * use this helper function in eval and step.
   *
   * This should the same code as from Lab 3 and Lab 4.
   */
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean = {
    require(isValue(v1), s"inequalityVal: v1 ${v1} is not a value")
    require(isValue(v2), s"inequalityVal: v2 ${v2} is not a value")
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    (v1, v2) match {
      case _ => ???
    }
  }

  /* Capture-avoiding substitution in e replacing variables x with esub. */
  def substitute(e: Expr, esub: Expr, x: String): Expr = {
    def subst(e: Expr): Expr = e match {
      case N(_) | B(_) | Undefined | S(_) | Null | A(_) => e
      case Print(e1) => Print(subst(e1))
        /***** Cases from Lab 3 */
      case Unary(uop, e1) => Unary(uop, subst(e1))
      case Binary(bop, e1, e2) => Binary(bop, subst(e1), subst(e2))
      case If(e1, e2, e3) => If(subst(e1), subst(e2), subst(e3))
      case Var(y) => if (y==x) esub else e
        /***** Cases need a small adaption from Lab 3 */
      case Decl(mut, y, e1, e2) => Decl(mut, y, subst(e1), if (x == y) e2 else subst(e2))
        /***** Cases needing adapting from Lab 4 */
      case Function(p, params, tann, e1) =>
        if (params.exists { case (xi, mt) => xi == x } || p.contains(x))
          Function(p, params, tann, e1)
        else
          Function(p, params, tann, subst(e1))
        /***** Cases directly from Lab 4 */
      case Call(e1, args) => Call(subst(e1), args map { ei => subst(ei) })
      case Obj(fields) => Obj(fields mapValues { ei => subst(ei) })
      case GetField(e1, f) => GetField(subst(e1), f)
        /***** New case for Lab 5 */
      case Assign(e1, e2) => Assign(subst(e1), subst(e2))

      /* Should not match: should have been removed */
      case InterfaceDecl(_, _, _) => throw new IllegalArgumentException("Gremlins: Encountered unexpected expression %s.".format(e))
    }

    def myrename(e: Expr): Expr = {
      val fvs = freeVars(esub)
      def fresh(x: String): String = if (fvs contains x) fresh(x + "$") else x
      rename[Unit](e)(){ x => doreturn(fresh(x)) }
    }

    subst(myrename(e))
  }

  /* Check whether or not an expression is reduced enough to be applied given a mode. */
  def isRedex(mode: Mode, e: Expr): Boolean = mode match {
    case MConst | MVar if !isValue(e) => true
    case MRef if !isLValue(e) => true
    case _ => false
  }

  def getBinding(mode: Mode, e: Expr): DoWith[Mem,Expr] = {
    require(!isRedex(mode,e), s"expression ${e} must not reducible under mode ${mode}")
    mode match {
      case MConst | MName | MRef => doreturn(e)
      case MVar => memalloc(e) map { a => Unary(Deref, a) }
    }
  }

  /* A small-step transition. */
  def step(e: Expr): DoWith[Mem, Expr] = {
    require(!isValue(e), "stepping on a value: %s".format(e))
    e match {
      //DoNeg
      case Unary(Neg, N(n)) => doreturn(N(-n))

      //DoNot
      case Unary(Not, B(b)) => doreturn(B(!b))

      //DoSeq
      case Binary(Seq, v1, e2) if isValue(v1) => doreturn(e2)

      //DoArith
      case Binary(Plus , N(n1), N(n2)) => doreturn(N(n1 + n2))
      case Binary(Minus, N(n1), N(n2)) => doreturn(N(n1 - n2))
      case Binary(Times, N(n1), N(n2)) => doreturn(N(n1 * n2))
      case Binary(Div  , N(n1), N(n2)) => doreturn(N(n1 / n2))

      //DoPlusString
      case Binary(Plus, S(s1), S(s2)) => doreturn(S(s1 + s2))

      //DoInequalityNumber
      case Binary(Lt, N(n1), N(n2)) => doreturn(B(n1 <  n2))
      case Binary(Le, N(n1), N(n2)) => doreturn(B(n1 <= n2))
      case Binary(Gt, N(n1), N(n2)) => doreturn(B(n1 >  n2))
      case Binary(Ge, N(n1), N(n2)) => doreturn(B(n1 >= n2))

      //DoInequalityNumber
      case Binary(Lt, S(s1), S(s2)) => doreturn(B(s1 <  s2))
      case Binary(Le, S(s1), S(s2)) => doreturn(B(s1 <= s2))
      case Binary(Gt, S(s1), S(s2)) => doreturn(B(s1 >  s2))
      case Binary(Ge, S(s1), S(s2)) => doreturn(B(s1 >= s2))

      //DoEquality
      case Binary(Eq, v1, v2) if isValue(v1) && isValue(v2) => doreturn(B(v1 == v2))
      case Binary(Ne, v1, v2) if isValue(v1) && isValue(v2) => doreturn(B(v1 != v2))

      //DoAndTrue
      case Binary(And, B(true), e2) => doreturn(e2)

      //DoAndFalse
      case Binary(And, B(false), _) => doreturn(B(false))

      //DoOrTrue
      case Binary(Or, B(true), _) => doreturn(B(true))

      //DoOrFalse
      case Binary(Or, B(false), e2) => doreturn(e2)

      //DoPrint
      case Print(v1) if isValue(v1) => doget map { m => println(pretty(m, v1)); Undefined }

      //DoIfTrue
      case If(B(true), e2, _) => doreturn(e2)

      //DoIfFalse
      case If(B(false), _, e3) => doreturn(e3)

      //DoObject
      case Obj(fields) if fields.forall { case (_, vi) => isValue(vi) } => memalloc(Obj(fields))

      //DoGetField
      case GetField(a @ A(_), f) => doget map { mem => mem(a) match {
        case Obj(fields) => fields(f)
        case _ => throw StuckError(e)
      } }

      //SearchObject
      case Obj(fields) => fields.find { case (_, ei) => !isValue(ei) } match {
        case Some((fi,ei)) => step(ei) map { eip => Obj(fields + (fi->eip))}
        case _ => throw StuckError(e)
      }

      //NullErrorGetField
      case GetField(Null, _) => throw NullDereferenceError(e)

      //SearchGetField
      case GetField(e1, f) => step(e1) map { e1p => GetField(e1p, f) }

      //DoDecl
      case Decl(m, x, e1, e2) if !isRedex(m, e1) => getBinding(m, e1) map { e1p =>  substitute(e2, e1p, x) }

      //SearchDecl
      case Decl(m, x, e1, e2) => step(e1) map { e1p => Decl(m, x, e1p, e2) }

      //DoDeref
      case Unary(Deref, a:A) => doget map { mem => mem(a) }

      //DoAssignVar
      case Assign(Unary(Deref, a:A), v) if isValue(v) => domodify[Mem] { mem => mem + (a, v) } map { _ => v }

      //DoAssignField
      case Assign(GetField(a:A, f), v) if isValue(v) => domodify[Mem] { mem => mem(a) match {
        case Obj(fields) => mem + (a, Obj(fields + (f->v)))
        case _ => throw StuckError(e)
      }} map { _ => v }

      //SearchAssign1
      case Assign(e1, e2) if !isLValue(e1) => step(e1) map { e1p => Assign(e1p, e2) }

      //SearchAssign2
      case Assign(lv1, e2) if isLValue(lv1) => step(e2) map { e2p => Assign(lv1, e2p) }

      //DoCall/DoCallRec
      case Call(v @ Function(p, params, _, e), args) if params.zip(args) forall { case ((_, MTyp(mi, _)), ei) => !isRedex(mi, ei) } =>
        val ep = params.zip(args).foldRight[DoWith[Mem,Expr]] (doreturn(e)) {
          case (((xi, MTyp(mi, _)), ei), acc) => acc flatMap { e_acc => getBinding(mi,ei) map { eip => substitute(e_acc, eip, xi) } }
        }
        p match {
          case None => ep
          case Some(x) => ep flatMap { e_acc => getBinding(MConst, v) map { epp => substitute(e_acc, epp, x) }}
        }

      //SearchCall1
      case Call(e, args) if !isValue(e) => step(e) map { ep => Call(ep, args) }

      //SearchCall2
      case Call(f @ Function(_,params,_,_), args) => mapFirstWith[Mem, ((String, MTyp), Expr)](params.zip(args)) {
        case (sigma @ (_, MTyp(mi, _)), ei) => if (isRedex(mi, ei)) Some(step(ei) map { eip => (sigma, eip) }) else None
      } map { pazip => Call(f, pazip.unzip._2) }

      //DoCastNull
      case Unary(Cast(t), Null) => doreturn(Null)

      //DoCastObj/TypeErrorCastObj
      case Unary(Cast(TObj(tfields)), a:A) => doget map {
        mem => mem(a) match {
          case Obj(fields) =>
            if (tfields forall { case (fi, _) => fields contains fi })
              a
            else
              throw DynamicTypeError(e)
          case _ => throw StuckError(e)
        }
      }

      //NullErrorAssignField
      case Assign(GetField(Null, _), _) => throw NullDereferenceError(e)

      //DoCast
      case Unary(Cast(t), v) if !v.isInstanceOf[A] => doreturn(v)

      //SearchUnary
      case Unary(uop, e1) => step(e1) map { e1p => Unary(uop, e1p) }

      //SearchBinary2
      case Binary(bop, v1, e2) if isValue(v1) => step(e2) map { e2p => Binary(bop, v1, e2p) }

      //SearchBinary1
      case Binary(bop, e1, e2) => step(e1) map { e1p => Binary(bop, e1p, e2) }

      //SearchPrint
      case Print(e1) => step(e1) map { e1p => Print(e1p) }

      //SearchIf
      case If(e1, e2, e3) => step(e1) map { e1p => If(e1p, e2, e3) }
    }
  }

  /*** Extra Credit: Lowering: Remove Interface Declarations ***/

  def lower(e: Expr): Expr =
    /* Do nothing by default. Change to attempt extra credit. */
    e

  /*** External Interfaces ***/

  //this.debug = true // comment this out or set to false if you don't want print debugging information
  this.maxSteps = Some(1000) // comment this out or set to None to not bound the number of steps.
  this.keepGoing = true // comment this out if you want to stop at first exception when processing a file
}
