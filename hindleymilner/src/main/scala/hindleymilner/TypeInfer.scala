package hindleymilner

import hindleymilner.DoWith._
import hindleymilner.ast._

object TypeInfer {
  def fresh: DoWith[Char, TVar] = doget[Char] flatMap { prevChar =>
    doput((prevChar+1).toChar) map { _ => TVar(prevChar.toString) }
  }

//  for {
//    prevChar <- doget[Char]
//    _ <- doput(prevChar+1)
//  } yield TVar(prevChar.toString)

  def ftv(t: Type): Set[String] = t match {
    case TVar(x) => Set(x)
    case TFun(t1, t2) => ftv(t1) union ftv(t2)
  }

  def ftv(s: Scheme): Set[String] = s match {
    case Forall(as, t) => ftv(t) diff as.toSet
  }

  def ftv(env: TEnv): Set[String] = env.m.values.toList.flatMap(ftv).toSet

  def bind(a: String, t:Type): Subst =
    if (TVar(a) == t)
      nullSubst
    else if (ftv(t) contains a)
      throw ???
    else
      Subst(Map(a->t))

  def unify(t1: Type, t2: Type): Subst = (t1, t2) match {
    case (TFun(l, r), TFun(lp, rp)) =>
      val s1 = unify(l, lp)
      val s2 = unify(s1.apply(r), s1.apply(rp))
      s2.compose(s1)

    case (TVar(a), t) => bind(a, t)
    case (t, TVar(a)) => bind(a, t)
    case _ => throw ???
//    case (TVar(a), t) = bind(a, t)
  }

//  def mapUnion[A,B](m1: Map[A,B], m2: Map[A,B]): Map[A,B] =
//    m1.keySet.union(m2.keySet).map { k => (k, m1.getOrElse(k, m2(k))) }.toMap
//
//  def instantiate(s: Scheme): DoWith[Char, Type] = ???

//  def infer(env: TEnv, e: Expr): DoWith[Char, (Subst, Type)] = e match {
//    case Var(x) => env.get(x) match {
//      case None =>
//      case Some(sigma) => for {
//        t <- instantiate(sigma)
//      } yield (nullSubst, t)
//    }
//    case Abs(x, e) => for {
//      tv <- fresh
//      val envp
//
//    }
//    case Abs(x, e) => ???
//  }

//  def instantiate(s: Scheme): DoWith[Char, Type] = s match {
//    case Forall(as, t) =>
//  }

  def mapWith[W,A,B](l: List[A])(f: A => DoWith[W,B]): DoWith[W,List[B]] = {
    l.foldRight[DoWith[W,List[B]]]( doreturn(Nil) ) {
      case (a, dwb) => f(a) flatMap { ap => dwb flatMap { dwbp => doreturn(ap :: dwbp) }} //dwb.flatMap { bs => f(a)}
    }
  }

  def instantiate(s: Scheme): DoWith[Char, Type] = s match {
    case Forall(as, t) => mapWith(as) { _ => fresh } map { asp =>
      val s = Subst(as.zip(asp).toMap)
      s.apply(t)
    }
  }

  def generalize(env: TEnv, t:Type) = Forall((ftv(t) diff ftv(env)).toList, t)

  def inferType(e: Expr): Type = {
    val p = infer(empty, e) map { case (s, t) => s.apply(t) }
    val (_, t) = p('a')
    t
  }

  def infer(env: TEnv, e: Expr): DoWith[Char, (Subst, Type)] = e match{

    case Var(x) => env.get(x) match {
      case None => throw ???
      case Some(s) => instantiate(s) map { t => (nullSubst, t) }
    }

    case Abs(x, e1) =>
      fresh flatMap { tv =>
      val envp = env.union(Map(x->Forall(Nil, tv)))
      infer(envp, e1) map { case (s1, t1) =>
      (s1, s1.apply(TFun(tv, t1)))
      }}

    case App(e1, e2) =>
      fresh flatMap { tv =>
      infer(env,e1) flatMap { case (s1, t1) =>
      infer(s1.apply(env), e2) map { case (s2, t2) =>
      val s3 = unify(s2.apply(t1), TFun(t2, tv))
      (s3.compose(s2).compose(s1), s2.apply(tv))
      }}}

    case Let(x, e1, e2) =>
      infer(env, e1) flatMap { case (s1, t1) =>
      val envp = s1.apply(env)
      val tp = generalize(envp, t1)
      infer(envp.union(Map(x->tp)), e2).map { case (s2, t2) =>
      (s1.compose(s2), t2)
      }}
//      fresh flatMap { tv =>
//      infer(env, e1) flatMap { case (s1, t1) =>
//      infer(s1.apply(env), e2) flatMap { case (s2, t2) =>
//
//        }
//      }
//    }

//    case Var(x) => env.get(x) match {
//      case Some(Forall(as, tp)) => {
////        mapWith(as) { ai => for {
////          bi <- fresh
////        } yield (ai, bi) } map { res => (nullSubst, Subst(res.toMap).apply(tp)) }
////        val s = as.foldRight(doreturn[Char, Subst](nullSubst)) {
////          case (ai, st) => st flatMap { acc => fresh flatMap { bi => doreturn(Subst(Map(ai->bi))) }}
////
////        }
//      }
//      case _ => throw ???
//    }

//    case App(e1, e2) => for {
//      (s1:Subst, t1) <- infer(env, e2)
//      (s2:Subst, t2) <- infer(s1.apply(A), e2)
//      b <- fresh
//      s3:Subst <- unify(s2.apply(t1), TFun(t2,b))
//    } yield (s3.compose(s2).compose(s1), s3.apply(b))



//    for {
//      b <- fresh
//      (s1, t1) <- infer(A.union(Map("x"->Forall(Nil, b))), e1)
//    } yield (s1, s1.apply(TFun(b, t1)))

//    case Let(x, e1, e2) =>  for {
//      (s1:Subst, t1:Type) <- infer(A, e1)
//      envp:TEnv <- s1.apply(A)
//      tp <- generalize(envp, t1)
//      (s2:Subst, t2) <- infer(envp.union(Map(x->tp)), e2)
//    } yield (s1.compose(s2), t2)
  }
}
