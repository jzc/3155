package hindleymilner

import hindleymilner.DoWith._
import hindleymilner.ast._

object TypeInfer {
  def fresh: DoWith[Char, TVar] = doget[Char] flatMap { prevChar =>
    doput((prevChar+1).toChar) map { _ => TVar(prevChar.toString) }
  }

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
      val s2 = unify(apply(s1, r), apply(s1, rp))
      s2 compose s1

    case (TVar(a), t) => bind(a, t)
    case (t, TVar(a)) => bind(a, t)
    case _ => throw ???
  }

  def instantiate(s: Scheme): DoWith[Char, Type] = s match {
    case Forall(as, t) => mapWith(as) { _ => fresh } map { asp =>
      val s = Subst(as.zip(asp).toMap)
      apply(s, t)
    }
  }

  def generalize(env: TEnv, t:Type) = Forall((ftv(t) diff ftv(env)).toList, t)

  def infer(env: TEnv, e: Expr): DoWith[Char, (Subst, Type)] = e match {

    case Var(x) => env.get(x) match {
      case None => throw ???
      case Some(s) => instantiate(s) map { t => (nullSubst, t) }
    }

    case Abs(x, e1) =>
      fresh flatMap { tv =>
      val envp = env extend (x, Forall(Nil, tv))
      infer(envp, e1) map { case (s1, t1) =>
      (s1, apply(s1, TFun(tv, t1)))
      }}

    case App(e1, e2) =>
      fresh flatMap { tv =>
      infer(env, e1) flatMap { case (s1, t1) =>
      infer(apply(s1, env), e2) map { case (s2, t2) =>
      val s3 = unify(apply(s1, t1), TFun(t2, tv))
      (s3 compose s2 compose s1, apply(s2, tv))
      }}}

    case Let(x, e1, e2) =>
      infer(env, e1) flatMap { case (s1, t1) =>
      val envp = apply(s1, env)
      val tp = generalize(envp, t1)
      infer(envp extend (x, tp), e2).map { case (s2, t2) =>
      (s1 compose s2, t2)
      }}
  }

  def inferType(e: Expr): Type = {
    val p = infer(empty, e) map { case (s, t) => apply(s, t) }
    val (_, t) = p('a')
    t
  }
}
