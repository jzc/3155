package hindleymilner

object ast {
  abstract class Expr

  /* Variables - x */
  case class Var(x: String) extends Expr

  /* Abstraction (function declaration) - function (x) { return e } or x => e */
  case class Abs(x: String, e: Expr) extends Expr

  /* Application (function call) - e0(e1) */
  case class App(e0: Expr, e1: Expr) extends Expr

  /* Let (variable declaration) - const x = e0; e1 */
  case class Let(x: String, e0: Expr, e1: Expr) extends Expr

  abstract class Type
  case class TVar(x: String) extends Type
  case class TFun(t1: Type, t2: Type) extends Type

  abstract class Scheme
  case class Forall(as: List[String], t: Type) extends Scheme

  def displayType(t: Type): String = t match {
    case TVar(x) => x
    case TFun(f1:TFun, t2) => "(" + displayType(f1) + ")" + " -> " + displayType(t2)
    case TFun(t1, t2) => displayType(t1) + " -> " + displayType(t2)
  }

  /* substitutions { a1 -> t1, a2 -> t2 ... an -> tn } */
  case class Subst (m: Map[String, Type]) {
    /* Compose two substitutions by apply the first one to everything in the second substitution */
    def compose(other: Subst): Subst = Subst(other.m.mapValues { ti => apply(this, ti) } ++ m)
  }

  /* Apply the substitution s to type t. */
  def apply(s: Subst, t: Type): Type = t match {
    case TVar(x) => s.m.getOrElse(x, TVar(x))
    case TFun(t1, t2) => TFun(apply(s, t1), apply(s, t2))
  }

  /* Apply the substitution s to scheme sc. */
  def apply(s: Subst, sc: Scheme) : Scheme = sc match {
    case Forall(as, t) => Forall(as, apply(Subst(as.foldRight(s.m) { case (xi, acc) => acc-xi }), t) )
  }

  /* Apply the substitution s to type environment t. */
  def apply(s: Subst, t: TEnv): TEnv = TEnv(t.m.mapValues { ti => apply(s, ti) })

  def nullSubst = Subst(Map())
  def empty = TEnv(Map())

  case class TEnv(m: Map[String, Scheme]) {
    def get(x: String): Option[Scheme] = m.get(x)
    def extend(k: String, v: Scheme): TEnv = TEnv(m + (k->v))
  }

  def ftv(t: Type): Set[String] = t match {
    case TVar(x) => Set(x)
    case TFun(t1, t2) => ftv(t1) union ftv(t2)
  }

  def ftv(s: Scheme): Set[String] = s match {
    case Forall(as, t) => ftv(t) diff as.toSet
  }

  def ftv(env: TEnv): Set[String] = env.m.values.toList.flatMap(ftv).toSet

}

