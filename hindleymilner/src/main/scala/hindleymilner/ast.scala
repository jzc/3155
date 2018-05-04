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

  /* substitutions { a1 -> t1, a2 -> t2 ... an -> tn } */

  case class Subst (m: Map[String, Type]) {
    /* Compose two substitutions by apply the first one to everything in the second substitution */
    def compose(other: Subst): Subst = Subst(other.m.mapValues { ti => this.apply(ti) } ++ m)

    /* Apply the substitution described by m. */
    def apply(t: Type): Type = t match {
      case TVar(x) => m.getOrElse(x, TVar(x))
      case TFun(t1, t2) => TFun(this.apply(t1), this.apply(t2))
    }

    def apply(s: Scheme): Scheme = s match {
      case Forall(as, t) => Forall(as, Subst(as.foldRight(m) { case (xi, acc) => acc-xi } ).apply(t) )
    }

    def apply(t: TEnv): TEnv = TEnv(t.m.mapValues { ti => this.apply(ti) })
  }

  def nullSubst = Subst(Map())
  def empty = TEnv(Map())

//  case class TEnv (m: )


  case class TEnv(m: Map[String, Scheme]) {
    def get(x: String):Option[Scheme] = m.get(x)
    def union(other: Map[String, Scheme]): TEnv =
      TEnv(m.keySet.union(other.keySet).map { k => (k, m.getOrElse(k, other(k))) }.toMap)
  }
//    def get(x: String) = m.get(x)
//    def
}

//  /* Substitutable trait */
//  trait S[A] {
//    def apply(s: Subst): S[A]
//    def ftv: Set[String]
//  }
//
//  val nullSubst = Map.empty[String, S[Type]]
//
////  abstract class Substitutable(a:)
//  abstract class Type
//  case class TVar(x: String) extends Type with S[Type] {
//    override def apply(s: Subst): S[Type] = s.getOrElse(x, this)
//    override def ftv: Set[String] = Set(x)
//  }
//
//  case class TFun(t1: S[Type], t2: S[Type]) extends Type with S[Type] {
//    override def apply(s: Subst): Type = TFun(t1.apply(s), t2.apply(s))
//    override def ftv: Set[String] = t1.ftv.union(t2.ftv)
//  }
//
//  abstract class Scheme
//  case class Forall(as: List[String], t: S[Type]) extends Scheme with S[Scheme] {
//    override def apply(s: Subst): S[Scheme] = Forall(as, t.apply(as.foldRight(s) { case (i, acc) => (acc - i) } ))
//    override def ftv: Set[String] = t.ftv.diff(as.toSet)
//  }
//
//  type TEnv = Map[String, Scheme]
//  def remove(env: TEnv)



  //  type Subst = Map[TVar, Type]

//  case class Subst[A] private (t: A) {
//
//  }

