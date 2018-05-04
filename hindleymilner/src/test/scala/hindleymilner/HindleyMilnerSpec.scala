package hindleymilner

import hindleymilner.TypeInfer._
import hindleymilner.ast._

object HindleyMilnerSpec {
  def main(args: Array[String]): Unit = {
    val id = Let("id", Abs("x", Var("x")), Var("id"))
    val compose = Let(
      "compose",
      Abs("f",
        Abs("g",
          Abs("x",
            App(Var("f"),
              App(Var("g"), Var("x"))
            )))),
      Var("compose"))
    val t = inferType(compose)
    println(t)
  }
}
