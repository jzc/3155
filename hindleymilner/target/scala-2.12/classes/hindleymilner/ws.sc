//import ast._
//import DoWith._
import hindleymilner.DoWith
import hindleymilner.DoWith._

import hindleymilner.TypeInfer._
import hindleymilner.ast._

val id = Let("x", Abs("x", Var("x")), Var("x"))

infer(empty, id)


//def next(s: String): String = s.toList match {
//  case Nil => "a"
//  case c :: Nil if c <= 'y' =
//}

//def mapWith[W,A,B](l: List[A])(f: A => DoWith[W,B]): DoWith[W,List[B]] = {
//  l.foldRight[DoWith[W,List[B]]]( doreturn(Nil) ) {
//    case (a, dwb) => f(a) flatMap { ap => dwb flatMap { dwbp => doreturn(ap :: dwbp) }} //dwb.flatMap { bs => f(a)}
//  }
//}
//val s1 = Map("x"->2, "y"->2)
//val s2 = Map("x"->1, "z"->3)
//s1.keySet.union(s2.keySet).map { k => (k, s1.getOrElse(k, s2(k))) }.toMap
//val dw = fresh flatMap { t1 => fresh map { t2 => (t1,t2) }}
//dw('a')
////"abc".toList
////'c'.toString



//List(Set(1,2,3),Set(2,3,4)).flatMap {a=>a}.distinct