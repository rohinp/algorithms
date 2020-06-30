package learning.algotithm.one.unionfind

import cats.Eq
import cats.implicits._
import cats.data.State
import scala.io.Source
object UF extends App {
  val objectTuple: Iterator[(Int, Int)] =
    Source
      .fromResource("tinyUF.txt")
      .getLines
      .map(
        _.split(" ")
          .map(_.toInt)
          .toList
      )
      .map({
        case a :: b :: _ => (a, b)
      })
  //data structure
  type UFList[A,R] = State[List[Set[A]], R]

  def union[A: Eq](p:A, q:A): UFList[A,Unit] =
        State.modify { ufList => {
            @scala.annotation.tailrec
            def loop(pSet: Set[A], qSet: Set[A], acc: List[Set[A]], currentSet: List[Set[A]]): List[Set[A]] = (pSet.isEmpty, qSet.isEmpty, currentSet) match {
                case (true, true, List()) => Set(p, q) :: acc
                case (true, false, List())  => acc ++ List(qSet + p)
                case (false, true, List())  => acc ++ List(pSet + q)
                case (false, false, List()) => acc ++ List(pSet ++ qSet)
                case (false, false, xs) => acc ++ xs ++ List(pSet ++ qSet)
                case (true, true, e@x::_)  if x.exists(_ === p) && x.exists(_ === q) => acc ++ e
                case (true, false, x :: _) if x.exists(_ === p) =>  acc ++ List(x ++ qSet)
                case (false, true, x :: _) if x.exists(_ === q) =>  acc ++ List(x ++ pSet)
                case (true, false, x :: xs) => loop(pSet, qSet, x :: acc, xs)
                case (false, true, x :: xs) => loop(pSet, qSet, x :: acc, xs)
                case (true, true,  x :: xs)  if x.exists(_ === p) => loop(x, qSet, acc, xs)
                case (true, true,  x :: xs)  if x.exists(_ === q) => loop(pSet, x, acc, xs)
                case (true, true,  x :: xs)  => loop(pSet, qSet, x :: acc, xs)
              }
            loop(Set.empty[A], Set.empty[A], List(), ufList)
          }
    }

  def connected[A: Eq](p:A, q:A): UFList[A,Boolean] =
    State.inspect( _.filter(s => s.exists(_ === p)).exists(s => s.exists(_ === q)))

  val allUnions: UFList[Int,Unit] = objectTuple.foldLeft(State.empty[List[Set[Int]],Unit])(
    (acc, v) => for {
      _ <- acc
      _ <- union(v._1,v._2)
    } yield ()
  )

  println((for {
    _ <- allUnions
    r <- connected(2,1)
  } yield r).run(List()).value._2)
}
