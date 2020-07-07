package learning.algotithm.one.unionfind

import cats.data.State
import cats.Eq
import cats.implicits._
import learning.algotithm.one.input.FileInput._
import learning.algotithm.one.Main

import scala.collection.immutable.TreeMap

object UF_v2 extends Main {
  /*
    Quick find implementation as per the tutorial
    link: https://www.coursera.org/learn/algorithms-part1/lecture/EcF3P/quick-find

    Note: In the tutorial the solution is in java using simple array and fixed data to integers

    Here the solution includes using TreeMap and generic data
    Also the constraint on fixing the number of input is ignored.
    Please check the tutorial for better understanding
   */

  type UFMap[A, R] = State[TreeMap[A, A], R]

  def union[A: Eq: Ordering](p: A, q: A): UFMap[A, Unit] = State.modify(qfMap => {
    if (qfMap.exists(kv => kv._1 === p) && qfMap.exists(kv => kv._1 === q))
      qfMap.map {
        case kv @ (data, key) => if (key == qfMap(p)) data -> qfMap(q) else kv
      } else if (qfMap.exists(kv => kv._1 === p)) qfMap ++ List(q -> qfMap(p))
    else if (qfMap.exists(kv => kv._1 === q)) qfMap ++ List(p -> qfMap(q))
    else qfMap ++ List(p -> q, q -> q)
  })

  def connected[A: Eq](p: A, q: A): UFMap[A, Boolean] =
    State.inspect(s => s(p) == s(q))

  val allUnions: UFMap[Int,Unit] = objectTuple.foldLeft(State.empty[TreeMap[Int,Int],Unit])(
    (acc, v) => for {
      _ <- acc
      _ <- union(v._1,v._2)
    } yield ()
  )

  //Sample Run
  override def execute():Unit = println((for {
    _ <- allUnions
    r <- connected(4,8)
  } yield r).run(TreeMap.empty[Int,Int]).value)
}
