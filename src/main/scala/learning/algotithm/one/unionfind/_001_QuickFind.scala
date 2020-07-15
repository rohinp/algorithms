package learning.algotithm.one.unionfind

import cats.data.State
import cats.Eq
import cats.implicits._
import learning.algotithm.one.input.FileInput._
import learning.algotithm.one.Main
import scala.collection.immutable.TreeMap
import scala.util.chaining._

object _001_QuickFind extends Main {
  /*
    Quick find implementation as per the tutorial
    link: https://www.coursera.org/learn/algorithms-part1/lecture/EcF3P/quick-find

    Note: In the tutorial the solution is in java using simple array and fixed data to integers

    Here the solution includes using TreeMap and generic data
    Also the constraint on fixing the number of input is ignored.
    Please check the tutorial for better understanding

    despite of the use of TreeMap which gives log time for operations
    NLogN operations, but better than quadratic operation as in case of using arrays

    When you run the program each and every step is printed on the screen in the below format
    {operation} {size of data}
    ----> intermediate operation or final operation
    --------> final operations in some case where entire list traversing is required.
    Helps to visualize how the algorithm is working.
   */

  type UFMap[A, R] = State[TreeMap[A, A], R]

  def union[A: Eq: Ordering](p: A, q: A): UFMap[A, Unit] =
    State.modify(qfMap => {
      (qfMap.get(p),qfMap.get(q)).tap(_ => println(Console.GREEN_B + s"union($p , $q) current size ${qfMap.size}" + Console.RESET)) match {
        case (Some(pKey), Some(qKey)) =>
          qfMap.map(dataKey => (if(dataKey._2 === pKey) dataKey._1 -> qKey else dataKey)
            .tap(kv => println(s"${"-" * 4}> Both p and q exists in the map, current element key(${kv._2}) == pKey($pKey) = ${kv._2 === pKey} result is $kv"))
          ).tap(tm => println(s"${"-" * 8}> Map traverse is done with replacing p key with q key; result is $tm"))
        case (Some(pKey), None) =>  (qfMap + (q -> pKey)).tap(tm => println(s"${"-" * 4}> q($q) element does'nt exists in the Map so add one to the result $tm"))
        case (None, Some(qKey)) => (qfMap + (p -> qKey)).tap(tm => println(s"${"-" * 4}> p($p) element does'nt exists in the Map so add one to the result $tm"))
        case (None, None) => (qfMap ++ List(p -> q, q -> q)).tap(tm => println(s"${"-" * 4}> Both element do not exist in the list so add both to the result $tm"))
      }
    })

  def connected[A: Eq](p: A, q: A): UFMap[A, Boolean] =
    State.inspect(s => (s.get(p) === s.get(q)).tap(b => println(Console.GREEN_B +  s"connected(p($p), q($q)) == $b" + Console.RESET)))

  val allUnions: UFMap[Int, Unit] =
    objectTuple.foldLeft(State.empty[TreeMap[Int, Int], Unit])(
      (acc, v) =>
        for {
          _ <- acc
          _ <- union(v._1, v._2)
        } yield ()
    )

  //Sample Run
  override def execute(): Unit =
    (for {
      _ <- allUnions
      r <- connected(4, 8)
    } yield r).run(TreeMap.empty[Int, Int]).value
}
