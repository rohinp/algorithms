package learning.algotithm.one.unionfind

import cats.data.State
import cats.implicits._
import cats.Eq
import learning.algotithm.one.Main
import learning.algotithm.one.input.FileInput.objectTuple

import scala.util.chaining._
import scala.collection.immutable.TreeMap

object _002_QuickUnion extends Main{
  /*
    Quick find implementation as per the tutorial
    link: https://www.coursera.org/learn/algorithms-part1 go to quick union video

    The previous implementations were eager implementations of the algorithm
    this one is a lazy implementation quick-union

    Falling back to Hashmap as it will be more performant, as we don't need the order to be maintained
    Performance is not that great though, please check it by executing the code. Next will do some minor improvements.
    Please check the tutorial.
   */

  type QUMap[A, R] = State[Map[A, A], R]

  def root[A:Eq](v:A):QUMap[A, A] = State.inspect(s => {
    println("\t"+ Console.YELLOW_B + "Finding Root" + Console.RESET)
    @scala.annotation.tailrec
    def loop(currentRoot: A):A = s.get(currentRoot) match {
      case None => currentRoot.tap(a => println(s"\t\tRoot == value for  $currentRoot"))
      case Some(newRoot) => println(s"\t\tRoot of $currentRoot is found to be $newRoot")
        loop(newRoot)
    }
    loop(v)
  })

  def union[A:Eq](p: A, q: A): QUMap[A, Unit] = {
    println(Console.GREEN_B + s"Union($p, $q)" + Console.RESET)
    for {
      rootOfP <- root(p)
      rootOfQ <- root(q)
      _ <- if(rootOfP === rootOfQ) State.get[Map[A,A]].tap(_ => println(s"\tThe rootOfP EQ rootOfQ no change in map"))
      else State.modify[Map[A,A]](m => (m + (p -> q)).tap(map => println(s"\tThe rootOfP NEQ rootOfQ and the new map is $map")))
    } yield ()
  }

  def connected[A:Eq](p: A, q: A): QUMap[A, Boolean] = {
    println(Console.GREEN_B + s"Connected($p, $q)" + Console.RESET)
    for {
      rootOfP <- root(p)
      rootOfQ <- root(q)
    } yield (rootOfP === rootOfQ).tap(b => println(s"\tConnected($p, $q) = $b"))
  }

  val allUnions: QUMap[Int, Unit] =
    objectTuple.foldLeft(State.empty[Map[Int, Int], Unit])(
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
