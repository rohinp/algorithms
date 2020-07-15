package learning.algotithm.one.unionfind

import cats.data.State
import cats.implicits._
import cats.Eq
import learning.algotithm.one.Main
import learning.algotithm.one.input.FileInput.objectTuple
import scala.util.chaining._
import scala.collection.immutable.TreeMap

object _003_WeightedQuickUnion extends Main{
  /*
    Quick find implementation as per the tutorial
    link: https://www.coursera.org/learn/algorithms-part1 go to quick union video

    The previous implementations were eager implementations of the algorithm
    this one is a lazy implementation quick-union

    This is an improvisation on the previous implementation on quick-union

    Improvisation can be seen when you compare the output of algorithm execution and compare with the previous implementations
   */

  type QUMap[A, R] = State[Map[A, A], R]
  case class Root[A](value:A, depth:Int)

  def root[A:Eq](v:A):QUMap[A, Root[A]] = State.inspect(s => {
    println("\t"+ Console.YELLOW_B + "Finding Root" + Console.RESET)
    @scala.annotation.tailrec
    def loop(currentRoot: A, depth:Int): Root[A] = s.get(currentRoot) match {
      case None => Root(currentRoot, depth).tap(a => println(s"\t\tRoot == value for  $currentRoot"))
      case Some(newRoot) => println(s"\t\tRoot of $currentRoot is found to be $newRoot")
        loop(newRoot, depth + 1)
    }
    loop(v, 0)
  })

  def union[A:Eq](p: A, q: A): QUMap[A, Unit] = {
    println(Console.GREEN_B + s"Union($p, $q)" + Console.RESET)
    for {
      rootOfP <- root(p)
      rootOfQ <- root(q)
      _ <- if(rootOfP.value === rootOfQ.value) State.get[Map[A,A]].map(map => {println(s"\tThe rootOfP EQ rootOfQ no change in map $map");map})
      else State.modify[Map[A,A]](m =>
        (if(rootOfP.depth <= rootOfQ.depth) m + (p -> rootOfQ.value) else m + (q -> rootOfP.value))
          .tap(map => println(s"\tThe rootOfP NEQ rootOfQ, depth ${rootOfP.depth} <= ${rootOfQ.depth}  and the new map is $map")))
    } yield ()
  }

  def connected[A:Eq](p: A, q: A): QUMap[A, Boolean] = {
    println(Console.GREEN_B + s"Connected($p, $q)" + Console.RESET)
    for {
      rootOfP <- root(p)
      rootOfQ <- root(q)
    } yield (rootOfP.value === rootOfQ.value).tap(b => println(s"\tConnected($p, $q) = $b"))
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
