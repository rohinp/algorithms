package learning.algotithm.one.unionfind

import cats.Eq
import cats.implicits._
import cats.data.State
import learning.algotithm.one.input.FileInput._
import learning.algotithm.one.Main
object UF_v1 extends Main {

  /*
  * This is a very naive implementation of quick-find without thinking the complexity of the algorithm
  *
  * The data structure is a simple List of sets
  * 1. It recursively iterates over the list of set
  * 2. pSet, qSet are like temporary sets holding the set containing pSet or qSet respectively
  * 3. acc is the accumulator which just collects the sets one by one
  * 4. To understand the recursion just check the first 8 conditions which terminates the loop
  *   1. the list is empty, no pSet or qSet found
  *   2. qSet was found and list is empty
  *   3. pSet was found and list is empty
  *   4. both pSet and qSet was found and list is empty
  *   5. both pSet and qSet was found and list is not empty
  *   6 both p and q found in remaining set
  *   7 q was already found and p was found in remaining set
  *   8 p was already found and q was found in remaining set
  * 5. Remaining 5 conditions where either
  *   1. q is already found and looking for p
  *   2. p is already found and looking for q
  *   3. p is found in the remaining set, continue looking for q (in this case p and q both sets were empty)
  *   4. q is found in the remaining set, continue looking for p (in this case p and q both sets were empty)
  *   5. last case both p and q are not found in remaining set so continue further
  *
  * Note: There are cases which can be merged into one but still explicitly mentioned for the sake of readability.
  *
  *
  * Algorithm Analysis by sampling data:
  * 1. First union call with no element in the list
  *   a. one of the terminate condition will get fired and the list will be created in one operation
  *   b. so the first add will be O(1)
  * 2. If both doesn't exists in the list of set
  *   a. traverse the entire list, so n traversals
  *   b. check exists on each set which is n` operations
  *   b. Operations are O(n * n`)
  * 3. If worst case both are there in the list and they are the last and second last set of the list
  *   a. n traversals
  *   b. n` exists calls
  *   c. Operations are O(n * n`)
  *
  * n` may or may not be bigger that n
  * assuming same size worst case the complexity is O(n2)
  * */

  //data structure
  type UFList[A,R] = State[List[Set[A]], R]

  def union[A: Eq](p:A, q:A): UFList[A,Unit] =
        State.modify { ufList => {
            @scala.annotation.tailrec
            def loop(pSet: Set[A], qSet: Set[A], acc: List[Set[A]], remainingSets: List[Set[A]]): List[Set[A]] = (pSet.isEmpty, qSet.isEmpty, remainingSets) match {
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

  //Sample Run
  override def execute():Unit = println((for {
    _ <- allUnions
    r <- connected(4,8)
  } yield r).run(List()).value._2)
}
