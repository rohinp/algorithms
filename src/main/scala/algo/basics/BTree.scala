package algo.basics

import scala.math.Ordering.Implicits._
import scala.util.chaining._
import scala.annotation.tailrec
import scala.collection.immutable.Queue

sealed trait BTree[+T]:
  def left: BTree[T]
  def data: T
  def right: BTree[T]
  def isEnd: Boolean
  def isLeaf: Boolean
  def isBST: Boolean

  override def toString: String =
    this match {
      case BTree.BEnd => "~"
      case BTree.BNode(l, d, r) =>
        s"(${l.toString} <-- ${Console.RED}$d${Console.RESET} --> ${r.toString})"
    }
end BTree

object BTree:

  case object BEnd extends BTree[Nothing]:
    def left: BTree[Nothing] = throw new IllegalArgumentException
    def data: Nothing = throw new IllegalArgumentException
    def right: BTree[Nothing] = throw new IllegalArgumentException
    def isEnd: Boolean = true
    def isLeaf: Boolean = false
    def isBST: Boolean = throw new IllegalArgumentException
  end BEnd

  case class BNode[+T: Ordering](left: BTree[T], data: T, right: BTree[T])
      extends BTree[T]:

    def isEnd: Boolean = false
    def isLeaf: Boolean = left.isEnd && right.isEnd

    def isBST: Boolean =
      val ord = summon[Ordering[T]]
      def traverse(remaining: List[BTree[T]]): Boolean =
        if remaining.isEmpty then true
        else
          remaining.head match {
            case BEnd =>
              traverse(remaining.tail)
            case n @ BNode(l, d, r) if n.isLeaf =>
              traverse(remaining.tail)
            case n @ BNode(l, d, BEnd) =>
              if ord.lteq(l.data, d) then traverse(remaining.tail) else false
            case n @ BNode(BEnd, d, r) =>
              if ord.lteq(d, r.data) then traverse(remaining.tail) else false
            case n @ BNode(l, d, r) =>
              if ord.lteq(l.data, d) && ord.lteq(d, r.data) then
                traverse(l :: r :: remaining.tail)
              else false

          }
      traverse(List(this))

  end BNode

  def apply[T: Ordering](data: T): BTree[T] =
    BNode(BEnd, data, BEnd)

  def insert[T: Ordering](data: T): BTree[T] => BTree[T] =
    root =>
      val ord = summon[Ordering[T]]
      @tailrec
      def loop(
          remaining: List[BTree[T]],
          visited: Set[BTree[T]],
          acc: List[BTree[T]]
      ): BTree[T] =
        if remaining.isEmpty then acc.head
        else
          remaining.head match
            case BEnd =>
              loop(remaining.tail, visited, BEnd :: acc)

            //if node already visited.
            case n if visited.contains(n) =>
              if n.isLeaf then loop(remaining.tail, visited, n :: acc)
              else
                val node1 :: node2 :: remainingAcc = acc
                val tmp =
                  if ord.lteq(node1.data, n.data) then
                    BNode(left = node1, n.data, right = node2)
                  else BNode(left = node2, n.data, right = node1)
                loop(
                  remaining.tail,
                  visited + tmp,
                  tmp :: remainingAcc
                )

            //for leaf node
            case n @ BNode(BEnd, d, BEnd) =>
              val newNode =
                if ord.lteq(data, d) then n.copy(left = BTree(data))
                else n.copy(right = BTree(data))
              loop(remaining.tail, visited, newNode :: acc)

            //for node with left as bend
            case n @ BNode(BEnd, d, right) if ord.lteq(data, d) =>
              val newNode = n.copy(left = BTree(data))
              loop(remaining.tail, visited, newNode :: acc)

            //for node with right as bend
            case n @ BNode(left, d, BEnd) if ord.lteq(d, data) =>
              val newNode = n.copy(right = BTree(data))
              loop(remaining.tail, visited, newNode :: acc)

            case n @ BNode(left, d, right) =>
              if ord.lteq(data, d) then
                loop(left :: remaining, visited + n, right :: acc)
              else loop(right :: remaining, visited + n, left :: acc)

      loop(List(root), Set(), List())

  def exists[T: Ordering](data: T)(root: BTree[T]): Boolean =
    val ord = summon[Ordering[T]]
    @tailrec
    def loop(current: List[BTree[T]]): Boolean =
      if current.isEmpty then false
      else
        current.head match
          case BEnd =>
            loop(current.tail)
          case BNode(left, d, right) if ord.equiv(data, d) => true
          case BNode(left, d, right) => loop(left :: right :: current.tail)
    loop(List(root))

  def min[T: Ordering](root: BTree[T]): T =
    val ord = summon[Ordering[T]]
    @tailrec
    def loop(current: BTree[T], minValue: T): T =
      current match
        case BEnd              => minValue
        case BNode(left, d, _) => loop(left, d)

    loop(root, root.data)

  def max[T: Ordering](root: BTree[T]): T =
    val ord = summon[Ordering[T]]
    @tailrec
    def loop(current: BTree[T], maxValue: T): T =
      current match
        case BEnd               => maxValue
        case BNode(_, d, right) => loop(right, d)
    loop(root, root.data)

  def height[T](root: BTree[T]): Int =
    def loop(current: List[BTree[T]], lHeight: Int, rHeight: Int): Int =
      if current.size == 0 then (if lHeight > rHeight then lHeight else rHeight)
      else
        current.head match
          case BEnd =>
            loop(current.tail, lHeight, rHeight)
          case BNode(BEnd, _, BEnd) =>
            loop(current.tail, lHeight, rHeight)
          case BNode(BEnd, _, right) =>
            loop(right :: current.tail, rHeight, rHeight + 1)
          case BNode(left, _, BEnd) =>
            loop(left :: current.tail, lHeight + 1, lHeight)
          case BNode(left, _, right) =>
            loop(left :: right :: current.tail, lHeight + 1, rHeight + 1)
    loop(List(root), 0, 0)

  def bfs_traverse[T](root: BTree[T]): List[T] =
    def traverse(
        remaining: List[BTree[T]],
        acc: Queue[T]
    ): List[T] =
      if remaining.isEmpty then acc.toList
      else
        remaining.head match
          case BEnd =>
            traverse(remaining.tail, acc)
          case BNode(l, d, r) =>
            traverse(
              remaining.flatMap {
                case BEnd           => List()
                case BNode(l, d, r) => List(l, r)
              },
              remaining.foldLeft(acc) {
                case (queue, BEnd)           => queue
                case (queue, BNode(_, d, _)) => queue.enqueue(d)
              }
            )

    traverse(List(root), Queue.empty)

end BTree

@main def bstMain =
  import BTree._

  given Ordering[Int] with
    def compare(x: Int, y: Int): Int =
      if x < y then -1 else if x > y then 1 else 0

  val tree = BNode(
    BNode(
      BEnd,
      10,
      BNode(BEnd, 12, BEnd)
    ),
    15,
    BNode(
      BNode(
        BEnd,
        17,
        BNode(
          BNode(BEnd, 20, BEnd),
          25,
          BNode(BEnd, 27, BEnd)
        )
      ),
      30,
      BEnd
    )
  )

  def randomeTree: BTree[Int] =
    (1 to 100).foldLeft[BTree[Int]](BEnd)((acc, v) => BNode(acc, v, BEnd))

  tree
    .tap(t => println(s"is binnary tree = ${t.isBST}"))
    .pipe(insert(9))
    .pipe(insert(28))
    .tap(println)
    .tap(t => println(s"exists 12 = ${exists(12)(t)}"))
    .tap(t => println(s"exists 100 = ${exists(100)(t)}"))
    .tap(t => println(s"minimum in the tree = ${min(t)}"))
    .tap(t => println(s"max in the tree = ${max(t)}"))
    .tap(t => println(s"height in the tree = ${height(t)}"))
    .tap(t => println(s"bfs traversal = ${bfs_traverse(t)}"))

  println(s"${"-" * 50}Bigger tree${"-" * 50}")
  println(s"${"-" * 50}___________${"-" * 50}")

  randomeTree
    .tap(t => println(s"height of the bigger tree = ${height(t)}"))
