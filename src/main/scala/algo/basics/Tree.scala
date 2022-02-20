package algo.basics

import scala.math.Ordering.Implicits._
import scala.util.chaining._

case class BSTNode[T:Ordering] private (
  var left:BSTNode[T] | Null,
  var data:T,
  var right:BSTNode[T] | Null
):
  def leftNode(l:BSTNode[T]):BSTNode[T] = 
    this.left = l
    this

  def rightNode(r:BSTNode[T]):BSTNode[T] = 
    this.right = r
    this

  def update(d:T):BSTNode[T] = 
    this.data = d
    this

object BSTNode:
  def apply[T:Ordering](data:T):BSTNode[T] =
    BSTNode(null, data, null)

  def insert[T:Ordering](data:T):BSTNode[T] => BSTNode[T] =
    root => 
      val ord = summon[Ordering[T]]
      def loop(current:BSTNode[T]):BSTNode[T] =
        current match
          case BSTNode(null, d, _) if ord.lteq(data,d) =>
            current.leftNode(BSTNode(data))
            root
          case BSTNode(_, d, null) if ord.gteq(data, d) => 
            current.rightNode(BSTNode(data))
            root
          case BSTNode(left, d, _) if ord.lteq(data,d) => loop(left)
          case BSTNode(_, d, right) => loop(right)
      loop(root)

  def exists[T:Ordering](data:T)(root:BSTNode[T]): Boolean =
      val ord = summon[Ordering[T]]
      def loop(current:BSTNode[T] | Null):Boolean =
        current match
          case null  => false
          case BSTNode(left, d, right) if ord.equiv(data,d) => true
          case BSTNode(left, d, right) => loop(left) || loop(right)
      loop(root)

  def min[T:Ordering](root:BSTNode[T]): T =
      val ord = summon[Ordering[T]]
      def loop(current:BSTNode[T] | Null, minValue:T):T =
        current match
          case null  => minValue
          case BSTNode(left, d, _) => loop(left, d)
      loop(root, root.data)

  def max[T:Ordering](root:BSTNode[T]): T =
      val ord = summon[Ordering[T]]
      def loop(current:BSTNode[T] | Null, maxValue:T):T =
        current match
          case null  => maxValue
          case BSTNode(_, d, right) => loop(right, d)
      loop(root, root.data)

  def height[T](root:BSTNode[T]): Int =
    def loop(current:BSTNode[T] | Null, height:Int):Int =
      current match
        case null  => height
        case BSTNode(left, _, right) => 
          val l = loop(left, height + 1)
          val r = loop(right, height + 1)
          if l > r then l else r
    loop(root, 0) - 1

  def bfs_traverse[T, U:Ordering](root:BSTNode[T])(f:T => U): BSTNode[U] =
    val newRoot = BSTNode(f(root.data))
    type QueueType = (BSTNode[T] | Null, BSTNode[U] | Null)
    val queue = scala.collection.mutable.Queue[QueueType]((root, newRoot))

    def traverse():BSTNode[U] =
      if queue.isEmpty then newRoot else
        queue.dequeue match
          case (BSTNode(null, data, null), uNode) => 
            newRoot
          case (BSTNode(left, data, null), uNode) => 
            val newLeft = BSTNode(f(left.data))
            uNode.leftNode(newLeft)
            queue.enqueue((left, newLeft))
            traverse()
          case (BSTNode(null, data, right), uNode) => 
            val newRight = BSTNode(f(right.data))
            uNode.rightNode(newRight)
            queue.enqueue((right, newRight))
            traverse()
          case (BSTNode(left, data, right), uNode) => 
            val newLeft = BSTNode(f(left.data))
            val newRight = BSTNode(f(right.data))
            uNode.leftNode(newLeft)
            uNode.rightNode(newRight)
            queue.enqueue((left, newLeft), (right, newRight))
            traverse()

    traverse()

  //DFS traversals
  //root -> Left -> Right
  def preorder_traverse[T, U:Ordering](root:BSTNode[T])(f:T => U): BSTNode[U] =
    val newRoot = BSTNode(f(root.data).tap(println))

    def traverse(current:BSTNode[T], uNode:BSTNode[U]):BSTNode[U] =
      current match
        case BSTNode(null, data, null) => 
          newRoot
        case BSTNode(left, data, null) => 
          val newLeft = BSTNode(f(left.data).tap(println))
          uNode.leftNode(newLeft)
          traverse(left, newLeft)
        case BSTNode(null, data, right) => 
          val newRight = BSTNode(f(right.data).tap(println))
          uNode.rightNode(newRight)
          traverse(right, newRight)
        case BSTNode(left, data, right) => 
          val newLeft = BSTNode(f(left.data).tap(println))
          uNode.leftNode(newLeft)
          traverse(left, newLeft)
          val newRight = BSTNode(f(right.data).tap(println))
          uNode.rightNode(newRight)
          traverse(right, newRight)
    traverse(root, newRoot)

  //going forward,
  //I'll assume the result type is of same type for simplicity of solutions
  //Left -> Root -> Right
  def inorder_traverse[T:Ordering](root:BSTNode[T])(f:T => T): BSTNode[T] =
    def traverse(current:BSTNode[T] | Null):BSTNode[T] =
      current match
        case null => 
          root
        case BSTNode(left, data, right) => 
          traverse(left)
          current.update(f(data).tap(println))
          traverse(right)
    traverse(root)

  //Left -> Right -> Root
  def postorder_traverse[T:Ordering](root:BSTNode[T])(f:T => T): BSTNode[T] =
    def traverse(current:BSTNode[T] | Null):BSTNode[T] =
      current match
        case null => 
          root
        case BSTNode(left, data, right) => 
          traverse(left)
          traverse(right)
          current.update(f(data).tap(println))
    traverse(root)

  //Pending functions:
      //chek if the BT is a BST
      //delete a node from bst
          //case 1: No child
          //case 2: one child
          //case 3: 2 children (find min in right or max in left)
      //find succesor, inorder traversal of a BST.
end BSTNode

@main def bstMain =
  import BSTNode._

  given Ordering[Int] with 
    def compare(x: Int, y: Int): Int =
      if x < y then -1 else if x > y then 1 else 0

  BSTNode(15)
    .pipe(insert(10))
    .pipe(insert(20))
    .pipe(insert(25))
    .pipe(insert(8))
    .pipe(insert(12))
    .tap(println)
    .tap(bst => println(s" exists 8 = ${exists(8)(bst)}"))
    .tap(bst => println(s" exists 18  = ${exists(18)(bst)}"))
    .tap(bst => println(s" min is = ${min(bst)}"))
    .tap(bst => println(s" max is = ${max(bst)}"))
    .tap(bst => println(s" height is = ${height(bst)}"))
    .tap(println)
    .tap(bst => println(bfs_traverse(bst)(_ * 10)))
    .tap(_ => println("-"*50))
    .tap(bst => println(preorder_traverse(bst)(identity)))
    .tap(_ => println("-"*50))
    .tap(bst => println(inorder_traverse(bst)(identity)))
    .tap(_ => println("-"*50))
    .tap(bst => println(postorder_traverse(bst)(identity)))
