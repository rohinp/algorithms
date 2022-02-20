package algo.basics

import scala.util.chaining._

case class Node[E] private (val data:E, var next:Node[E] | Null):
  def update(n:Node[E]):Node[E] = 
    this.next = n
    this

  def head:E = data
  
  override def toString:String = 
    def loop(in:Node[E], acc:String):String =     
        in match
          case Node(d, null) => if acc == "LinkedList(" then s"$acc$d)" else s"$acc, $d)"
          case Node(d, next) => loop(next, if acc == "LinkedList(" then s"$acc$d" else s"$acc, $d")
    loop(this, "LinkedList(")

object Node:
  import Node._ 
  def apply[T](data:T):Node[T] = Node(data, null)

  def append[T](data:T):Node[T] => Node[T] = 
    root => 
      def loop(in:Node[T]):Node[T] =     
        in match
          case Node(_, null) => 
            in.update(Node(data))
            root
          case Node(_, next) => loop(next)
      loop(root)

  def insertAt[T](position:Int, data:T):Node[T] => Either[String,Node[T]] =
    root => 
      def loop(in:Node[T], index:Int):Either[String,Node[T]] =     
          if position <= 0 then Left(s"Index out of bound, where position = $position")
          else 
            in match
              case Node(_, null) if index == (position - 1) => 
                in.update(Node(data))
                Right(root)
              case Node(_, null) => 
                Left(s"Index out of bound, where position = $position")
              case Node(_, next) if index == (position -1) => 
                if position == 1 then Right(Node(data, in)) 
                  else 
                    in.update(Node(data, next))
                    Right(root)
              case Node(_, next) => loop(next, index + 1)
      loop(root, 0)

  def deleteAt[T](position:Int):Node[T] => Either[String,Node[T]] =
    root => 
      def loop(in:Node[T], index:Int, prevNode:Node[T] | Null):Either[String,Node[T]] =     
          if position <= 0 then Left(s"Index out of bound, where position = $position")
          else 
            (in,prevNode) match
              case (Node(_, null), null) if index == (position - 1) => 
                Left(s"Can't delete from a single element linked list = $root")
              case (Node(_, null), prev) if index == (position - 1) => 
                prev.update(null) //delete last
                Right(root) 
              case (Node(_, null), prev) => 
                Left(s"Index out of bound, where position = $position")
              case (Node(_, next), null) if index == (position -1) => 
                root.update(null) 
                Right(next) //delete first
              case (Node(_, next), prev) if index == (position -1) => 
                in.update(null)
                prev.update(next) //non edge delete
                Right(root)
              case (Node(_, next), _) => loop(next, index + 1, in)
      loop(root, 0, null)

  def reverse[T]:Node[T] => Node[T] =
    root => 
      def loop(next:Node[T], prevNode:Node[T] | Null):Node[T] = 
        (next, prevNode) match
          case (Node(_, null), null) => next //just root node
          case (Node(_, null), Node(_,_)) => next.update(prevNode) //last node
          case (Node(_, n), _) => loop(n, next.update(prevNode)) //intermidiate nodes
          
      loop(root, null)

  def inserAtHead[T](data:T):Node[T] => Node[T] =
    Node(data, _)

  def removeHead[T]:Node[T] => Either[String,Node[T]] =
      deleteAt(1)

end Node

@main def llmain =
  import Node._
  println("Here is output..")
  Node(1)
    .pipe(append(2))
    .pipe(append(3))
    .pipe(append(4))
    .pipe(append(5))
    .tap(println)
    .pipe(insertAt(2,33))
    .tap(println)
    .pipe(_.flatMap(insertAt(1,111)))
    .tap(println)
    .pipe(_.flatMap(deleteAt(1)))
    .tap(println)
    .pipe(_.flatMap(deleteAt(6)))
    .tap(println)
    .pipe(_.flatMap(deleteAt(3)))
    .tap(println)
    .pipe(_.map(reverse))
    .tap(println)