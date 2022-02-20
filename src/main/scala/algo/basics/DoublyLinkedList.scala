package algo.basics

import scala.util.chaining._

case class DNode[E] private (
  var prev:DNode[E] | Null,
  val data:E, 
  var next:DNode[E] | Null
  ):
  def next(n:DNode[E]):DNode[E] = 
    this.next = n
    this
  def prev(p:DNode[E]):DNode[E] = 
    this.prev = p
    this

  override def toString:String = 
    def loop(in:DNode[E], acc:String):String =     
        in match
          case DNode(_, d, null) => if acc == "DLinkedList(" then s"$acc$d)" else s"$acc, $d)"
          case DNode(_, d, next) => loop(next, if acc == "DLinkedList(" then s"$acc$d" else s"$acc, $d")
    loop(this, "DLinkedList(")

  def gotoLastNode:DNode[E] = 
    this match
      case lastNode@DNode(_, _, null) => lastNode
      case DNode(_, _, next) => next.gotoLastNode

  def reversePrint:String = 
    def loop(in:DNode[E], acc:String):String =     
        in match
          case DNode(null, d, _) => if acc == "DLinkedList(" then s"$acc$d)" else s"$acc, $d)"
          case DNode(prev, d, _) => loop(prev, if acc == "DLinkedList(" then s"$acc$d" else s"$acc, $d")
    loop(gotoLastNode, "DLinkedList(")

object DNode:
  
  def apply[E](e:E):DNode[E] = DNode(null, e, null)

  def insertAtHead[E](e:E):DNode[E] => DNode[E] = 
    head =>
      val newHead = DNode(e)
      head.prev(newHead)
      newHead.next(head)

  def insertAtTail[E](e:E):DNode[E] => DNode[E] = 
    head => 
      val lastNode = head.gotoLastNode
      lastNode.next(DNode(e).prev(lastNode))
      head

end DNode

@main def dllmain =
  import DNode._

  DNode(3)
    .tap(println)
    .pipe(insertAtHead(2))
    .pipe(insertAtHead(1))
    .pipe(insertAtHead(0))
    .pipe(insertAtTail(4))
    .tap(println)
    .tap(ll => println(ll.reversePrint))