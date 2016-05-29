package org.sthapna.algorithms.unionFind.datastructures

trait IndexedList {
  def allKeysWithSameValueAs(index: Int):List[Int]
  val keyValue: Map[Int, Int]
  def modify(is: List[Int],elem: Int):IndexedList
  def modify(index: Int, element: Int): IndexedList
  def valueOf(index: Int):Int
}

private case class NonEmptyIndexedList(val keyValue: Map[Int, Int], val size:Int) extends IndexedList {

  override def valueOf(index: Int): Int =
    keyValue.get(index).getOrElse(throw new IndexedList.NoSuchIndexExists)

  override def modify(index: Int, element: Int): IndexedList =
    if(isValidIndex(index) && isValidElement(element))
      IndexedList(keyValue.updated(index,element))
    else throw new IndexedList.NoSuchIndexExists

  private def isValidIndex(i:Int) = if(i > 0 && i <= size) true else false
  private def isValidElement(e:Int) = isValidIndex(e);

  override def modify(is: List[Int],elem: Int): IndexedList = is match {
    case List() => this
    case x::Nil => modify(x,elem)
    case x::xs  => modify(x,elem).modify(xs,elem)
  }

  override def allKeysWithSameValueAs(index: Int): List[Int] =
    keyValue.filter(e => e._2 == valueOf(index)).map(k => k._1).toList
}

private case class EmptyIndexedList() extends IndexedList {
  override val keyValue: Map[Int, Int] = Map()
  override def valueOf(index: Int): Int = throw new IndexedList.NoSuchIndexExists
  override def modify(index: Int, element: Int): IndexedList = throw new IndexedList.NoSuchIndexExists
  override def modify(is: List[Int],elem: Int): IndexedList = modify(0,0)
  override def allKeysWithSameValueAs(index: Int): List[Int] = List()
}

object IndexedList {
  def apply(size:Int):IndexedList = {
    if(size > 0)
      NonEmptyIndexedList ((1 to size).map(e => (e,e)).toMap[Int,Int], size)
    else EmptyIndexedList()
  }

  def apply(il: Map[Int, Int]):IndexedList = NonEmptyIndexedList (il,il.size)

  case class NoSuchIndexExists() extends RuntimeException
}
