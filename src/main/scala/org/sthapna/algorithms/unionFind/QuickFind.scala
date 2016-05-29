package org.sthapna.algorithms.unionfind

import org.sthapna.algorithms.unionfind.datastructures.IndexedList

trait QuickFind {
  val indexedList: IndexedList
  def union(start:Int, end:Int):QuickFind
  def connected(start:Int, end:Int):Boolean
}

case class NotConnectedQuickFind(size:Int) extends QuickFind {
  val indexedList = IndexedList(size)
  override def union(start: Int, end: Int): QuickFind = QuickFind(indexedList.modify(start,end))
  override def connected(start: Int, end: Int): Boolean = false
}

case class ConnectedQuickFind(val indexedList: IndexedList) extends QuickFind {
  override def union(start: Int, end: Int): QuickFind =
    QuickFind(indexedList.modify(indexedList.allKeysWithSameValueAs(start),end))

  override def connected(start: Int, end: Int): Boolean = indexedList.valueOf(start) == indexedList.valueOf(end)
}

object QuickFind {
  def apply(size: Int) = NotConnectedQuickFind(size)
  def apply(indexedList: IndexedList) = ConnectedQuickFind(indexedList)
}
