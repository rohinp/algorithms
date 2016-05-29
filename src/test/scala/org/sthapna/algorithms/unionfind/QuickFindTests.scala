package org.sthapna.algorithms.unionfind

import org.junit.Test
import org.scalatest.Matchers._
import org.sthapna.algorithms.unionFind.QuickFind

class QuickFindTests {

  @Test
  def itShouldNotHaveAnyConnectedElementBeforeUnionCommand(): Unit ={
    //given
    val quickFind = QuickFind(2)
    //when

    //then
    quickFind.connected(1,2) shouldBe false
  }

  @Test
  def itShouldFireUnionCommandForGivenTwoPoints(): Unit ={
    //given
    val quickFind = QuickFind(10)
    //when
    val actual = quickFind.union(1,5).union(2,4)

    //then
    actual.indexedList.keyValue should contain (1 -> 5)
    actual.indexedList.keyValue should contain (2 -> 4)
  }

  @Test
  def itShouldFindConnectedElementsAfterUnion(): Unit ={
    //given
    val quickFind = QuickFind(10)
    //when
    val actual = quickFind.union(1,5).union(2,4)

    //then
    actual connected(2,4) shouldBe true
    actual connected(1,5) shouldBe true

  }


}
