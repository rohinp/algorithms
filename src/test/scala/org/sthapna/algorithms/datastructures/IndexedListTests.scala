package org.sthapna.algorithms.datastructures

import org.junit.Test
import org.scalatest.Matchers._

class IndexedListTests {

  @Test
  def itShouldCreateEmptyIndexedListForZeroOrLessSize(): Unit ={
    //given
    val list = IndexedList(0);
    //when

    //then
    list.keyValue shouldBe empty
  }

  @Test
  def itShouldCreateAutoGeneratedIndexedIntList(): Unit ={
    //given
    val list = IndexedList(2);
    //when

    //then
    list.keyValue should contain (1,1)
    list.keyValue should contain (2,2)

  }


  @Test(expected = classOf[IndexedList.NoSuchIndexExists])
  def itShouldThrowExceptionForInvalidPosition_EmptyList(): Unit ={
    //given
    val list = IndexedList(0);
    //when
    list.valueOf(5)
    //then
  }

  @Test(expected = classOf[IndexedList.NoSuchIndexExists])
  def itShouldThrowExceptionForInvalidPosition(): Unit ={
    //given
    val list = IndexedList(3);
    //when
    list.valueOf(5)
    //then
  }

  @Test
  def itShouldGetElementFromGivenPosition(): Unit ={
    //given
    val list = IndexedList(10);
    //when

    //then
    list.valueOf(5) shouldEqual 5
  }

  @Test(expected = classOf[IndexedList.NoSuchIndexExists])
  def itShouldThrowExceptionForModifyingElementInEmptyList(): Unit ={
    //given
    val list = IndexedList(0);
    //when
    list.modify(2,5)
    //then
  }

  @Test(expected = classOf[IndexedList.NoSuchIndexExists])
  def itShouldThrowExceptionForModifyingElementIndexDoNotExists(): Unit ={
    //given
    val list = IndexedList(10);
    //when
    list.modify(12,5)
    //then
  }

  @Test(expected = classOf[IndexedList.NoSuchIndexExists])
  def itShouldThrowExceptionForModifyingValueIndexDoNotExists(): Unit ={
    //given
    val list = IndexedList(10);
    //when
    list.modify(5,15)
    //then
  }

  @Test
  def itShouldModifyAGiveIndexValue(): Unit ={
    //given
    val list = IndexedList(3);
    //when
    val actual = list.modify(1,2)
    //then
    actual.keyValue should contain (2 -> 2)
  }

  @Test
  def itShouldModifyMultipleGiveIndexValue(): Unit ={
    //given
    val list = IndexedList(5);
    //when
    val actual = list.modify(List(2,3),1)

    //then
    actual.keyValue should contain (2 -> 1)
    actual.keyValue should contain (3 -> 1)
  }

  @Test
  def itShouldGetAListOfIndexWhereTheGivenKeysValueMatches(): Unit ={
    //given
    val il = IndexedList(10).modify(List(2,3,5),1)

    //when
    val actual:List[Int] = il.allKeysWithSameValueAs(5)

    //then
    actual should contain allOf (2,3,5,1)

  }
}
