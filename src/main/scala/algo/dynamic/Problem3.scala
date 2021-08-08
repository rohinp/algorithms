package algo.dynamic
import scala.util.chaining._

/* 
Magic Index: A magic index in an array A [e ... n -1] is defined to be an index such that 
  A[i] = i. 
Given a sorted array of distinct integers, write a method to find a magic index, if one exists, in
array A.
  FOLLOW UP
  What if the values are not distinct? 
*/
//This is the most simplest way by using scala lib functions.
//O(2n)
def magicIndex1(arr:Vector[Int]):Option[Int] = 
  arr.zipWithIndex.find(t => t._1 == t._2).map(_._1)

//Second approach, with more control and optimizations
//O(n) this is kind of weired though the solution
// as it takes care of two situation one of which is not mentioned in the question
//1. it finds multiple magic numbers
//2. it takes care if the list if not sorted.
def magicIndex2(arr:Vector[Int]):List[Int] = 
  def loop(start:Int,end:Int):List[Int] = 
    (start, end) match
      case _ if end - start < 2 => 
        List()
      case _ =>
        val mid = (start + end)/2
        if mid == arr(mid) then List(mid) else loop(start, mid) ++ loop(mid, end)
  loop(0,arr.length)
    

@main def problem3 =
  //magicIndex1(Vector(2,33,44,55,66,5,77,88,44,333,555,888,1000)) tap println
  magicIndex2(Vector(2,33,44,55,66,5,77,88,44,333,555,888,1000)) tap println
