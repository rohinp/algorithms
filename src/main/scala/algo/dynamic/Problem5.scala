package algo.dynamic

import scala.util.chaining._
/* 

Recursive Multiply: Write a recursive function to multiply two positive integers without using the
* operator. You can use addition, subtraction, and bit shifting, but you should minimize the number
of those operations.

*/

object Test:
  /* 
    This one is the most simplest solution by using standard scala lib functions.
    Logic is prety simple, if we try to recollect 
      1. how two binary numbers can be multipled
      2. And the left shift operator works

    Though scala already provides functions like toBinaryString and zipindex etc.
    Which makes solution look extremely simple.

    Though if we check we are traversing the binary string three times.
    We can minimise this by implementing a custom recursion.
  */
  def multiply(x:Int, y:Int):Int = 
    x.toBinaryString
    .reverse
    .zipWithIndex
    .foldLeft(0)((acc, t) => if t._1 == '1' then acc + (y << t._2) else acc)

  /* 
    The custom recursion has much less steps then the above solution.
    Just one traverse on the binary String and left shift as many time we get a '1'
  */  
  def multiply1(x:Int, y:Int):Int = 
    def loop(binaryOfX:List[Char],index:Int):Int = 
      binaryOfX match
        case '0'::xs => loop(xs,index - 1)
        case '1'::xs => (y << index) + loop(xs,index - 1)
        case _ => 0

    val binaryOfX = x.toBinaryString
    loop(binaryOfX.toList, binaryOfX.length - 1)


@main def problem5 =
  import Test._
  multiply(64, 45) tap println
  multiply1(64, 45) tap println