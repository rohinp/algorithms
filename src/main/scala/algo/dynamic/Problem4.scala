package algo.dynamic

//Power Set: Write a method to return all subsets of a set.

/* 
This was already covered in the basics/Recursions.scala 
Will put here the soltuion without explanation.
*/

def combination[A](list:List[A]):List[List[A]] =
    list match 
      case Nil => List(List())
      case x::Nil => List(List(x))
      case x::xs => 
        combination(List(x)) ++ combination(xs) ++ combination(xs).map(e => x :: e)