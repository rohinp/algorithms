package algo.edx
import math.Ordering.Implicits.infixOrderingOps

def mergeSort[T:Ordering](lst:Vector[T]):Vector[T] = 
  def merge(l:Vector[T],r:Vector[T]):Vector[T] = 
      (l,r) match 
        case (x+:_, y+:ys) if x < y => 
          val (addToacc, newXs) = l.partition(_ <= y)
          addToacc ++ Vector(y) ++ merge(newXs,ys)
        case (x+:xs, y+:ys) => 
          val (addToacc, newYs) = r.partition(_ <= x)
          addToacc ++ Vector(x) ++merge(xs,newYs)
        case (xs,ys) if xs.isEmpty => ys
        case (xs,ys) => xs
  
  lst.splitAt(lst.length / 2) match
    case (Vector(x), Vector(y)) => if x < y then Vector(x,y) else Vector(y,x)
    case (left, Vector(y)) => merge(mergeSort(left), Vector(y))
    case (Vector(x), right) => merge(Vector(x), mergeSort(right))
    case (left, right) => merge(mergeSort(left),mergeSort(right))
  

def quickSort[T:Ordering](lst:List[T]):List[T] =
  val left:T => List[T] => List[T] = e => l => l.filter(_ <= e)
  val right:T => List[T] => List[T] = e => l => l.filter(_ > e)
  lst match
    case Nil => Nil
    case x::xs => 
      quickSort(left(x)(xs)) ++ List(x) ++ quickSort(right(x)(xs))