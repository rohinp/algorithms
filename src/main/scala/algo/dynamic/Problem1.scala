package algo.dynamic

object Problem1:
  /* 
  Question:  A child is running up a staircase with n steps and can hop either 1 step, 2 steps, or 3
              steps at a time. Implement a method to count how many possible ways the child can run up the
              stairs.

  The question is more of counting the combinations, so when we say combinations we dont care of the 
  sequence of elements, what it means is [a,b] and [b,a] are the same. So lets start solving the question in a stepwise
  manner as we generally solve a recussion.

  Function to be implemented: def possibleCombinations(numOfStairs:Int):Int

  lets create an inner function which gives a list of possible steps the child takes to run on the stairs
  def loop(n:Int):List[List[Int]], to implement loop lets discuss the steps.

  1. step one what can be the simplest possible input for the function, 0 stairs.
    loop(0) => List(List())
  2. lets go one step by making the stairs to 1. List(1,2,3) 
    loop(1) => List(List(1))
  3. next for stairs num 2, List(1,2,3)
      2 - 1
      1 - 1
      0
      List(1,1)
      2 - 1
      1 - 2
      -1
      -------
      2 - 2
      0
      List(2)
      2 - 3
      -1
      List()

    loop(2) => List(List(1,1),List(2))
  4. next 3 List(1,2,3)
    loop(3) => List(List(1,1,1),List(1,2),List(3)) 
    3 - 1
    2 - 1
    1 - 1
    0
    List(1,1,1)
    3 - 1
    2 - 1
    1 - 2
    -1
    List()
    3 - 1
    2 - 2
    0
    List(1,2)
    3 - 2
    1 - 2
    -1
    List()
    3 - 2
    1 - 3
    -2
    List()
    3 - 3
    0
    List(3)
    3 - 2
    1 - 1
    0
    List(2,1)
  5. next 4
    loop(4) => List(List(1,1,1,1),List(1,1,2), List(2,2),List(1,3))
  6. next 5
    loop(5) => List(List(1,1,1,1,1),List(1,1,1,2),List(1,2,2),List(1,1,3),List(2,3))

    It might happen that you come up on a solution and it works but some how you are still not able to get your head around
    it. In that case lets take a step back and draw the tree of execution and check how things are executing.

                                                            loop(3)
                                                              |
                                               1step          | 2step            3step
                                  ----------------------------|--------------------------------
                                  |                           |                               |
                                loop(2)                     loop(1)                         loop(0) = [[]]
                                  |                            |                                                                                                  
                    1step         | 2step     3step)           |                                                                                                 
                 -----------------|----------------            |                                                                                                      
                 |                |                |           |                                                                                            
               loop(1)           loop(0)=[[]]   loop(-1)=[]    |                                                               
                 |                                             |                                                       
  1step          | 2step     3step)                            |                                                      
-----------------|----------------                             |                                           
|                |                |                            |                                           
loop(0)        loop(-1)         loop(-2)                       |                                            
[[]]            []                []                           |                                        
                                                               |    
                                                1step          | 2step     3step)     
                                              -----------------|----------------      
                                              |                |                |     
                                            loop(0)        loop(-1)         loop(-2)
                                            [[]]            []                []                                                                      
                 
1. We start at loop(3) where number of stairs are 3
2. The jumps we have are [1,2,3]
3. for every jump we spawn a new tree by subtrating it with stairs
4. we continue this till be hit the bottom.
5. In this case we have two bottom conditions
        a. if the star goes to 0
        b if its a negative number, we ignore this branch
For more clarity if you go through the tree in reverse order by concatinating the results you observe, you might hit the silver linning :-)        
               
  */
  //in our case jumps is hardcoded to List(1,2,3)
  def loop(num:Int):List[List[Int]] = 
    num match
      case 0 => List(List())
      case n if n < 0 => List()
      case n => for {
        j <- List(1,2,3)
        r <- loop(n - j)
      } yield j :: r

  /*
    Ok so the solution does not gives correct combinations as it deals [1,2] and [2,1] as two different answers
    What we need is a simple filter to get rid of the duplicates, plus the complexity is not so good.
    ---------
    |x |y   |
    ---------
    |2 |6   |
    ---------
    |3 |12  |
    ---------
    |4 |24  |
    ---------
    |5 |45  |
    ---------
    |6 |84  |
    ---------
    |7 |156 |
    ---------
    |8 |288 |
    ---------
    |9 |531 |
    ---------
    |10|978 |
    ---------
    from the sample input and number of steps to get the results are exponential. O(3^(n/2 + 1.5))
  */

  //n(stairs) -> List(List(jumps))
  val memo = scala.collection.mutable.Map(0 -> List(List.empty[Int]))
  def loop_mem(num:Int):List[List[Int]] =    
    num match
      case n if memo.contains(n) => memo(n)
      case n if n < 0 => List()
      case n => 
        val r = List(1,2,3).flatMap(j => loop_mem(n - j).map(l => (j :: l))).map(_.sorted).distinct
        memo.put(n, r)
        r

  /* 
      At this moment the complexity is O(3n), though not considerered the computation time for sort and distinct
      But we can improvise by making the inner list to set and then we dont need to do distinct.
  */

  //completing the solution by counting the number of combinations.
  def possibleCombinations(numOfStairs:Int):Int =
    val memo = scala.collection.mutable.Map(0 -> List(List.empty[Int]))
    def loop(num:Int):List[List[Int]] =    
      num match
        case n if memo.contains(n) => memo(n)
        case n if n < 0 => List()
        case n => 
          val r = List(1,2,3).flatMap(j => loop(n - j).map(l => (j :: l))).map(_.sorted).distinct
          memo.put(n, r)
          r
    loop(numOfStairs).length    
      
