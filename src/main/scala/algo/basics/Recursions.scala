package algo.basics

object Recursions:
  /**
    Pretext: 

    The assense of recursion is prety simple, 
      1. finding the base case(r) or termination condition(s)
      2. finding the repetative task(s) in the algorithm

    Though for some problem statements it is easy to figure out the repetative tasks.
    But in complicated cases it might be difficult to get it, but with some practice it becomes 
    quite obvious. At some point you might realise the repetative patterns 
    are quite often similar and thus becomes easy to solve such problem statements.

    So with out furthur delay, lets put some simple recursion examples and their solutions.

    We wont start with factorial, because it is so often used that most of the developwers 
    out there just remember the solution. So lets take a different example here.

    Compute the nth Fibonacci number

    While the step to find out our recursive solution is
    1. Write some sample examples, and fidn out the most simple step which your algorithm requires.
      First the fib numbers for reference: 0 1 1 2 3 5 8 13 ...      
      In this case the expected solution is the nth finonacci number so, what is the 0th and first 
      fibonacci number, well the answer we all know it's fib(0) = 0 and fib(1) = 1
      now lets write some more examples
      fib(2) = fib(0) + fib(1) // second position
      fib(3) = fib(1) + fib(3) // third position
      fib(4) = fib(2) + fib(2) // third position
      ... and so on
    2. We see the repetative pattern, now lets put down a generic implementation based on the examples:
      fib(n) = fib(n-2) + fib(n-1)
    3. What is the base case in our algorithm, we have already identified those in step 1 though.
        fib(0) and fib(1)
    4. Now that we have generalised the repetative pattern and the base cases it is very simple to 
        write the implementation.
  **/
  def fib(nth:Int):Int = nth match
    case 0 | 1 => nth
    case n => fib(n-2) + fib(n-1)

  /**
   * Alhtough there is nothing really wrong with the above solution but it literally evaluates the
   * same steps again and again while doing recursion. And because of that the complexity of the 
   * algorithm is bad, lets examine the time complexity.
   * 
   * We could do in differnet ways, one naive way is to just print the number of times the evaluation is happening.
   * Also we can add a counter to check how many evaluations took place
   * var counter = 0
   * def fib(nth:Int):Int = nth match
      case 0 | 1 => nth
      case n => 
        counter += 1
        println(s"Evaluating for $n")
        fib(n-2) + fib(n-1)

      println("number of steps $counter")
  * 
  *One more way is to draw a graph for example
                                                    fib(5)
                                                       |
                                  fib(4)---------------|--------------fib(3) 
                                    |                                     |
                  fib(3)------------|-----fib(2)                fib(2)----|----fib(1)
                     |                       |                     |
            fib(2)---|---fib(1)     fib(1)---|---fib(0)   fib(1)---|---fib(0) 
               |
      fib(1)---|---fib(0)
    
    Drawing graph really shows how the steps are repeated and more specifically its a binary tree.
    It makes sense to conclude that the number of operations are in the order of 2^n.
    Though in case of fib it is not actually 2^n but near by it, why this is the case because
    we can see how the number of computations are evaluated from the above graph.

    Well we can plot a graph with sample inputs and come up with an exact complexity.
    Which is some where near 1.6^n.

    Okay so the conclusion is recurssions in general can be represented as a tree of evaluations.
    Which inturn helps us to get to the complexity of the algorithm and also how we can 
    write.

    The questions is how do we improvise the algorithm. In general there is one very simple
    technique called memoization.
    Memoization means in this context is cache the results and resuse it so that computations
    which are reduntant wont get evaluated again and again.

    Lets implement that in our code.
  * **/
  val fib_memo = scala.collection.mutable.Map(0 -> 0, 1 -> 1) //smartly initialized with base cases ;-)
  def fib_(nth:Int):Int = nth match
    case n if fib_memo.contains(n) => fib_memo(n)
    case n => 
      val first = (n-2) -> fib_(n-2)
      val second = (n-1) -> fib_(n-1)
      fib_memo.addAll(List(first,second))
      fib_memo(n-2) + fib_memo(n-1)

  /**
   * Again this can be visualized with a binary tree by removing the redundant computations
   * Also can analize the number of steps by using a counter.
   * 
   * 
   * Lets do a litte more complicated example, finding all posible combinations of the given
   * list of elements:
   * For example List(a,b,c) = List(List(a),List(b),List(c),List(a,b),List(b,c),List(a,c),List(a,b,c))
   * 
   * Inorder to come up with a solution we will start with small cases/samples. or we can say
   * start with smallest possible input.
   * 
   * ##1
   * [] = [[]]
   * [a] = [[a]]
   *
   * In a step by step approach 
   *  
   * ##2
   * [] = [[]]
   * [a] = [[a]]
   * [b] = [[b]]
   * [a,b] = [[a],[b],[a,b]]
    def combination[A](list:List[A]):List[List[A]] =
      list match 
        case Nil => List(List())
        case x::Nil => List(List(x))
        case x::xs => 
          combination(List(x)) ++ combination(xs) ++ combination(xs).map(l => x :: l)

  combination([a,b,c])

  combination([a]) ++ combination([b,c]) ++ combination([b,c]).map(l => a :: l)
  [[a]] ++ combination([b]) ++ combination([c]) ++ combination([c]).map(l => b :: l) ++ (combination([b]) ++ combination([c]) ++ combination([c]).map(l => b :: l)).map(l => x :: l)
  [[a]] ++ [[b]] ++ [[c]] ++ [[b,c]] ++ ([[b]] ++ [[c]] ++ [[b,c]]).map(l => a :: l)
  [[a]] ++ [[b]] ++ [[c]] ++ [[b,c]] ++ [[a,b]] ++ [[a,c]] ++ [[a,b,c]]
  [[a],[b],[c],[b,c],[a,b],[a,c],[a,b,c]]

   * 
   * 
   * ##3
   * [] = [[]]
   * [a] = [[a]]
   * [b] = [[b]]
   * [c] = [[c]]
   * [a,b] = [[a],[b],[a,b]]
   * [b,c] = [[b],[c],[b,c]]
   * [a,c] = [[a],[c],[a,c]]
   * [a,b,c] = [[a],[b],[c], [a,b],[a,c],[b,c],[a,b,c]]
   * 
   * From the sampleling above we can easily make out that we need base case to handle empty list and list with single element.
   * In below code we can see the base cases:
   * case Nil and case x::Nil
   * 
   * so the code combination(List(x)) ++ combination(xs) actually gives List(List(a),List(b),List(c))
   * and then we need a seperate recursive tree runnning to loop on the remeining elements and add the previous to it.
   * i.e. combination(xs).map(e => x :: e)
   * 
   * we can also visualise the problem in a tree and come up with a similar conclusion.
  
   * **/


  def combination[A](list:List[A]):List[List[A]] =
    list match 
      case Nil => List(List())
      case x::Nil => List(List(x))
      case x::xs => 
        combination(List(x)) ++ combination(xs) ++ combination(xs).map(e => x :: e)          

end Recursions
  