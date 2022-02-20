package algo.basics

object Stack:
  import Node._
  opaque type Stack[T] = Node[T] | Null

  def apply[T](e:T):Stack[T] = Node(e)

  def push[T](e:T):Stack[T] => Stack[T] = 
    inserAtHead(e)

  def pop[T]:Stack[T] => Either[String,(T, Stack[T])] = 
    head => removeHead(head)
              .map(newHead => (head.data, newHead))
              .left.map(_ => "No more element on the stack to pop.")
    



  //reverse string with stack
  //solve balanced paranthesis with stack
  //pre, post and infix expression evaluation

  //next queue
  //enque, deque, isEmpty and peek operations
  //circular array que implementation 
    //next position = (current position + 1) % N
    //previous position  = (current position + N - 1) % N
  //Linked list based queue 
  //constant time operations.

end Stack