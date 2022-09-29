package algo.graph

object Graph {
  type Graph[T] = Map[T, Set[T]]

  val socialNetwork: Graph[String] = Map(
    "Alice" -> Set("Bob", "Charlie", "David"),
    "Bob" -> Set(),
    "Charlie" -> Set("David"),
    "David" -> Set("Bob", "Mary"),
    "Mary" -> Set("Bob", "Charlie")
  )

  def outDegree(graph: Graph[String], node: String): Int = {
    graph.get(node).getOrElse(Set.empty).size
  }

  def inDegree(graph: Graph[String], node: String): Int = {
    graph.foldLeft(0) {
      case (counter, (_, values)) => {
        if (values.contains(node)) {
          counter + 1
        } else counter
      }
    }
  }

  def isPath[T](graph: Graph[T], start: T, end: T): Boolean = {

    def loop(remaining: List[T], visited: Set[T]): Boolean = remaining match {
      case Nil                         => false
      case head :: tail if head == end => true
      case head :: tail if visited.contains(head) =>
        loop(tail, visited)
      case head :: tail =>
        loop(remaining ++ graph(head), visited + head)
    }
    def loop_(acc: Set[T]): Boolean = {
      if (acc.isEmpty) {
        false
      } else if (acc.contains(end)) {
        true
      } else {
        loop_(acc.flatMap(graph.get(_).getOrElse(Set.empty)) - start)
      }
    }
    loop(List(start), Set())
  }

  /*
  val socialNetwork: Graph[String] = Map(
    "Alice" -> Set("Bob", "Charlie", "David"),
    "Bob" -> Set(),
    "Charlie" -> Set("David"),
    "David" -> Set("Bob", "Mary"),
    "Mary" -> Set("Bob", "Charlie")
  )
  Alice to Mary
  loop([(Alice -> [Alice])], [])
    neibhours = ["Bob", "Charlie", "David"]
    updatedPaths = ["Bob" -> [Bob, Alice], "Charlie" -> [Charlie, Alice], "David" -> [Charlie, Alice]]
    visited = [Alice]
  loop(["Bob" -> [Bob, Alice], "Charlie" -> [Charlie, Alice], "David" -> [Charlie, Alice]], [Alice])
    neibhours = [Bob, Marry]
    updatedPaths = []
   */
  def findPath[T](graph: Graph[T], start: T, end: T): List[T] = {
    def loop(
        remaining: List[(T, List[T])],
        visited: Set[T]
    ): List[T] = {
      if (remaining.isEmpty) List()
      else {
        val (node, currentPath) = remaining.head
        if (node == end) currentPath.reverse
        else if (visited.contains(node)) loop(remaining.tail, visited)
        else {
          val neibhours = graph(node)
          val updatedPath = neibhours.map(n => n -> (n :: currentPath))
          loop(remaining.tail ++ updatedPath, visited + node)
        }
      }
    }
    loop(List(start -> List(start)), Set())
  }

  def makeUndirected[T](graph: Graph[T]): Graph[T] = {
    def addNode(graph: Graph[T], from: T, to: T): Graph[T] =
      graph.updatedWith(from) {
        case None            => Some(Set(to))
        case Some(neibhours) => Some(neibhours + to)
      }

    def loop(acc: Graph[T], nodes: Set[T]): Graph[T] = {
      if (nodes.isEmpty) acc
      else {
        val node = nodes.head
        val neibhours = acc(node)
        val newAcc =
          neibhours.foldLeft[Graph[T]](acc)((map, n) => addNode(map, n, node))
        loop(newAcc, nodes.tail)
      }
    }
    loop(graph, graph.keySet)
  }

}

@main def runGraph = {
  println(Graph.outDegree(Graph.socialNetwork, "Alice"))
  println(Graph.inDegree(Graph.socialNetwork, "David"))
  println(Graph.isPath(Graph.socialNetwork, "Alice", "Mary"))
  println(Graph.isPath(Graph.socialNetwork, "Mary", "Alice"))
  println(Graph.findPath(Graph.socialNetwork, "Alice", "Mary"))
  print(Graph.makeUndirected(Graph.socialNetwork))
}
