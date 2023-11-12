package io.navidjalali.lookingglass

object Algo:
  def topologicalSort[A](
    graph: Map[A, List[A]]
  ): List[A] =
    def dfs(
      node: A,
      visited: Set[A],
      topologicalOrdering: List[A]
    ): (Set[A], List[A]) =
      if visited.contains(node) then (visited, topologicalOrdering)
      else
        val children                             = graph.getOrElse(node, Nil)
        val (newVisited, newTopologicalOrdering) =
          children.foldLeft((visited + node, topologicalOrdering)) {
            case ((visitedSoFar, acc), nameOfChild) =>
              dfs(nameOfChild, visitedSoFar, acc)
          }
        (newVisited, node :: newTopologicalOrdering)

    graph.keySet
      .foldLeft((Set.empty[A], List.empty[A])) { case ((visited, acc), node) =>
        dfs(node, visited, acc)
      }
      ._2
      .reverse
