package io.navidjalali.lookingglass

import munit.*

final class AlgoTest extends FunSuite:
  test("topological sort sorts topologically"):
    val graph = Map(
      "a" -> List("b", "c"),
      "b" -> List("d", "e"),
      "c" -> List("e", "f"),
      "g" -> List("f", "h")
    )

    val obtained = Algo.topologicalSort(graph)

    val expected = List("d", "e", "b", "f", "c", "a", "h", "g")

    assertEquals(obtained, expected)
