package io.navidjalali.lookingglass.swagger

import munit.*

final class PathTest extends FunSuite:
  test("correctly forms a path from a string"):
    val raw      = "/api/v1/cities/{cityName}/streets/{streetName}/buildings/{buildingId}"
    val expected = Path(
      Vector(
        Segment.Literal("api"),
        Segment.Literal("v1"),
        Segment.Literal("cities"),
        Segment.Variable("cityName"),
        Segment.Literal("streets"),
        Segment.Variable("streetName"),
        Segment.Literal("buildings"),
        Segment.Variable("buildingId")
      )
    )

    assertEquals(Path.fromRaw(raw), expected)

  test("can materialize a path with all the variables present"):
    val raw  = "/api/v1/cities/{cityName}/streets/{streetName}/buildings/{buildingId}"
    val path = Path.fromRaw(raw)

    val valueMap = Map(
      "cityName"   -> "London",
      "streetName" -> "Baker Street",
      "buildingId" -> "221B"
    )

    val expected = Right(
      Path(
        Vector(
          Segment.Literal("api"),
          Segment.Literal("v1"),
          Segment.Literal("cities"),
          Segment.Literal("London"),
          Segment.Literal("streets"),
          Segment.Literal("Baker Street"),
          Segment.Literal("buildings"),
          Segment.Literal("221B")
        )
      )
    )

    assertEquals(path.materialize(valueMap), expected)

  test("reports missing segments during materialization"):
    val raw  = "/api/v1/cities/{cityName}/streets/{streetName}/buildings/{buildingId}"
    val path = Path.fromRaw(raw)

    val valueMap = Map(
      "streetName" -> "Baker Street"
    )

    val expected = Left(Set("cityName", "buildingId"))

    assertEquals(path.materialize(valueMap), expected)
