package io.navidjalali.lookingglass.swagger

final case class Path(segments: Vector[Segment]):
  def materialize(valueMap: Map[String, String]): Either[Set[String], Path] =
    val (missing, literals) = segments.foldLeft(Set.empty[String], Vector.empty[Segment.Literal]) {
      case ((missing, literals), segment) =>
        segment match
          case lit: Segment.Literal   =>
            (missing, literals :+ lit)
          case Segment.Variable(name) =>
            valueMap.get(name) match
              case Some(value) => (missing, literals :+ Segment.Literal(value))
              case None        => (missing + name, literals)
    }

    Either.cond(missing.isEmpty, Path(literals), missing)

object Path:
  def fromRaw(raw: String): Path =
    Path(
      raw.split("/").toVector.filter(_.nonEmpty).map { segment =>
        if segment.startsWith("{") && segment.endsWith("}") then
          Segment.Variable(segment.stripPrefix("{").stripSuffix("}"))
        else Segment.Literal(segment)
      }
    )
