package io.navidjalali.lookingglass.swagger

enum Segment:
  case Literal(value: String)
  case Variable(name: String)
