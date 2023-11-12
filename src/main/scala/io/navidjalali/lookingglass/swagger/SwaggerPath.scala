package io.navidjalali.lookingglass.swagger

final case class Op(
  name: String,
  methods: List[Method]
)

enum Method:
  case Get
  case Post
  case Put
  case Delete
  case Patch
  case Head
  case Options
