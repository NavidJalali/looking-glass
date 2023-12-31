package io.navidjalali.lookingglass.swagger

import io.swagger.v3.oas.models.media.Schema

import scala.jdk.CollectionConverters.*

enum SchemaType:
  self =>
  case Str
  case Int
  case Long
  case Double
  case Bool
  case DateTime
  case Date
  case Array(itemType: SchemaType)
  case Ref(name: String)
  case CoProduct(values: List[String])
  case Product(fields: List[(String, SchemaType)])
  case Object
  case Unsupported(name: String)
  case Optional(itemType: SchemaType)

  def getInnerRefs: List[String] =
    self match
      case Ref(name)          => List(name.stripPrefix("#/components/schemas/"))
      case Array(itemType)    => itemType.getInnerRefs
      case Product(fields)    =>
        fields.flatMap { case (_, fieldType) =>
          fieldType.getInnerRefs
        }
      case Optional(itemType) => itemType.getInnerRefs
      case _                  => Nil

object SchemaType:
  def fromSwaggerSchema(s: Schema[_]): SchemaType =
    Option(s.getType) match
      case Some("string")  =>
        Option(s.getEnum) match
          case Some(values) =>
            CoProduct(values.asScala.toList.map(_.toString))
          case None         =>
            Option(s.getFormat) match
              case Some("date-time") => DateTime
              case Some("date")      => Date
              case _                 => Str
      case Some("integer") =>
        Option(s.getFormat) match
          case Some("int64") => Long
          case _             => Int
      case Some("number")  =>
        Option(s.getFormat) match
          case Some("double")  => Double
          case Some("integer") => Int
          case _               => Double
      case Some("boolean") => Bool
      case Some("array")   =>
        fromSwaggerSchema(s.getItems) match
          case Unsupported(name) => Unsupported(s"array of $name")
          case itemType          => Array(itemType)
      case Some("object")  =>
        Option(s.getProperties)
          .map(_.asScala.toList)
          .fold(Object) { properties =>
            val required =
              Option(s.getRequired).toSet.flatMap(_.asScala.toList)
            Product(properties.map { case (name, property) =>
              (
                name,
                if required.contains(name)
                then fromSwaggerSchema(property)
                else Optional(fromSwaggerSchema(property))
              )
            })
          }
      case unsupported     =>
        Option(s.get$ref)
          .fold(Unsupported(unsupported.getOrElse("unknown")))(Ref.apply)
