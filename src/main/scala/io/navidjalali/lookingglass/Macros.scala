package io.navidjalali.lookingglass

import io.navidjalali.lookingglass.swagger.SchemaType
import io.swagger.parser.OpenAPIParser

import scala.jdk.CollectionConverters.*
import scala.quoted.*

object Macros:
  final case class Service(
    methods: Map[(String), Seq[Any] => Any]
  ) extends Selectable:
    def applyDynamic(name: String)(args: Any*): Any = methods(name)(args)

  final case class Rec(values: Map[String, Any]) extends Selectable:
    def selectDynamic(name: String): Any = values(name)

  object Rec:
    transparent inline def fromCaseClass[T](p: T): Any =
      ${ caseClassImpl[T]('p) }

  def caseClassImpl[T: Type](t: Expr[T])(using Quotes): Expr[Any] =
    import quotes.reflect.*
    val tpe =
      TypeRepr
        .of[T]
        .typeSymbol
        .caseFields
        .map(_.tree.asInstanceOf[ValDef])
        .map(valDef => (valDef.name, valDef.tpt.tpe))
        .foldLeft((TypeRepr.of[Rec])) { case (acc, (name, tpe)) =>
          Refinement(acc, name, tpe)
        }

    tpe.asType match
      case '[t] =>
        '{
          val product = $t.asInstanceOf[Product]
          Rec(product.productElementNames.zip(product.productIterator).toMap)
            .asInstanceOf[t]
        }

  inline def inspectBody[T](inline body: T): String =
    ${ inspectBodyImpl('body) }

  def inspectBodyImpl[T](body: Expr[T])(using Quotes): Expr[String] =
    import quotes.reflect.*
    report.info(body.asTerm.toString())
    Expr(body.show)

  inline def readSwagger[T](inline filename: String) =
    val openAPI =
      new OpenAPIParser()
        .readLocation(
          filename,
          null,
          null
        )
        .getOpenAPI()

    println(
      openAPI
        .getPaths()
        .asScala
        .map { case (name, path) =>
          val methods = path
            .readOperationsMap()
            .asScala
            .map { case (method, operation) =>
              (
                method,
                Option(
                  path
                    .getParameters()
                    .asScala
                ).toList
                  .flatMap(_.toList)
                  .flatMap(parameters => Option(parameters.getName())),
                Option(
                  operation
                    .getParameters()
                    .asScala
                ).toList
                  .flatMap(_.toList)
                  .flatMap(parameters => Option(parameters.getName()))
              )
            }
            .toList
          (name, methods)
        }
        .mkString("\n")
    )

    val schemas = openAPI.getComponents.getSchemas.asScala.map { case (name, property) =>
      val schemaType = SchemaType.fromSwaggerSchema(property)
      (name, schemaType)
    }.toMap

    schemas

  transparent inline def makeSchema(filename: String): Any = ${
    makeSchemaImpl('filename)
  }

  def refineType(quotes: Quotes)(
    initial: quotes.reflect.TypeRepr,
    refinements: List[(String, SchemaType)]
  ): quotes.reflect.TypeRepr =
    given Quotes = quotes
    import quotes.reflect.*
    refinements.foldLeft(initial) { case (acc, (name, tpe)) =>
      val repr = tpe match
        case _ => TypeRepr.of[Int]

        // quotes.reflect.TypeRepr.typeConstructorOf(Class.forName(typeName))

      Refinement(acc, name, repr)
    }

  def makeSchemaImpl(filename: Expr[String])(using Quotes): Expr[Any] =
    import quotes.reflect.*
    // File name should be known at compile time
    val file = filename.value match
      case None        =>
        report.errorAndAbort("File name should be known at compile time")
      case Some(value) => value

    val swagger = readSwagger(file)

    val types = swagger.toList

    // report.info(swagger.mkString("\n"))

    val recordType = refineType(summon)(TypeRepr.of[Rec], types)

    recordType.asType match
      case '[t] =>
        '{
          // Service(
          //   Map(
          //     "getPerson" -> (_ => Rec($map).asInstanceOf[t])
          //   )
          // ).asInstanceOf[Service { def getPerson(): t }]
          Rec(Map.empty).asInstanceOf[t]
        }
