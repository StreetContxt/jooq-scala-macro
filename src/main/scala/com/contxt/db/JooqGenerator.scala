package com.contxt.db


import java.time.LocalDateTime

import org.jooq.{Record, TableField}

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.Context

class fromCatalog[C <: org.jooq.Catalog] extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro JooqGenerator.fromSchema_impl
}

object JooqGenerator {
  gen =>
  def fromSchema_impl(c: Context)(annottees: c.Expr[Any]*) = {
    import c.universe._

    case class FieldData(termName: TermName,
                         modifiedFlag: TermName,
                         fieldName: String,
                         javaType: Type,
                         scalaType: Type,
                         nullable: Boolean,
                         recordSetter: Tree,
                         recordGetter: Tree)

    def genTableTypes(t: Type): List[Tree] = {

      val fields: List[TermSymbol] = t.declarations
        .filterNot(_.isMethod)
        .filter(_.isTerm)
        .map(_.asTerm)
        .filter(_.typeSignature.baseClasses.exists(_.fullName == "org.jooq.TableField"))
        .toList

      val tableValue = t.typeSymbol
        .companionSymbol
        .typeSignature
        .declarations
        .filter(_.typeSignature == t)
        .head
        .name.toTermName

      val tableTrait = t.typeSymbol.typeSignature.baseClasses.filter(_.fullName == "org.jooq.Table").head.asClass
      val recordType = tableTrait.typeParams.head.asType.toType.asSeenFrom(t, tableTrait)
      val fieldData = genFieldList(fields, t, recordType)
      val typeName: TypeName = newTypeName(t.typeSymbol.name.toString)

      val fieldDefs: List[Tree] = fieldData.map(f => q"val ${f.termName}: ${f.scalaType}") :::
        fieldData.map(f => q"val ${f.modifiedFlag}: Boolean = true")


      List(
        q"""
            object ${newTermName(typeName.toString)} extends $t {
              def apply(rec: $recordType): $typeName =
                new $typeName(..${fieldData.map(f => q"${f.termName} = ${f.recordGetter}(rec)") :::
                  fieldData.map(f => q"${f.modifiedFlag} = rec.changed(${f.fieldName})")
                })
              def apply(..${fieldData.map(f => q"${f.termName}: ${f.scalaType}")}) =
                new $typeName(..${fieldData.map(f => q"${f.termName} = ${f.termName}")})
            }
          """,
        q"""implicit def ${newTermName(s"${typeName}Record2RecordLike")} (rec: $recordType): $typeName = ${newTermName(typeName.toString)}(rec)""",
        q"""implicit def ${newTermName(s"${typeName}ResultToListOfRecordLike")} (res: org.jooq.Result[$recordType]): List[$typeName] =
                res.asScala.toList.map(${newTermName(s"${typeName}Record2RecordLike")}(_))""",
        q"""
            class $typeName private(..$fieldDefs)
              extends JooqRecordLike[$recordType] with Equals {

              def record: $recordType = {
                val rec = new $recordType();
                ..${fieldData.map(f => q"${f.recordSetter}(rec, ${f.termName})")}
                ..${fieldData.map(f => q"rec.changed(${f.fieldName}, ${f.modifiedFlag})")}
                rec
              }

              def copy(..${fieldData.map(f => q"val ${f.termName}: ${f.scalaType} = ${f.termName}")}): $typeName =
                new $typeName(
                  ..${
          fieldData.map(f => q"${f.termName} = ${f.termName}") :::
            fieldData.map(f =>
              q"""
                         ${f.modifiedFlag} =
                         this.${f.modifiedFlag} || this.${f.termName} != ${f.termName}
                      """)
        }
                )

              override def canEqual(other: Any): Boolean = other.isInstanceOf[$typeName]

              override def equals(other: Any): Boolean = other match {
                case o: $typeName if canEqual(o) =>
                  ${(fieldData.map(f => q"${f.termName} == o.${f.termName}") :::
                     fieldData.map(f => q"${f.modifiedFlag} == o.${f.modifiedFlag}"))
                     .reduceOption((t1, t2) => q"$t1 && $t2").getOrElse(q"false")}
                case _ => false
              }

              override def hashCode(): Int = {
                 ${(fieldData.map(f => q"${f.termName}.hashCode") :::
                   fieldData.map(f => q"${f.modifiedFlag}.hashCode"))
                     .reduceOption((t1: Tree, t2: Tree) => q"$t1 + $t2").getOrElse(q"0")}
              }

              override def toString(): String = {
                   val sep = ", "
                   ${typeName.toString} + "(" +
                   ${(fieldData.map(f => q"${f.termName}.toString") :::
                        fieldData.map(f => q"${f.modifiedFlag}.toString"))
                    .reduceOption((t1: Tree, t2: Tree) => q"$t1 + sep + $t2")
                    .getOrElse(q"")} + ")"
              }
            }
          """
      )
    }

    def genFieldList(fields: List[TermSymbol], container: Type, recordType: Type): List[FieldData] = {
      val tableClass: Class[_] = Class.forName(container.typeSymbol.fullName.toString)
      val table = tableClass.getDeclaredFields
        .filter(_.getType == tableClass).head.get(null)
        .asInstanceOf[org.jooq.Table[org.jooq.Record]]

      val fieldInfo: List[(TermSymbol, TableField[Record, _])] = fields
        .map { cf =>
          (cf, tableClass.getDeclaredField(cf.name.toString).get(table)
            .asInstanceOf[TableField[Record, _]])
        }

      fieldInfo.map { case (cf, rf) =>
        val fieldType: Type = cf.typeSignature.typeSymbol.asClass
          .typeParams(1)
          .asType
          .toType
          .asSeenFrom(cf.typeSignature, cf.typeSignature.typeSymbol)
        val scalaType = determineScalaType(rf, fieldType)
        val camel: String = underscoreToCamel(cf.name.toTermName.toString.toLowerCase)

        FieldData(
          termName = newTermName(camel),
          modifiedFlag = newTermName(camel + "Modified"),
          fieldName = rf.getName,
          javaType = fieldType,
          scalaType = scalaType,
          nullable = rf.getDataType.nullable,
          recordSetter = genSetter(rf, recordType, scalaType, fieldType, camel),
          recordGetter = genGetter(rf, recordType, scalaType, fieldType, camel)
        )
      }
    }

    def genSetter(rf: TableField[Record, _], recordType: Type, scalaType: Type, fieldType: Type, camel: String): Tree = {
      val recordSetter: Symbol = recordType.declaration(newTermName(s"set${camel.capitalize}"))
      val convert =
        if (fieldType == typeOf[java.sql.Timestamp]) q"java.sql.Timestamp.valueOf(value)"
        else if (fieldType == typeOf[java.math.BigDecimal]) q"value.bigDecimal"
        else if (fieldType == typeOf[java.lang.Byte]) q"byte2Byte(value)"
        else if (fieldType == typeOf[java.lang.Short]) q"short2Short(value)"
        else if (fieldType == typeOf[java.lang.Integer]) q"int2Integer(value)"
        else if (fieldType == typeOf[java.lang.Long]) q"long2Long(value)"
        else if (fieldType == typeOf[java.lang.Float]) q"float2Float(value)"
        else if (fieldType == typeOf[java.lang.Double]) q"double2Double(value)"
        else if (fieldType == typeOf[java.lang.Character]) q"char2Char(value)"
        else if (fieldType == typeOf[java.lang.Boolean]) q"boolean2Boolean(value)"
        else q"value"

      if (rf.getDataType.nullable) q"{(rec: $recordType, opt: $scalaType) => rec.$recordSetter(opt.map(value => $convert).getOrElse(null))}"
      else q"{(rec: $recordType, value: $scalaType) => rec.$recordSetter($convert)}"
    }

    def genGetter(rf: TableField[Record, _], recordType: Type, scalaType: Type, fieldType: Type, camel: String): Tree = {
      val recordGetter: Symbol = recordType.declaration(newTermName(s"get${camel.capitalize}"))
      val (convert: Tree, get: Tree) =
        if (fieldType == typeOf[java.sql.Timestamp]) (q"value.toLocalDateTime()", q"rec.$recordGetter")
        else if (fieldType == typeOf[java.math.BigDecimal]) (q"scala.math.BigDecimal(value)", q"rec.$recordGetter")
        else if (fieldType == typeOf[java.lang.Byte]) (q"Byte2byte(value)", q"rec.$recordGetter")
        else if (fieldType == typeOf[java.lang.Short]) (q"Short2short(value)", q"rec.$recordGetter")
        else if (fieldType == typeOf[java.lang.Integer]) (q"Integer2int(value)", q"rec.$recordGetter")
        else if (fieldType == typeOf[java.lang.Long]) (q"Long2long(value)", q"rec.$recordGetter")
        else if (fieldType == typeOf[java.lang.Float]) (q"Float2float(value)", q"rec.$recordGetter")
        else if (fieldType == typeOf[java.lang.Double]) (q"Double2double(value)", q"rec.$recordGetter")
        else if (fieldType == typeOf[java.lang.Character]) (q"Char2char(value)", q"rec.$recordGetter")
        else if (fieldType == typeOf[java.lang.Boolean]) (q"Boolean2boolean(value)", q"rec.$recordGetter")
        else (q"value", q"rec.$recordGetter")

      if (rf.getDataType.nullable) q"{rec: $recordType => Option($get).map(value => $convert)}"
      else q"{rec: $recordType => val value = $get; $convert}"
    }

    def determineScalaType(rf: TableField[Record, _], fieldType: Type): Type = {
      val baseType =
        if (fieldType == typeOf[java.sql.Timestamp]) typeOf[LocalDateTime]
        else if (fieldType == typeOf[java.math.BigDecimal]) typeOf[BigDecimal]
        else if (fieldType == typeOf[java.lang.Byte]) typeOf[Byte]
        else if (fieldType == typeOf[java.lang.Short]) typeOf[Short]
        else if (fieldType == typeOf[java.lang.Integer]) typeOf[Int]
        else if (fieldType == typeOf[java.lang.Long]) typeOf[Long]
        else if (fieldType == typeOf[java.lang.Float]) typeOf[Float]
        else if (fieldType == typeOf[java.lang.Double]) typeOf[Double]
        else if (fieldType == typeOf[java.lang.Character]) typeOf[Char]
        else if (fieldType == typeOf[java.lang.Boolean]) typeOf[Boolean]
        else fieldType

      if (rf.getDataType.nullable) appliedType(weakTypeOf[Option[_]], List(baseType)) else baseType
    }

    def genSchemaObject(schema: Type): Tree = {
      val tables = schema.declarations
        .filterNot(_.isMethod)
        .filter(_.isTerm)
        .map(_.asTerm)
        .map(_.typeSignature)
        .filter(_.baseClasses.exists(_.fullName == "org.jooq.Table"))
        //        .filter(_.typeSymbol.name.toString == "DateByDay")
        .toList

      q"""
        object ${newTermName(schema.typeSymbol.name.toString)} {
          ..${tables.flatMap(genTableTypes(_))}
        }
      """
    }

    def bail(message: String) = c.abort(c.enclosingPosition, message)

    val macroTypeWithArguments = c.typeCheck(q"${c.prefix.tree}").tpe
    val annotationClass: ClassSymbol = macroTypeWithArguments.typeSymbol.asClass
    val annotationTypePlaceholder: Type = annotationClass.typeParams.head.asType.toType
    val catalog: Type = annotationTypePlaceholder.asSeenFrom(macroTypeWithArguments, annotationClass)

    val schemas = catalog.declarations
      .filterNot(_.isMethod)
      .filter(_.isTerm)
      .map(_.asTerm)
      .map(_.typeSignature)
      .filter(_.baseClasses.exists(_.fullName == "org.jooq.Schema"))
      .toList

    annottees.map(_.tree) match {
      case List(q"object $name { ..$body }") if body.isEmpty =>
        c.Expr[Any](
          q"""
            object $name {
              import scala.language.implicitConversions
              import scala.collection.JavaConverters._

              trait JooqRecordLike[R <: org.jooq.Record] {
                def record: R
              }

              implicit def RecordLikeToRecord[R <: org.jooq.Record] (rl: JooqRecordLike[R]): R = {
                rl.record
              }

              ..${schemas.map(genSchemaObject(_))}
            }
          """
        )
      case _ => bail(
        "You must annotate an object definition with an empty body."
      )
    }
  }

  def underscoreToCamel(name: String) = "_([a-z\\d])".r.replaceAllIn(name, { m =>
    (if (m.group(1).matches("^[0-9]$")) "_" else "") + m.group(1).toUpperCase()
  }).replaceAll("_$", "")
}

