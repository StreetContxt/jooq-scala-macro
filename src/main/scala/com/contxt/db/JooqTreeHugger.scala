package com.contxt.db

import org.jooq._
import treehugger.forest._
import definitions._
import treehugger.forest
import treehuggerDSL._

import scala.collection.JavaConverters._

object JooqTreeHugger {
  private val typeMappings = Map[Class[_], (String, String)](
    classOf[java.lang.Integer] -> ("scala.Int", "int2Integer"),
    classOf[java.lang.Long] -> ("scala.Long", "long2Long"),
    classOf[java.lang.Double] -> ("scala.Double", "double2Double"),
    classOf[java.lang.Character] -> ("scala.Char", "char2Char"),
    classOf[java.lang.Short] -> ("scala.Short", "short2Short"),
    classOf[java.lang.Byte] -> ("scala.Byte", "byte2Byte"),
    classOf[java.lang.Boolean] -> ("scala.Boolean", "boolean2Boolean"),
    classOf[java.lang.Float] -> ("scala.Float", "float2Float"),
    classOf[java.math.BigDecimal] -> ("scala.math.BigDecimal", "javaBigDecimal2bigDecimal"),
    classOf[java.math.BigInteger] -> ("scala.math.BigInt", "javaBigInteger2bigInt")
  )

  def fromCatalog(catalog: Catalog, pkg: String): List[forest.PackageDef] = {
    catalog.getSchemas.asScala.toList.flatMap { s =>
      val str: String =
        if (catalog.getName.isEmpty) pkg
        else pkg + s".${catalog.getName.toLowerCase}"
      fromSchema(s, str)
    }
  }

  def fromSchema(schema: Schema, pkg: String): List[forest.PackageDef] = {
    schema.getTables.asScala.toList.map { t =>
      fromTable(t, pkg + s".${schema.getName.toLowerCase}")
    }
  }

  def fromTable(table: Table[_], pkg: String): forest.PackageDef = {
    val fields = table.getClass.getDeclaredFields.filter(f => classOf[TableField[_, _]].isAssignableFrom(f.getType))

    val snippets = fields.map(f => f.get(table).asInstanceOf[TableField[_, _]]).map(fromTableField)

    val recordType = table.getRecordType.getCanonicalName

    val tableName = underscoreToCamel(table.getName).capitalize

    BLOCK(
      CASECLASSDEF(tableName)
        .withParams(
          snippets.map(_.fieldDecl)
        ) := BLOCK(
        snippets.map(_.fieldModFlagDecl) ++
          Array(
            DEFTHIS
              .withParams(snippets.map(_.fieldDecl) ++ snippets.map(_.fieldModFlagDecl)) := BLOCK(
              (THIS APPLY snippets.map(_.fieldAssignment)) +:
                snippets.map(_.modFlagAssignment)

            ),
            DEF("record", recordType) := BLOCK(
              (VAL("rec", recordType) := NEW(recordType)) +:
                snippets.flatMap(_.recordSetter(REF("rec"))) :+
                REF("rec")
            ),
            DEF("copy", tableName).withParams(
              snippets.map(_.fieldDecl)
            ) := BLOCK(
              NEW(tableName) APPLY snippets.flatMap(_.fieldCopier)
            )
          )
      ),

      OBJECTDEF(tableName)
        .withParents(table.getClass.getCanonicalName) := BLOCK(
        DEF("fromRecord", tableName)
          .withParams(VAL("rec", recordType).tree) := BLOCK(
          NEW(tableName) APPLY snippets.flatMap(_.recordGetter(REF("rec")))
        )
      )
    ) inPackage pkg

  }

  case class FieldSnippets(fieldDecl: forest.ValDef,
                           fieldModFlagDecl: forest.ValDef,
                           recordSetter: forest.Ident => Array[forest.Tree],
                           recordGetter: forest.Ident => Array[forest.Tree],
                           fieldCopier: Array[forest.Tree],
                           fieldAssignment: forest.Tree,
                           modFlagAssignment: forest.Tree)

  def fromTableField(tf: TableField[_, _]): FieldSnippets = {
    val dataType: DataType[_] = tf.getDataType

    val typeData = typeMappings.getOrElse(dataType.getType, (dataType.getType.getCanonicalName, "identity"))
    val typ: String =
      if (dataType.nullable || dataType.defaulted)
        s"Option[${typeData._1}]"
      else typeData._1

    val fieldName = underscoreToCamel(tf.getName)

    val modifiedField = fieldName + "Modified"

    FieldSnippets(
      fieldDecl = VAL(fieldName, typ).tree,
      fieldModFlagDecl = VAR(modifiedField, BooleanClass) := LIT(false),
      recordSetter = (rec: forest.Ident) => {
        val recordSetter =
          if (dataType.nullable || dataType.defaulted)
            rec DOT ("set" + fieldName.capitalize) APPLY (
              REF(fieldName) DOT "map" APPLY REF(typeData._2) DOT "orNull")
          else rec DOT ("set" + fieldName.capitalize) APPLY REF(fieldName)

        Array(
          recordSetter,
          rec DOT "changed" APPLY(LIT(tf.getName), REF(modifiedField))
        )
      },
      recordGetter = (rec: forest.Ident) => {
        val recordGetter =
          if (dataType.nullable || dataType.defaulted) {
            REF(fieldName) := REF("Option") APPLY (rec DOT ("get" + fieldName.capitalize))
          }
          else REF(fieldName) := rec DOT ("get" + fieldName.capitalize)

        Array(
          recordGetter,
          REF(modifiedField) := rec DOT "changed" APPLY LIT(tf.getName)
        )
      },
      fieldCopier = Array(
        REF(fieldName) := THIS DOT fieldName,
        REF(modifiedField) := (THIS DOT modifiedField INFIX "||"
          APPLY (THIS DOT fieldName INFIX "!=" APPLY REF(fieldName)))
      ),
      fieldAssignment = REF(fieldName) := REF(fieldName),
      modFlagAssignment = THIS DOT modifiedField := REF(modifiedField)
    )
  }

  private def underscoreToCamel(name: String) =
    "_([a-z\\d])".r.replaceAllIn(name.toLowerCase, { m => m.group(1).toUpperCase() })
}
