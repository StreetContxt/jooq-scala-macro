

import com.contxt.jooq.test.Test
import com.contxt.jooq.test.tables.Author
import com.contxt.jooq.test.tables.records.AuthorRecord
import org.jooq._
import treehugger.forest._
import definitions._
import treehugger.forest
import treehuggerDSL._

import scala.collection.JavaConverters._

//treeToString(JooqTreeHuggerTmp.fromTable(Author.AUTHOR, "helloworld"))

JooqTreeHuggerTmp.fromSchema(Test.TEST, "helloworld")
  .map(t => treeToString(t))


object JooqTreeHuggerTmp {
  val typeMappings = Map[Class[_], Class[_]](
    classOf[java.lang.Integer] -> classOf[scala.Int],
    classOf[java.lang.Long] -> classOf[scala.Long],
    classOf[java.lang.Double] -> classOf[scala.Double],
    classOf[java.lang.Character] -> classOf[scala.Char],
    classOf[java.lang.Short] -> classOf[scala.Short],
    classOf[java.lang.Byte] -> classOf[scala.Byte],
    classOf[java.lang.Boolean] -> classOf[scala.Boolean],
    classOf[java.lang.Float] -> classOf[scala.Float],
    classOf[java.math.BigDecimal] -> classOf[scala.math.BigDecimal],
    classOf[java.math.BigInteger] -> classOf[scala.math.BigInt]
  )


  def fromSchema(schema: Schema, pkg: String) = {
    schema.getTables.asScala.map { t =>
      fromTable(t, pkg + s".${schema.getName}")
    }.toList
  }

  def fromTable(table: Table[_], pkg: String) = {
    val fields = table.getClass.getDeclaredFields.filter(f => classOf[TableField[_, _]].isAssignableFrom(f.getType))

    val snippets = fields.map(f => f.get(table).asInstanceOf[TableField[_, _]]).map(fromTableField)

    val recordType = table.getRecordType.getCanonicalName

    val tableName = underscoreToCamel(table.getName).capitalize

    BLOCK(
      CASECLASSDEF(tableName)
        .withParams(
          snippets.map(_.fieldDecl) ++ snippets.map(_.fieldModFlagDecl)
        ) := BLOCK(
        DEF("record", recordType) := BLOCK(
          (VAL("rec", recordType) := NEW(recordType)) +:
            snippets.flatMap(_.recordSetter(REF("rec"))) :+
            REF("rec")
        ),
        DEF("copy", tableName).withParams(
          snippets.map(_.fieldDecl)
        ) := BLOCK(
          REF(tableName) APPLY snippets.flatMap(_.fieldCopier)
        )
      ),

      OBJECTDEF(tableName)
        .withParents(table.getClass.getCanonicalName) := BLOCK(
        DEF("fromRecord", tableName)
          .withParams(VAL("rec", recordType).tree) := BLOCK(
          REF(tableName) APPLY snippets.flatMap(_.recordGetter(REF("rec")))
        )
      )
    ) inPackage pkg

  }

  case class FieldSnippets(
                            fieldDecl: forest.ValDef,
                            fieldModFlagDecl: forest.ValDef,
                            recordSetter: forest.Ident => Array[forest.Tree],
                            recordGetter: forest.Ident => Array[forest.Tree],
                            fieldCopier: Array[forest.Tree]
                          )

  def fromTableField(tf: TableField[_, _]) = {
    val dataType: DataType[_] = tf.getDataType


    val typ =
      if (dataType.nullable || dataType.defaulted)
        s"Option[${typeMappings.getOrElse(dataType.getType, dataType.getType).getCanonicalName}]"
      else typeMappings.getOrElse(dataType.getType, dataType.getType).getCanonicalName

    val fieldName = underscoreToCamel(tf.getName)

    val modifiedField = fieldName + "Modified"

    FieldSnippets(
      VAL(fieldName, typ).tree,
      VAL(modifiedField, BooleanClass).tree,
      (rec: forest.Ident) => {
        val recordSetter =
          if (dataType.nullable || dataType.defaulted)
            rec DOT ("set" + fieldName.capitalize) APPLY (REF(fieldName) DOT "orNull")
          else rec DOT ("set" + fieldName.capitalize) APPLY REF(fieldName)

        Array(
          recordSetter,
          rec DOT "changed" APPLY(LIT(tf.getName), REF(modifiedField))
        )
      },
      (rec: forest.Ident) => {
        val recordGetter =
          if (dataType.nullable || dataType.defaulted) {
            REF(fieldName) INFIX "=" APPLY REF("Option") APPLY (rec DOT ("get" + fieldName.capitalize))
          }
          else (REF(fieldName) INFIX "=" APPLY rec DOT ("get" + fieldName.capitalize)).tree

        Array(
          recordGetter,
          REF(modifiedField) INFIX "=" APPLY rec DOT "changed" APPLY LIT(tf.getName)
        )
      },
      Array(
        THIS DOT fieldName INFIX "=" APPLY REF(fieldName),
        THIS DOT modifiedField INFIX "=" APPLY (THIS DOT modifiedField INFIX "||"
          APPLY (THIS DOT fieldName INFIX "!=" APPLY REF(fieldName)))
      )
    )
  }
}

def underscoreToCamel(name: String) = "_([a-z\\d])".r.replaceAllIn(name.toLowerCase, { m =>
  m.group(1).toUpperCase()
})


