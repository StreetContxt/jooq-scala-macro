package com.streetcontxt.db

import org.jooq.{Record, TableField, TableRecord}

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

class fromCatalog[C <: org.jooq.Catalog] extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro JooqGenerator.fromCatalog_impl
}

class fromSchema[C <: org.jooq.Catalog] extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any  = macro JooqGenerator.fromSchema_impl
}

class JooqGenerator(val c: whitebox.Context) {
  gen =>

  import c.universe._

  case class FieldData(termName: TermName,
                       modifiedFlag: TermName,
                       modifiedFlagAccessor: TermName,
                       fieldName: String,
                       javaType: Type,
                       scalaType: Type,
                       nullable: Boolean,
                       recordSetter: Tree,
                       recordGetter: Tree,
                       rawGetter: Tree,
                       index: Int
                      )


  def genTableTypes(t: Type): List[Tree] = {
    val fields: List[TermSymbol] = t.decls
      .filterNot(_.isMethod)
      .filter(_.isTerm)
      .map(_.asTerm)
      .filter(_.typeSignature.baseClasses.exists(_.fullName == "org.jooq.TableField"))
      .toList

    val tableTrait = t.typeSymbol.typeSignature.baseClasses.filter(_.fullName == "org.jooq.Table").head.asClass
    val recordType = tableTrait.typeParams.head.asType.toType.asSeenFrom(t, tableTrait)
    val fieldData = genFieldList(fields, t, recordType)
    val typeName: TypeName = TypeName(t.typeSymbol.name.toString)

    val fieldDefs: List[Tree] = fieldData.map(f => q"val ${f.termName}: ${f.scalaType}")
    val modFlags = fieldData.map(f => q"""private var ${f.modifiedFlag}: Boolean = true""")
    val modAccessors = fieldData.map(f => q"""def ${f.modifiedFlagAccessor}: Boolean = ${f.modifiedFlag}""")



    val caseClass: Tree = q"""
            case class $typeName (..$fieldDefs)
              extends JooqRecordLike[$recordType] {

              ..$modFlags
              ..$modAccessors

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
        fieldData.map(f => q"""${f.modifiedFlag} = this.${f.modifiedFlag}|| this.${f.termName} != ${f.termName}""")
    }
                )
            }
          """ match {
      case q"case class $typeName (..$fieldDefs) extends JooqRecordLike[$recordType] { ..$body }" =>
        val modCtor =
          q"""
          private def this(..${fieldDefs ::: fieldData.map(f => q"""private var ${f.modifiedFlag}: Boolean""")}) = {
                this(..${fieldData.map(f => q"${f.termName} = ${f.termName}")})
                ..${fieldData.map(f => q"this.${f.modifiedFlag} = ${f.modifiedFlag}")}
          }"""
        val recCtor =
          q"""
          def this(rec: $recordType) =
             this(..${
            fieldData.map(f => q"${f.termName} = ${f.recordGetter}(rec)") :::
              fieldData.map(f => q"${f.modifiedFlag} = rec.changed(${f.fieldName})")
          })
          """
        val defaultCtorPos = c.enclosingPosition
        val modCtorPos = defaultCtorPos
          .withEnd(defaultCtorPos.end + 1)
          .withStart(defaultCtorPos.start + 1)
          .withPoint(defaultCtorPos.point + 1)
        val recCtorPos = modCtorPos
          .withEnd(modCtorPos.end + 1)
          .withStart(modCtorPos.start + 1)
          .withPoint(modCtorPos.point + 1)
        val newBody = body :+ atPos(modCtorPos)(modCtor) :+ atPos(recCtorPos)(recCtor)
        q"case class $typeName (..$fieldDefs) extends JooqRecordLike[$recordType] { ..$newBody }"
    }


    List(
      q"""object ${TermName(typeName.toString)} extends $t {
              def apply(rec: $recordType): $typeName = new $typeName(rec)
        }""",
      q"""implicit def ${TermName(s"${typeName}Record2RecordLike")} (rec: $recordType): $typeName = ${TermName(typeName.toString)}(rec)""",
      q"""implicit def ${TermName(s"${typeName}ResultToListOfRecordLike")} (res: org.jooq.Result[$recordType]): List[$typeName] =
                res.asScala.toList.map(${TermName(s"${typeName}Record2RecordLike")}(_))""",
      caseClass
    )
  }

  def genFieldList(fields: List[TermSymbol], container: Type, recordType: Type): List[FieldData] = {
    val tableClass: Class[_] = Class.forName(container.typeSymbol.fullName.toString)
    val table = tableClass.getDeclaredFields
      .filter(_.getType == tableClass).head.get(null)
      .asInstanceOf[org.jooq.Table[org.jooq.Record]]

    val recordFields: Map[String, Int] = Class.forName(recordType.typeSymbol.fullName.toString)
      .getConstructor().newInstance().asInstanceOf[TableRecord[_]].fields()
      .zipWithIndex.map(t => (t._1.getName, t._2)).toMap

    val fieldInfo: List[(Int, TermSymbol, TableField[Record, _])] = fields
      .map { cf =>
        (cf, tableClass.getDeclaredField(cf.name.toString).get(table)
          .asInstanceOf[TableField[Record, _]])
      }
      .map { case (cf, rf) => (recordFields(rf.getName), cf, rf) }
      .sortBy(_._1)


    fieldInfo.map { case (idx, cf, rf) =>
      val fieldType: Type = cf.typeSignature.typeSymbol.asClass
        .typeParams(1)
        .asType
        .toType
        .asSeenFrom(cf.typeSignature, cf.typeSignature.typeSymbol)
      val scalaType = determineScalaType(rf, fieldType)
      val camel: String = underscoreToCamel(cf.name.toTermName.toString.toLowerCase)
      FieldData(
        termName = TermName(camel),
        modifiedFlag = TermName(s"${camel}Modified_"),
        modifiedFlagAccessor = TermName(s"${camel}Modified"),
        fieldName = rf.getName,
        javaType = fieldType,
        scalaType = scalaType,
        nullable = rf.getDataType.nullable,
        recordSetter = genSetter(rf, recordType, scalaType, fieldType, camel),
        recordGetter = genGetter(rf, recordType, scalaType, fieldType, camel),
        rawGetter = genRawGetter(rf, scalaType, fieldType, camel),
        idx
      )
    }.sortBy(f => f.index)
  }

  def genSetter(rf: TableField[Record, _], recordType: Type, scalaType: Type, fieldType: Type, camel: String): Tree = {
    val recordSetter: Symbol = recordType.decl(TermName(s"set${camel.capitalize}"))
    val convert =
      if (fieldType == typeOf[java.math.BigDecimal]) q"value.bigDecimal"
      else if (fieldType == typeOf[java.lang.Byte]) q"byte2Byte(value)"
      else if (fieldType == typeOf[java.lang.Short]) q"short2Short(value)"
      else if (fieldType == typeOf[java.lang.Integer]) q"int2Integer(value)"
      else if (fieldType == typeOf[java.lang.Long]) q"long2Long(value)"
      else if (fieldType == typeOf[java.lang.Float]) q"float2Float(value)"
      else if (fieldType == typeOf[java.lang.Double]) q"double2Double(value)"
      else if (fieldType == typeOf[java.lang.Character]) q"char2Char(value)"
      else if (fieldType == typeOf[java.lang.Boolean]) q"boolean2Boolean(value)"
      else q"value"

    if (scalaType.typeSymbol == typeOf[Array[String]].typeSymbol)
      q"{(rec: $recordType, value: $scalaType) => rec.$recordSetter(value : _*)}"
    else if (rf.getDataType.nullable && scalaType.typeArgs.head.typeSymbol == typeOf[Array[String]].typeSymbol)
      q"{(rec: $recordType, opt: $scalaType) => rec.$recordSetter(opt.map(value => $convert).getOrElse(null) : _*)}"
    else if (rf.getDataType.nullable)
      q"{(rec: $recordType, opt: $scalaType) => rec.$recordSetter(opt.map(value => $convert).getOrElse(null))}"
    else q"{(rec: $recordType, value: $scalaType) => rec.$recordSetter($convert)}"
  }

  def genGetter(rf: TableField[Record, _], recordType: Type, scalaType: Type, fieldType: Type, camel: String): Tree = {
    val recordGetter: Symbol = recordType.decl(TermName(s"get${camel.capitalize}"))
    val (convert: Tree, get: Tree) =
      if (fieldType == typeOf[java.math.BigDecimal]) (q"scala.math.BigDecimal(value)", q"rec.$recordGetter")
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


  def genRawGetter(rf: TableField[Record, _], scalaType: Type, fieldType: Type, camel: String): Tree = {
    val convert: Tree =
      if (fieldType == typeOf[java.math.BigDecimal]) q"scala.math.BigDecimal(value)"
      else if (fieldType == typeOf[java.lang.Byte]) q"Byte2byte(value)"
      else if (fieldType == typeOf[java.lang.String]) q"Short2short(value)"
      else if (fieldType == typeOf[java.lang.Short]) q"Short2short(value)"
      else if (fieldType == typeOf[java.lang.Integer]) q"Integer2int(value)"
      else if (fieldType == typeOf[java.lang.Long]) q"Long2long(value)"
      else if (fieldType == typeOf[java.lang.Float]) q"Float2float(value)"
      else if (fieldType == typeOf[java.lang.Double]) q"Double2double(value)"
      else if (fieldType == typeOf[java.lang.Character]) q"Char2char(value)"
      else if (fieldType == typeOf[java.lang.Boolean]) q"Boolean2boolean(value)"
      else q"value"

    if (rf.getDataType.nullable) q"{v: $fieldType => Option(v).map(value => $convert)}"
    else q"{value: $fieldType => $convert}"
  }

  def determineScalaType(rf: TableField[Record, _], fieldType: Type): Type = {
    val baseType =
      if (fieldType == typeOf[java.math.BigDecimal]) typeOf[BigDecimal]
      else if (fieldType == typeOf[java.lang.Byte]) typeOf[Byte]
      else if (fieldType == typeOf[java.lang.Short]) typeOf[Short]
      else if (fieldType == typeOf[java.lang.Integer]) typeOf[Int]
      else if (fieldType == typeOf[java.lang.Long]) typeOf[Long]
      else if (fieldType == typeOf[java.lang.Float]) typeOf[Float]
      else if (fieldType == typeOf[java.lang.Double]) typeOf[Double]
      else if (fieldType == typeOf[java.lang.Character]) typeOf[Char]
      else if (fieldType == typeOf[java.lang.Boolean]) typeOf[Boolean]
      else fieldType

    if (rf.getDataType.nullable) {
      val optionTpe = symbolOf[Option[_]].asClass.toTypeConstructor
      appliedType(optionTpe, baseType)
    } else baseType
  }

  def genSchemaObject(schema: Type): Tree = {
    val tables = schema.decls
      .filterNot(_.isMethod)
      .filter(_.isTerm)
      .map(_.asTerm)
      .map(_.typeSignature)
      .filter(_.baseClasses.exists(_.fullName == "org.jooq.Table"))
      .toList

    q"""
        object ${TermName(schema.typeSymbol.name.toString)} {
          ..${tables.flatMap(genTableTypes)}
        }
      """
  }

  private def bail(message: String) = c.abort(c.enclosingPosition, message)

  def fromSchema_impl(annottees: c.Expr[Any]*): c.Expr[Any] = {
    val macroTypeWithArguments = c.typecheck(q"${c.prefix.tree}").tpe
    val annotationClass: ClassSymbol = macroTypeWithArguments.typeSymbol.asClass
    val annotationTypePlaceholder: Type = annotationClass.typeParams.head.asType.toType
    val schema: Type = annotationTypePlaceholder.asSeenFrom(macroTypeWithArguments, annotationClass)

    val tables = schema.decls
      .filterNot(_.isMethod)
      .filter(_.isTerm)
      .map(_.asTerm)
      .map(_.typeSignature)
      .filter(_.baseClasses.exists(_.fullName == "org.jooq.Table"))
      .toList

    annottees.map(_.tree) match {
      case List(q"object $name { ..$body }") if body.isEmpty =>
        c.Expr[Any](
          q"""
          object $name {
            import scala.language.implicitConversions
            import scala.jdk.CollectionConverters._

            trait JooqRecordLike[R <: org.jooq.Record] {
              def record: R
            }

            implicit def RecordLikeToRecord[R <: org.jooq.Record] (rl: JooqRecordLike[R]): R = {
              rl.record
            }
            ..${tables.flatMap(genTableTypes)}
          }
            """
        )
      case a => bail(
        s"You must annotate an object definition with an empty body. instead found $a"
      )
    }

  }

  def fromCatalog_impl(annottees: c.Expr[Any]*): c.Expr[Any] = {
    val macroTypeWithArguments = c.typecheck(q"${c.prefix.tree}").tpe
    val annotationClass: ClassSymbol = macroTypeWithArguments.typeSymbol.asClass
    val annotationTypePlaceholder: Type = annotationClass.typeParams.head.asType.toType
    val catalog: Type = annotationTypePlaceholder.asSeenFrom(macroTypeWithArguments, annotationClass)

    val schemas = catalog.decls
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
              import scala.jdk.CollectionConverters._

              trait JooqRecordLike[R <: org.jooq.Record] {
                def record: R
              }

              implicit def RecordLikeToRecord[R <: org.jooq.Record] (rl: JooqRecordLike[R]): R = {
                rl.record
              }

              ..${schemas.map(genSchemaObject)}
            }
          """
        )
      case a => bail(
        s"You must annotate an object definition with an empty body. instead found $a"
      )
    }
  }

  private def underscoreToCamel(name: String) = "_([a-z\\d])".r.replaceAllIn(name, { m =>
    (if (m.group(1).matches("^[0-9]$")) "_" else "") + m.group(1).toUpperCase()
  }).replaceAll("_$", "")
}
