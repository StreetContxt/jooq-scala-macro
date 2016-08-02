package com.contxt.db

import java.sql.DriverManager

import org.specs2.Specification
import com.contxt.db.{TestDatabase => db}
import db.Test._
import org.jooq.SQLDialect
import org.jooq.impl.DSL._
import scala.collection.JavaConverters._

class JooqGeneratorTableSpec extends Specification {
  def is = s2"""
  |The generated object for representing tables
  |  We should be able to use the generated companion object for selects $select
  |  We should be able to use the generated companion object for selectfrom $selectFrom
  """.stripMargin

  Class.forName("org.h2.Driver")
  val dsl = using(DriverManager.getConnection("jdbc:h2:localhost:9092:/~/blah", "", ""), SQLDialect.H2)
  val select = dsl.select(count(Author.ID)).from(Author).fetchOne().value1() must_== 2
  val selectFrom = dsl.selectFrom(Author).fetch().asScala.toList must have size 2
}
