package com.contxt.db

import java.sql.DriverManager

import com.contxt.db.TestDatabase._
import Test._
import com.contxt.jooq.test.tables.records.AuthorRecord
import org.jooq.SQLDialect
import org.jooq.impl.DSL._
import org.jooq.scala.Conversions._
import org.specs2.Specification

class JooqGeneratorRecordSpec extends Specification {
  def is = s2"""
  |The generated class for representing records
  |  We should be able to use copy and have it mark fields as changed $changed
  """.stripMargin

  Class.forName("org.h2.Driver")
  val dsl = using(DriverManager.getConnection("jdbc:h2:./target/test-db", "", ""), SQLDialect.H2)

  val authorRecord: AuthorRecord = dsl.selectFrom(Author).where(Author.ID === 1).fetchOne()
  val author: Author = authorRecord
  val copy: Author = author.copy(firstName = Some("bob"))
  val recordCopy = copy.record
  val isChanged = recordCopy.changed(Author.FIRST_NAME)
  val changed = isChanged should beTrue
}
