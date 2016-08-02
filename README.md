# Introduction

The Jooq-scala-macro project consists of a public type provider for scala which generates immutable scala classes based on the standard generated jooq meta-model classes

Implicit conversions are also provided to go to/from the java and scala representations, and the companion object for a table extends the `org.jooq.Table` implementation allowing the companion to be used in Jooq queries

More detailed documentation is being worked on, in the meantime feel free to ask questions in the [Gitter Channel](https://gitter.im/streetcontxt/Lobby).

# Example Code

```scala

import my.jooq.generated.package.DefaultCatalog

@fromCatalog[DefaultCatalog]
object Database { }

import Database._
import Database.MySchema._
import

val dsl: DSLContxt = ???

// implicitly converts from AuthorRecord to Author
// the companion object for Author extends the jooq generated table for Author
val author: Author = dsl.selectFromAuthor)
                        .where(Author.ID === 1)
                        .fetchOne()

// generated classes have a .copy() which "remembers" which fields were modified
author.copy(firstName = "Bob")

// implicitly converts from Author to AuthorRecord
author.changed(Author.FIRST_NAME)

```

# Questions and issues

The [github issue tracker](https://github.com/StreetContxt/jooq-scala-macro/issues) can be used for bug reports and feature requests.

Our [Gitter](https://gitter.im/streetcontxt/Lobby) channel can be used to ask questions.

# Note

This project was written separately from the Jooq project, our use of the name shouldn't be taken as any sort of endorsment from the developers of that library.