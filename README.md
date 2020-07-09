[![Build Status](https://circleci.com/gh/StreetContxt/jooq-scala-macro.svg?style=shield&circle-token=7ad3b21e88294341c19094eb316ef5f8865b10e8)](https://circleci.com/gh/StreetContxt/jooq-scala-macro)

# Introduction

The `jooq-scala-macro` project consists of a public type provider for Scala which generates immutable Scala classes based on the standard generated JOOQ meta-model classes.

Implicit conversions are also provided to go to/from the Java and Scala representations, and the companion object for a table extends the `org.jooq.Table` implementation allowing the companion to be used in JOOQ queries.

More detailed documentation is being worked on, in the meantime feel free to ask questions in the [Gitter Channel](https://gitter.im/streetcontxt/Lobby).

# Scala & Jooq versions support

Version 13.0:

| Scala version | JOOQ version |
|---------------|--------------|
|    2.11.x     |   ≥ 3.10.2   |
|    2.12.x     |   ≥ 3.10.2   |


Version 13.1+:

| Scala version | JOOQ version |
|---------------|--------------|
|    2.11.x     |   ≥ 3.10.8   |
|    2.12.x     |   ≥ 3.10.8   |
|    2.13.x*    |   ≥ 3.13.    |

\* JOOQ configuration notes for Scala 2.13.x & JOOQ 3.13.x:

The JOOQ code generation process default settings has an important change in v3.13. By default, array types were using varargs setters in versions 3.9..3.12, but then this has been switched to collections in 3.13.

The macro code does not have any knowledge about the settings used in generation, and assumes the varargs setters for backward compatibility, which could generate an incompatible code:
```
... type mismatch;
[error]  found   : Array[String]
[error]  required: Array[_ <: Array[String]]
[error] @fromCatalog[DefaultCatalog]
```

To fix the above error, the varargs setters should be explicitly enabled in JOOQ codegen configuration in the client code:

```xml
<configuration>
    ...
    <generator>
        ...
        <generate>
            <varargSetters>true</varargSetters>
        </generate>
    </generator>
</configuration>

```

# Example Code

```scala

import my.jooq.generated.package.DefaultCatalog

@fromCatalog[DefaultCatalog]
object Database { }

import Database._
import Database.MySchema._

val dsl: DSLContxt = ???

// implicitly converts from AuthorRecord to Author
// the companion object for Author extends the jooq generated table for Author
val author: Author = dsl.selectFrom(Author)
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

This project was written separately from the JOOQ project, our use of the name shouldn't be taken as any sort of endorsement from the developers of that library.
