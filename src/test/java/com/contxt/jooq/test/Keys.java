/*
 * This file is generated by jOOQ.
*/
package com.contxt.jooq.test;


import com.contxt.jooq.test.tables.Author;
import com.contxt.jooq.test.tables.Book;
import com.contxt.jooq.test.tables.BookStore;
import com.contxt.jooq.test.tables.BookToBookStore;
import com.contxt.jooq.test.tables.Language;
import com.contxt.jooq.test.tables.records.AuthorRecord;
import com.contxt.jooq.test.tables.records.BookRecord;
import com.contxt.jooq.test.tables.records.BookStoreRecord;
import com.contxt.jooq.test.tables.records.BookToBookStoreRecord;
import com.contxt.jooq.test.tables.records.LanguageRecord;

import javax.annotation.Generated;

import org.jooq.ForeignKey;
import org.jooq.UniqueKey;
import org.jooq.impl.AbstractKeys;


/**
 * A class modelling foreign key relationships and constraints of tables of 
 * the <code>TEST</code> schema.
 */
@Generated(
    value = {
        "http://www.jooq.org",
        "jOOQ version:3.10.2"
    },
    comments = "This class is generated by jOOQ"
)
@SuppressWarnings({ "all", "unchecked", "rawtypes" })
public class Keys {

    // -------------------------------------------------------------------------
    // IDENTITY definitions
    // -------------------------------------------------------------------------


    // -------------------------------------------------------------------------
    // UNIQUE and PRIMARY KEY definitions
    // -------------------------------------------------------------------------

    public static final UniqueKey<AuthorRecord> CONSTRAINT_7 = UniqueKeys0.CONSTRAINT_7;
    public static final UniqueKey<BookRecord> CONSTRAINT_1 = UniqueKeys0.CONSTRAINT_1;
    public static final UniqueKey<BookStoreRecord> CONSTRAINT_F = UniqueKeys0.CONSTRAINT_F;
    public static final UniqueKey<BookToBookStoreRecord> CONSTRAINT_2 = UniqueKeys0.CONSTRAINT_2;
    public static final UniqueKey<LanguageRecord> CONSTRAINT_C = UniqueKeys0.CONSTRAINT_C;

    // -------------------------------------------------------------------------
    // FOREIGN KEY definitions
    // -------------------------------------------------------------------------

    public static final ForeignKey<BookRecord, AuthorRecord> FK_BOOK_AUTHOR = ForeignKeys0.FK_BOOK_AUTHOR;
    public static final ForeignKey<BookRecord, LanguageRecord> FK_BOOK_LANGUAGE = ForeignKeys0.FK_BOOK_LANGUAGE;
    public static final ForeignKey<BookToBookStoreRecord, BookStoreRecord> FK_B2BS_BOOK_STORE = ForeignKeys0.FK_B2BS_BOOK_STORE;
    public static final ForeignKey<BookToBookStoreRecord, BookRecord> FK_B2BS_BOOK = ForeignKeys0.FK_B2BS_BOOK;

    // -------------------------------------------------------------------------
    // [#1459] distribute members to avoid static initialisers > 64kb
    // -------------------------------------------------------------------------

    private static class UniqueKeys0 extends AbstractKeys {
        public static final UniqueKey<AuthorRecord> CONSTRAINT_7 = createUniqueKey(Author.AUTHOR, "CONSTRAINT_7", Author.AUTHOR.ID);
        public static final UniqueKey<BookRecord> CONSTRAINT_1 = createUniqueKey(Book.BOOK, "CONSTRAINT_1", Book.BOOK.ID);
        public static final UniqueKey<BookStoreRecord> CONSTRAINT_F = createUniqueKey(BookStore.BOOK_STORE, "CONSTRAINT_F", BookStore.BOOK_STORE.NAME);
        public static final UniqueKey<BookToBookStoreRecord> CONSTRAINT_2 = createUniqueKey(BookToBookStore.BOOK_TO_BOOK_STORE, "CONSTRAINT_2", BookToBookStore.BOOK_TO_BOOK_STORE.NAME, BookToBookStore.BOOK_TO_BOOK_STORE.BOOK_ID);
        public static final UniqueKey<LanguageRecord> CONSTRAINT_C = createUniqueKey(Language.LANGUAGE, "CONSTRAINT_C", Language.LANGUAGE.ID);
    }

    private static class ForeignKeys0 extends AbstractKeys {
        public static final ForeignKey<BookRecord, AuthorRecord> FK_BOOK_AUTHOR = createForeignKey(com.contxt.jooq.test.Keys.CONSTRAINT_7, Book.BOOK, "FK_BOOK_AUTHOR", Book.BOOK.AUTHOR_ID);
        public static final ForeignKey<BookRecord, LanguageRecord> FK_BOOK_LANGUAGE = createForeignKey(com.contxt.jooq.test.Keys.CONSTRAINT_C, Book.BOOK, "FK_BOOK_LANGUAGE", Book.BOOK.LANGUAGE_ID);
        public static final ForeignKey<BookToBookStoreRecord, BookStoreRecord> FK_B2BS_BOOK_STORE = createForeignKey(com.contxt.jooq.test.Keys.CONSTRAINT_F, BookToBookStore.BOOK_TO_BOOK_STORE, "FK_B2BS_BOOK_STORE", BookToBookStore.BOOK_TO_BOOK_STORE.NAME);
        public static final ForeignKey<BookToBookStoreRecord, BookRecord> FK_B2BS_BOOK = createForeignKey(com.contxt.jooq.test.Keys.CONSTRAINT_1, BookToBookStore.BOOK_TO_BOOK_STORE, "FK_B2BS_BOOK", BookToBookStore.BOOK_TO_BOOK_STORE.BOOK_ID);
    }
}