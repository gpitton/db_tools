#lang scribble/manual

@(require scribble/eval
          (for-label "../lib/sqlite-query-maker.rkt"))

@title{A basic data management library}

@defmodule[lib/sqlite-query-maker]

@section[#:tag "queries"]{Automating SQLite queries}

The library has a few functions that can be handy for creating strings describing some typical queries
in the SQLite language.


We can create a new table by first creating a new database or accessing to an existing one with the
standard db package, then we just have to call @racket[make-table] with the appropriate parameters.

@(define helper-eval (make-base-eval))
@interaction-eval[
 #:eval helper-eval
 (require "../lib/sqlite-query-maker.rkt")
 (define tablename "atable")
 (define cols '((ids . integer)
                (name . text)
                (age . real)))
 (define new-cols '((team_name . text)
                   (address . text)))
 (define vals '((ids . 42)
               (name . "John Doe")
               (age . 33.3)))
 (define new-vals '((team_name . "QPR")
                  (address . "Loftus Road")))]

TODO: implement these contracts in the library


@defproc[(make-table [tablename (or/c symbol? string?)]
                     [cols (listof pair?)])
         (string?)]{
Returns a string that can be used as a SQLite query that creates a new table in an existing database.
The name of the new table must be specified either as a symbol or as a string.
The columns of the new table must be given as a list of pairs, where the first element of the pair is
the name of the column and the second element of the pair must be a valid SQLite type.

@racketblock[
 (define tablename "atable")
 (define cols '((ids . integer)
                (name . text)
                (age . real)))

 (require db)
 (define db
   (sqlite3-connect #:database "mydb.db"
                    #:mode 'create))
 (query-exec db (make-table tablename
                            cols))
]

@examples[
 #:eval helper-eval
 (display (make-table tablename cols))
]}


@defproc[(insert-col [tablename (or/c symbol? string?)]
                     [new-cols  (listof? pair?)])
         (listof (string?))]{
 Returns a list of string, and each string in the list can be used as a SQLite query that creates
 new columns for an existing table.
 
 To change the structure of an existing table, use @tt{insert-col} as follows (note that since the
 output of @tt{insert-col} is a list of string, we need to iterate over the resulting list).
@racketblock[
(define new-cols '((team_name . text)
                   (address . text)))
(for/list ((q (insert-col tablename
                          new-cols)))
  (query-exec db q))
]

@examples[
  #:eval helper-eval
  (for/list ((q (insert-col tablename new-cols)))
    (display q)
    (newline))
]}


@defproc[(insert-row [tablename (or/c symbol? string?)]
                     [values (listof? pair?)])
         (string?)]{
Returns a string that can be used as a SQLite query to insert a new row in an existing table.

To insert data into an existing row, use @tt{insert-row} as in the next example.
@racketblock[
(define vals '((ids . 42)
               (name . "John Doe")
               (age . 33.3)))
(query-exec db (insert-row tablename vals))
]

@examples[
  #:eval helper-eval
  (display (insert-row tablename vals))
]}


@defproc[(update-row [tablename (or/c symbol? string?)]
                     [where (listof? pair?)]
                     [values (listof? pair?)])
         (string?)]{
Returns a string that can be used as a SQLite query to update a row in an existing table.
@tt{where} is a list of @tt{(column-name . value)} pairs that is used to identify a row in
@tt{tablename} that we want to update.
@tt{values} is a list of @tt{(column-to-update . new-value)} pairs that specify which column
to update and the new value for the corresponding column.
                    
To update the content of an existing row, use @tt{update-row} as in the following example.
@racketblock[
(define new-vals '((team_name . "QPR")
                  (address . "Loftus Road")))
(query-exec db (update-row tablename
                           vals
                           new-vals))
]

@examples[
  #:eval helper-eval
  (display (update-row tablename vals new-vals))
]}
