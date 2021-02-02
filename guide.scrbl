#lang scribble/manual

@title{A basic data management library}

@section{Automating SQLite queries}

The library has a few functions that can be handy for creating strings describing some typical queries in the SQLite language.

The script using these functions must require them as dependences, as follows:

@codeblock|{
(require "lib/sqlite-query-maker.rkt")
}|

We can create a new table by first creating a new database or accessing to an existing one with the standard db package, then we just have to call @tt{make-table} with the appropriate parameters.

@codeblock|{
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
}|

To change the structure of an existing table, use @tt{insert-col}, as follows.
@codeblock|{
(define new-cols '((team_name . text)
                   (address . text)))
(for/list ((q (insert-col tablename
                          new-cols)))
  (query-exec db q))
}|


To insert data into an existing row, use @tt{insert-row}.
@codeblock|{
(define vals '((ids . 42)
               (name . "John Doe")
               (age . 33.3)))
(query-exec db (insert-row tablename vals))
}|


To update the content of an existing row, use @tt{update-row}.
@codeblock|{
(define new-vals '((team_name . "QPR")
                  (address . "Loftus Road")))
(query-exec db (update-row tablename
                           vals
                           new-vals))
}|
