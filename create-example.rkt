#lang racket

(require db)
(require "lib/sqlite-query-maker.rkt")

(define dbname "test.db")
(define tablename "mytable")
(define cols '((ids . integer)
               (name . text)
               (age . real)))

(define db
  (sqlite3-connect #:database "test.db"
                   #:mode 'create))

(query-exec db
            (make-table tablename
                        cols))

(display (make-table tablename cols))
(newline)


(define vals '((ids . 42)
               (name . "Test")
               (age . 13.5)))
(query-exec db (insert-row tablename vals))
(display (insert-row tablename vals))
(newline)

(define new-cols '((team_name . text)
                   (address . text)))

(for/list ((q (insert-col tablename new-cols)))
  (query-exec db q))

(for/list ((q (insert-col tablename new-cols)))
  (display q)
  (newline))

(define new-vals '((team_name . "QPR")
                  (address . "Loftus Road")))

(query-exec db (update-row tablename vals new-vals))
(display (update-row tablename vals new-vals))
(newline)

