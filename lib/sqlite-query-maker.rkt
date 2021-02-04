#lang racket/base

(require srfi/13
         racket/format)

(provide make-table)
(provide insert-row)
(provide insert-col)
(provide update-row)


(define (unzip-with op select s)
  ;; Given a list of lists s, returns a list obtained by
  ;; applying op to each element of s, whose position in
  ;; sublist is determined by the selector select.
  (map (lambda (el)
         (op (select el)))
       s))


(define (prune-and-terminate s term)
  ;; Removes the last comma and whitespace from the string s
  ;; and appends the terminating string term.
  (string-append
    (substring s 0 (string-index-right s #\,))
    term))


(define (preprocess-value val)
  ;; if val is a string or symbol, returns it as a string within quotes
  ;; otherwise just converts it to string.
  (cond ((number? val) (~a val))
         (else (string-append "\"" val "\""))))


(define (make-table name cols)
  ;; Returns a query for creating a new table with name "name"
  ;; with columns and column types expressed as a list of pairs
  ;; in "cols". Example: '((name . string) (age . integer) ... (weight . real))
  (letrec ((query (format "CREATE TABLE ~a(" name))
           (col-names (unzip-with ~a car cols))
           (col-types (unzip-with ~a cdr cols))
           (A (lambda (s0 ns ts) ; initial string, names and types of columns
                (cond ((null? ns) (prune-and-terminate s0 ");"))
                      (else (A (string-append s0 (car ns) " " (car ts) ", ")
                               (cdr ns)
                               (cdr ts)))))))
    (A query col-names col-types)))


(define (insert-col tabname cols)
  ;; Returns a list of sqlite queries (each expressed as a string) that inserts new
  ;; columns in the table tabname. cols is a list of name-type pairs.
  ;; Example: cols = '((name . text) ... (weight . real))
  (letrec ((query (format "ALTER TABLE ~a ADD COLUMN " tabname))
           (col-names (unzip-with ~a car cols))
           (col-types (unzip-with ~a cdr cols))
           (A (lambda (s0 ns ts)
                ;; s0: string
                ;; ns: list of column names (as symbols)
                ;; vs: list of column types (as symbols)
                (cond ((null? ns) '())
                      (else (cons (string-append s0 (car ns) " " (car ts) ";")
                                  (A query (cdr ns) (cdr ts))))))))
    (A query col-names col-types)))


(define (insert-row name vals)
  ;; name is the table name, vals is a list of pairs
  ;; in the form: '((colname-1 . val-1) ... (colname-n . val-n))
  ;; the values are assumed to be already of the appropriate type
  ;; (e.g. ints, floats, strings, ...)
  (letrec ((query (format "INSERT INTO ~a(" name))
           (id (lambda (x) x))
           (col-names  (unzip-with ~a car vals))
           (col-values (unzip-with id cdr vals))
           (A (lambda (s0 ns vs)
                (cond ((null? ns) (B (prune-and-terminate s0 ") VALUES (") vs))
                      (else (A (string-append s0 (car ns) ", ") (cdr ns) vs)))))
           (B (lambda (s0 vs)
                (cond ((null? vs) (prune-and-terminate s0 ");"))
                      ; we need to check if the value is a string,
                      ; and change the query accordingly.
                      (else (B (string-append s0
                                              (preprocess-value (car vs)) ", ")
                               (cdr vs)))))))
    (A query col-names col-values)))
           

(define (update-row name where vals)
  ;; updates table "name" with the values "vals" in the row determined by
  ;; "where". Both "where" and "vals" should be list of pairs with the
  ;; following structure:
  ;; where: '((col-name . value) ... (col-name-n . value-n))
  ;; vals:  '((col-name-to-update-1 . vals-update-1) ...)
  (letrec ((query (format "UPDATE ~a SET " name))
           (id (lambda (x) x))
           (where-names  (unzip-with ~a car where))
           (where-values (unzip-with id cdr where))
           (col-names  (unzip-with ~a car vals))
           (col-values (unzip-with ~a cdr vals))
           (U (lambda (s0 ns vs)
                ;; "update" part of the query
                (cond ((null? ns) (W (prune-and-terminate s0 " WHERE ")
                                     where-names
                                     where-values))
                      (else (U (string-append s0
                                              (car ns) " = " (preprocess-value (car vs)) ", ")
                               (cdr ns)
                               (cdr vs))))))
           (W (lambda (s0 ns vs [and-clause? #f])
                ;; "where" part of the query
                (let ((s0-prefix (if and-clause? (string-append s0 " AND ") s0)))
                  (cond ((null? ns) (error "Update values field is empty."))
                        ((null? (cdr ns)) (string-append
                                           s0-prefix
                                           (car ns) " = " (preprocess-value (car vs)) ";"))
                        (else (W (string-append
                                  s0-prefix
                                  (car ns) " = " (preprocess-value (car vs)))
                                 (cdr ns)
                                 (cdr vs)
                                 #t)))))))
    (U query col-names col-values)))
