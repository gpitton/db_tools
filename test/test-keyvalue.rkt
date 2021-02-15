#lang racket/base

;; Tests the keyvalue utilities.


(require "../lib/keyvalue-parser.rkt")

;; generate a random keyvalue file for testing
(define fname "test.txt")
(call-with-output-file fname
                       #:exists 'replace
                       #:mode 'text
  (lambda (out)
    (for ((i (in-range 10)))
      (fprintf out "ID: ~a\nName: st~a\n\n" i i))))


(define (print-all data)
  (for* ((d data)
        ((key value) (in-hash d)))
      (printf "Key: ~a --- Value: ~a\n" key value)))


(define data (read-keyvalue-file "test.txt"))
(print-all data)

(newline)
(display "With #:keys parameter:")
(newline)
(define data2 (read-keyvalue-file "test.txt" #:keys '(Name)))
(print-all data2)

(delete-file fname)

