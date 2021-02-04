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

(define data (read-keyvalue-file "test.txt"))

(for/list ((d data))
  (for (((key value) (in-hash d)))
    (printf "Key: ~a --- Value: ~a\n" key value)))

(delete-file fname)

