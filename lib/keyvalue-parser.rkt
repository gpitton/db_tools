#lang racket/base


(require racket/format
         racket/sequence
         racket/stream
         racket/string)
(provide read-keyvalue-file)


(define (read-keyvalue-file filename #:mode [mode 'text])
  ;; A keyvalue file is a text file with one record per line
  ;; e.g.: "Item: bolts\nQuantity: 500\nType: M14\n".
  ;; Returns a stream of hashes, where each hash contains the key-value
  ;; data of each block. Both the keys and the values of the hash map
  ;; are returned as symbols.
  (let*
      ((parse-line (lambda (s)
                     ;; Given a string s, splits it in key and value components,
                     ;; and returns a pair in the form: (key . value).
                     (let* ((ts (string-trim (string-trim s "\n" #:repeat? #t) #:repeat? #t))
                            (ps (string-split ts ":")))
                       (cond ((null? ps) ps)
                             (else (cons (string->symbol (car ps))
                                         (string->symbol (cadr ps))))))))
       (parse-lines (lambda (ds l)
                      ;; ds is the stream of dictionaries parsed so far,
                      ;; l is the current line.
                      (let ((p (parse-line l)))
                        (cond ((null? p) ds)  ;; We probably read an empty line.
                              ((or (stream-empty? ds)
                                   (hash-has-key? (stream-first ds) (car p)))
                               (stream-cons (hasheq (car p) (cdr p)) ds))
                              (else
                               (stream-cons (hash-set (stream-first ds) (car p) (cdr p))
                                            (stream-rest ds))))))))
    (with-input-from-file filename
                          #:mode mode
      (lambda ()
        (sequence-fold parse-lines '() (in-lines))))))
