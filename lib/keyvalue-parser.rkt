#lang racket


(provide read-keyvalue-file)


(define (read-keyvalue-file filename #:mode [mode 'text])
  ;; A keyvalue file is a text file with one record per line
  ;; e.g.: "Item: bolts\nQuantity: 500\nType: M14\n".
  ;; Returns a list of hashes, where each hash contains the key-value
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
                      ;; ds is the list of dictionaries parsed so far,
                      ;; l is the current line.
                      (let ((p (parse-line l)))
                        (cond ((null? p) ds)  ;; We probably read an empty line.
                              ((or (null? ds)
                                   (hash-has-key? (car ds) (car p)))
                               (cons (hasheq (car p) (cdr p)) ds))
                              (else
                               (cons (hash-set (car ds) (car p) (cdr p)) (cdr ds))))))))
    (with-input-from-file filename
                          #:mode mode
      (lambda ()
        (sequence-fold parse-lines '() (in-lines))))))
