#lang racket/base


(require racket/format
         racket/sequence
         racket/stream
         racket/string)
(provide read-keyvalue-file)


(define (read-keyvalue-file filename #:mode [mode 'text] #:keys [keys 'all])
  ;; A keyvalue file is a text file with one record per line
  ;; e.g.: "Item: bolts\nQuantity: 500\nType: M14\n".
  ;; Returns a stream of hashes, where each hash contains the key-value
  ;; data of each block. Both the keys and the values of the hash map
  ;; are returned as symbols.
  ;; if keys is set, parses only the given keys.
  (letrec
      ((key (lambda (h) (car h)))
       (val (lambda (h) (cdr h)))
       (split-line (lambda (s)
                     ;; Given a string s, splits it in key and value components,
                     ;; and returns a pair in the form: (key . value).
                     (let* ((ts (string-trim (string-trim s "\n" #:repeat? #t) #:repeat? #t))
                            (ps (string-split ts ":")))
                       (cond ((null? ps) ps)
                             (else (cons (string->symbol (car ps))
                                         (string->symbol (cadr ps))))))))
       (parse-lines (lambda (ds l)
                    ;; ds is the stream of dictionaries parsed so far,
                    ;; l is the current line (as a stream of lines).
                    (if (stream-empty? l) ds
                      (let ((p (split-line (stream-first l))))
                        (cond ((null? p)  ;; We probably read an empty line.
                               (parse-lines ds (stream-rest l)))
                              ((and (pair? keys)
                                    (not (memq (key p) keys)))
                               ;; The current key is not among the required keys
                               (parse-lines ds (stream-rest l)))
                              ((or (null? ds)
                                   (hash-has-key? (car ds) (key p)))
                                 ;; We need a new hash map at the front of the stream.
                                 (parse-lines (cons (hasheq (key p) (val p)) ds)
                                              (stream-rest l)))
                              (else
                               ;; We need to add a new key to the existing front of the stream
                               (parse-lines (cons (hash-set (car ds) (key p) (val p)) (cdr ds))
                                            (stream-rest l)))))))))
    (with-input-from-file filename
                          #:mode mode
      (lambda ()
        (parse-lines '() (sequence->stream (in-lines)))))))

