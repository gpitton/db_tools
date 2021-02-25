#lang racket/base                                                     

(require racket/port
         racket/string)


(define (get-free-mem)
  (let*-values
    ([(p stdout stdin stderr)
      (subprocess #f #f #f "/usr/bin/free" "-m")]
     [(res) (port->string stdout)])
  (close-input-port stdout)
  (close-output-port stdin)
  (close-input-port stderr)
  (subprocess-wait p)
  res))


(define (get-vmstat)
  (let*-values
    ([(p stdout stdin stderr)
      (subprocess #f #f #f "/usr/bin/vmstat")]
     [(res) (port->string stdout)])
  (close-input-port stdout)
  (close-output-port stdin)
  (close-input-port stderr)
  (subprocess-wait p)
  res))


(define (get-top)
  (let*-values
    ([(p stdout stdin stderr)
      (subprocess #f #f #f "/usr/bin/top" "-ibn1")]
     [(res) (port->string stdout)])
  (close-input-port stdout)
  (close-output-port stdin)
  (close-input-port stderr)
  (subprocess-wait p)
  res))


(define (parse-free-mem s)
  (let* ((rows (string-split s "\n"))
         (row-mem (cadr rows))
         (row-mem-fields (string-split row-mem)))
  (caddr row-mem-fields)))


(define (parse-vmstat s)
  (let* ((rows (string-split s "\n"))
         (row (caddr rows))
         (fields (string-split row))
         (user (list-ref fields 12))
         (sys (list-ref fields 13)))
    (cons user sys)))


(define (parse-top s)
  (let* ((rows (string-split s "\n"))
         (row (caddr rows))
         (fields (string-split row))
         (user (list-ref fields 1))
         (sys (list-ref fields 3)))
    (cons user sys)))


(with-output-to-file "resources.log"
     #:mode 'text
     #:exists 'replace
     (lambda ()
       (for ((_ (in-naturals)))
         (let* ((mem (parse-free-mem (get-free-mem)))
                ;(cpu (parse-vmstat (get-vmstat)))
                (cpu (parse-top (get-top)))
                (usr (car cpu))
                (sys (cdr cpu)))
           (printf "~a ~a ~a\n" usr sys mem)
           (sleep 0.1)))))

