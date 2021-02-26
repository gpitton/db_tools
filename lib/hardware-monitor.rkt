#lang racket/base                                                     

(require racket/contract
         racket/port
         racket/string)

(provide start-log)


(define (call-shell-command cmd . args)
  (lambda ()
    (let*-values
      ([(cmd-full) (format "/usr/bin/~a" cmd)]
       [(p stdout stdin stderr)
        (apply subprocess #f #f #f cmd-full args)]
       [(res) (port->string stdout)])
    (close-input-port stdout)
    (close-output-port stdin)
    (close-input-port stderr)
    (subprocess-wait p)
    res)))

(define get-free-mem (call-shell-command "free" "-m"))
(define get-vmstat (call-shell-command "vmstat"))
(define get-top (call-shell-command "top" "-ibn1"))


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


(define/contract (start-log [fname "resources.log"]
                   #:time-step [time-step 0.5]
                   #:mode [mode 'text]
                   #:exists [exists 'replace])
  (->* ()
       (string?
        #:time-step positive?
        #:mode (or/c 'binary 'text)
        #:exists (or/c 'error 'append 'update 'replace 'truncate 'truncate/replace))
       any)
  (with-output-to-file fname
       #:mode mode
       #:exists exists
       (lambda ()
         (for ((_ (in-naturals)))
           (let* ((mem (parse-free-mem (get-free-mem)))
                  ;(cpu (parse-vmstat (get-vmstat)))
                  (cpu (parse-top (get-top)))
                  (usr (car cpu))
                  (sys (cdr cpu)))
             (printf "~a ~a ~a\n" usr sys mem)
             (sleep time-step))))))

