#lang racket

(require "../lib/hardware-monitor.rkt")

;; Run a task while monitoring resources in the background.

(define fname "resources.log")

(define monitor
  (lambda () (start-log fname #:time-step 0.1)))


(define monitor-worker (thread monitor))
(sleep 1)
(displayln "Monitorning resources, please wait...")
(sleep 1)
(kill-thread monitor-worker)
(sleep 1)
(displayln "Monitor results (first 20 lines):")
(sleep 2)
(with-input-from-file
  fname
  #:mode 'text
  (lambda ()
    (for ([l (sequence->list (in-lines))])
      (display l))))

;(delete-file fname)
