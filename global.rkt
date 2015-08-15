#lang racket

(provide (all-defined-out))

(define *global* '())

(define (init-global!)
  (set! *global* '()))

(define (add-global! n code)
  (set! *global* (cons (list n code)
                       *global*)))

#|(define *proc* '())
(define (init-global!)
  (set! *proc* '()))

(define (add-global-proc! n code)
  ;(env!:ext! *global* n proc-n)
  (set! *proc* (cons (list n code)
                     *proc*)))|#
