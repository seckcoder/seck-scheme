#lang racket

(provide (all-defined-out))

(define (ext! env v val)
  (hash-set! env v val))

(define (env? v)
  (hash-eq? v))

(define (app env v)
  (hash-ref env v (lambda ()
                    (error 'mutable-env:app "~a not found" v))))

(define (empty)
  (make-hasheq))

