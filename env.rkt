#lang racket

(provide (all-defined-out))

(define (ext env v val)
  (hash-set env v val))

; ext multi
(define (exts env vs vals)
  (foldl
    (lambda (v val env)
      (ext env v val))
    env
    vs
    vals))

(define (ext-env env1 env2)
  (exts
    env1
    (hash-keys env2)
    (hash-values env2)))

(define (env? v)
  (and (hash? v) (hash-eq? v)))

(define (app env v)
  (hash-ref env v (lambda ()
                    (error 'env:app "~a not found" v))))

(define (init vs vals)
  (exts (empty) vs vals))

(define (empty)
  (make-immutable-hasheq))
