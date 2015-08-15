#lang racket

(require (prefix-in env: "env.rkt")
         "base.rkt")
 
(provide (rename-out [convert alpha-conversion]))

(define (genvar v)
  (gensym v))

; alpha conversion
(define (convert env)
  (letrec ([cvt
             (lambda (e)
               (match e
                 [(? immediate?) e]
                 [(? string?) e]
                 [(? symbol? v)
                  (env:app env v)]
                 [`(quote ,v) e]
                 [(list (? prim-op? op) v* ...)
                  `(,op ,@(map cvt v*))]
                 [`(if ,test ,then ,else)
                   `(if ,(cvt test)
                      ,(cvt then)
                      ,(cvt else))]
                 [`(let ((,v* ,e*) ...) ,body)
                   (match (cvt-vars v* env)
                     [(list v* env)
                      `(let ,(map list v* (map cvt e*))
                         ,((convert env) body))])]
                 [`(begin ,exp* ...)
                   `(begin
                      ,@(map cvt exp*))]
                 [`(lambda (,v* ...) ,body)
                   (match (cvt-vars v* env)
                     [(list v* env)
                      `(lambda ,v*
                         ,((convert env) body))])]
                 [`(app ,rator ,rand* ...)
                   `(app ,(cvt rator)
                         ,@(map cvt rand*))]
                 [_
                   (error 'alpha-conversion
                          "~a not match" e)]))]
           [cvt-vars
             (lambda (vs env)
               (let loop ([new-vs '()]
                          [vs vs]
                          [env env])
                 (if (null? vs)
                   (list (reverse new-vs)
                         env)
                   (let ([new-v (genvar (car vs))])
                     (loop (cons new-v new-vs)
                           (cdr vs)
                           (env:ext env
                                    (car vs)
                                    new-v))))))])
    (lambda (e)
      (cvt e))))


(module+ test
  (define (test-alpha-conversion e)
    ((convert (env:empty)) e))
  (test-alpha-conversion '(lambda (v)
                            (cons v v)))
  )
