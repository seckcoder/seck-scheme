#lang racket

(require "base.rkt"
         "global.rkt"
         "../../base/utils.rkt")

(provide closure-conversion)
(define (closure-conversion e dir)
  (letrec ([cvt1 (lambda (e)
                   (match e
                     [`(lambda (,v* ...) ,body)
                       (let ([fvs (set->list (free e))]
                             [proc-n (gensym 'proc_)]
                             [r (gensym 'env)])
                         (add-global!
                           proc-n
                           `(proc ,(cons r v*)
                                  ,(subst body r fvs)))
                         `(closure ,proc-n
                                   (vec ,@fvs)))]
                     [_ e]
                     ))]
           [cvt-up (lambda (e)
                     (define (cvt-up1 e)
                       (match e
                         [(? immediate?)
                          e]
                         [(? string?)
                          e]
                         [(? symbol?)
                          e]
                         [`(constant-ref ,v)
                           ; for constant-ref, return it directly
                           e]
                         [(list (? prim-op? op) v* ...)
                          `(,op ,@(map cvt-up v*))]
                         [`(if ,test ,then ,else)
                           `(if ,(cvt-up test)
                              ,(cvt-up then)
                              ,(cvt-up else))]
                         [`(let ((,v* ,e*) ...) ,body)
                           `(let ,(map list v* (map cvt-up e*))
                              ,(cvt-up body))]
                         [`(begin ,exp* ...)
                           `(begin
                              ,@(map cvt-up exp*))]
                         [`(labels ((,v* ,e*) ...) ,exp)
                           ; for debugging purpose
                           e]
                         [`(lambda (,v* ...) ,body)
                           `(lambda ,v*
                              ,(cvt-up body))]
                         [`(app ,rator ,rand* ...)
                           (let ([rator (cvt-up rator)]
                                 [rand* (map cvt-up rand*)])
                             `(app ,rator ,@rand*))]
                         [`(app-proc ,rator ,rand* ...)
                           ; for debuggin purpose
                           e]
                         [_ (error 'cvt-up "~a not match" e)]
                         ))
                     (cvt1 (cvt-up1 e)))]
           [cvt-down (lambda (e) e)]
           [cvt (lambda (e)
                  (if (eq? dir 'topdown)
                    (cvt-down e)
                    (cvt1 (cvt-up e))))])
    (cvt e)))

(define free
  (lambda (e)
    (match e
      [(? immediate?)
       (seteq)]
      [(? string?)
       (seteq)]
      [(? symbol? v)
       (seteq v)]
      [`(constant-ref ,v)
        ; v is not free variable, it's global variable
        (seteq)]
      [(list (? prim-op? op) v* ...)
       (free-U v*)]
      [`(if ,test ,then ,else)
        (free-U (list test then else))]
      [`(let ((,v* ,e*) ...) ,body)
        (set-union (free-U e*)
                   (set-subtract (free body)
                                 (list->seteq v*)))]
      [`(begin ,exp* ...)
        (free-U exp*)]
      [`(lambda (,v* ...) ,body)
        (set-subtract (free body)
                      (list->seteq v*))]
      [`(closure ,f ,rv)
        (free rv)]
      [`(app ,rator ,rand* ...)
        (free-U (cons rator rand*))]
      [`(app-clj ,rator ,rand* ...)
        (free-U (cons rator rand*))]
      [_ (error 'free "~a not match" e)]
      )))
(define free-U
  (lambda (es)
    (foldl
      (lambda (e u)
        (set-union u (free e)))
      (seteq)
      es)))

(define (list-subtract l1 l2)
  (set->list
    (set-subtract
      (list->seteq l1)
      (list->seteq l2))))

; for v in e, if v is in fvs, then, subst
(define subst
  (lambda (e env fvs)
    (define (subst1 e)
      (match e
        [(? immediate?) e]
        [(? string?) e]
        [(? symbol? v)
         (let ([idx (index-of fvs v)])
           (if (>= idx 0)
             `(vec-ref ,env ,idx)
             v))]
        [`(constant-ref ,v)
          ; don't replace constant-ref
          e]
        [(list (? prim-op? op) v* ...)
         `(,op ,@(map subst1 v*))]
        [`(if ,test ,then ,else)
          `(if ,(subst1 test)
             ,(subst1 then)
             ,(subst1 else))]
        [`(let ((,v* ,e*) ...) ,body)
          `(let ,(map list v* (map subst1 e*))
             ,(subst body env (list-subtract fvs v*)))]
        [`(begin ,exp* ...)
          `(begin
             ,@(map subst1 exp*))]
        [`(lambda (,v* ...) ,body)
          `(lambda ,v*
             ,(subst body env (list-subtract fvs v*)))]
        [`(closure ,f ,rv)
          `(closure ,f
                    ,(subst1 rv))]
        [`(proc (,v* ...) ,body)
          ; proc means it's already substituted
          e]
        [`(app ,rator ,rand* ...)
          `(app ,(subst1 rator) ,@(map subst1 rand*))]
        [_ (error 'subst "~a not match" e)]
        ))
    (subst1 e)))

(define (env-from-fvs fvs)
  (make-immutable-hasheq
    (map
      (lambda (x)
        (cons x x))
      (set->list fvs))))


(module+ test
  (define (test-bottom-up e)
    (pretty-write e)
    (printf "--------->\n")
    (pretty-write
      (closure-conversion
        e
        'bottom-up))
    (newline))
  (test-bottom-up
    '(app
       (lambda (x)
         (+ x y)) u))

  (test-bottom-up
    '(lambda (v)
       (app
         (lambda (x)
           (+ x v)) u)))

  (test-bottom-up
    '(let ([f (lambda (v)
                (+ v u))])
       (app f 3)))

  (test-bottom-up
    ; Notice value:u
    '(lambda (v)
       (app
         (lambda (x)
           (+ x u)) v)))

  (test-bottom-up
    ; A multi-arg version
    '(lambda (v)
       (app
         (lambda (x)
           (+ (+ x u)
              w)) v)))

  (test-bottom-up
    '(let ([v 3])
       (app
         (lambda (u)
          (+ v u)) v)))
  )
