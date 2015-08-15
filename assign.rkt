#lang racket

(require "base.rkt")
(provide (rename-out [convert assign-conversion]))


; assignment conversion(bottom-up)
; e -> (list e set)
; e -> (list converted-e unconverted-set!-vars)
(define (convert e)
  (match e
    [(? immediate?)
     (list e (seteq))]
    [(? string?)
     (list e (seteq))]
    [(? symbol?)
     (list e (seteq))]
    [`(constant-ref ,v)
      (list e (seteq))]
    [`(set! ,v ,e1)
      (match (convert e1)
        [(list e1 set!-vars)
         (list `(set! ,v ,e1)
               (set-add set!-vars v))])]
    [(list (? prim-op? op) v* ...)
     (match (convert*-union v*)
       [(list v* set!-vars)
        (list `(,op ,@v*) set!-vars)])]
    [`(if ,test, then ,else)
      (match (convert*-union (list test then else))
        [(list (list test then else) set!-vars)
         (list `(if ,test ,then ,else) set!-vars)])]
    [`(let ((,v* ,e*) ...) ,body)
      (match (convert*-union e*)
        [(list e* e*-set!-vars)
         (match (convert body)
           [(list body body-set!-vars)
            (let ([free-set!-vars-in-body 
                     ; set!-vars in body but not bound in v*
                     (set-subtract body-set!-vars
                                   (list->seteq v*))]
                  [bound-set!-vars-in-body
                    (set-intersect body-set!-vars
                                   (list->seteq v*))])
              (list 
                `(let ,(map
                         (lambda (v e)
                           (if (set-member? body-set!-vars v)
                             (list v `(vec ,e))
                             (list v e)))
                         v* e*)
                   ,(subst body bound-set!-vars-in-body))
                (set-union e*-set!-vars free-set!-vars-in-body)))])])]
    [`(begin ,exp* ...)
      (match (convert*-union exp*)
        [(list exp* exp*-set!-vars)
         (list
           `(begin ,@exp*)
           exp*-set!-vars)])]
    [`(labels ((,v* ,e*) ...) ,exp)
      ; for debuggin purpose
      e]
    [`(lambda (,v* ...) ,body)
      (match (convert body)
        [(list body body-set!-vars)
         (let ([free-set!-vars-in-body
                 (set-subtract body-set!-vars
                               (list->seteq v*))]
               [bound-set!-vars-in-body
                 (set-intersect body-set!-vars
                                (list->seteq v*))])
           (list
             `(lambda ,v*
                ,(if (set-empty? bound-set!-vars-in-body)
                   body
                   `(let ,(for/list ([v (set->list bound-set!-vars-in-body)])
                            (list v `(vec ,v)))
                      ,(subst body bound-set!-vars-in-body))))
             free-set!-vars-in-body
             ))])]
    [`(app ,rator ,rand* ...)
      (match (convert*-union (cons rator rand*))
        [(list (list rator rand* ...) set!-vars)
         (list `(app ,rator ,@rand*) set!-vars)])]
    [_ (error 'assign-conversion "~a is not matched" e)]))

(define (convert*-union exps)
  (let loop ([exps exps]
             [cvted-exps '()]
             [set!-vars (seteq)])
    (if (null? exps)
      (list (reverse cvted-exps)
            set!-vars)
      (match (convert (car exps))
        [(list cvted-exp a-set!-vars)
         (loop (cdr exps)
               (cons cvted-exp cvted-exps)
               (set-union set!-vars a-set!-vars))]))))


; for every free var in e, if var=v, subst it
; with exp.
(define (subst e vars)
  (letrec ([subst1
             (lambda (e)
               (match e
                 [(? immediate?) e]
                 [(? string?) e]
                 [(? symbol? v)
                  (if (set-member? vars v)
                    `(vec-ref ,v 0)
                    v)]
                 [`(constant-ref ,v) e]
                 [`(set! ,v ,e1)
                   (if (set-member? vars v)
                     `(vec-set! ,v 0 ,(subst1 e1))
                     e)]
                 [(list (? prim-op? op) v* ...)
                  `(,op ,@(map subst1 v*))]
                 [`(if ,test ,then ,else)
                   `(if ,(subst1 test)
                      ,(subst1 then)
                      ,(subst1 else))]
                 [`(let ((,v* ,e*) ...) ,body)
                   `(let ,(map list v* (map subst1 e*))
                      ,(subst body
                              (set-subtract vars (list->seteq v*))))]
                 [`(begin ,exp* ...)
                   `(begin ,@(map subst1 exp*))]
                 [`(labels ((,v* ,e*) ...) ,exp)
                   ; for debugging purpose
                   e]
                 [`(lambda (,v* ...) ,body)
                   `(lambda ,v*
                      ,(subst body
                              (set-subtract vars (list->seteq v*))))]
                 [`(app ,rator ,rand* ...)
                   `(app ,(subst1 rator)
                         ,@(map subst1 rand*))]
                 [_ (error 'subst "~a is not matched" e)]))])
    (subst1 e)))


(module+ test
  (pretty-write
    (car
      (convert '(let ((f (lambda (c)
                           (cons (lambda (v) (set! c v))
                                 (lambda () c)))))
                  (let ((p (app f 0)))
                    (app (car p) 12)))))))
