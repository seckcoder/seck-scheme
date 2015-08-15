#lang racket

; macro expander
(require "base.rkt")
         
(provide (rename-out [expand macro-expand]))

(define (expand x [boundvars (seteq)])
  (let ([expand-with-boundvars
          (lambda (e)
            (expand e boundvars))])
    (match x
      [(? immediate?) x]
      [(? string?) x]
      [(? symbol?) x]
      [`(quote ,v)
        x]
      [`(make-vector ,v* ...)
        (expand-with-boundvars `(make-vec ,@v*))]
      [`(vector ,v* ...)
        (expand-with-boundvars `(vec ,@v*))]
      [`(vector-set! ,v* ...)
        (expand-with-boundvars `(vec-set! ,@v*))]
      [`(vector-ref ,v* ...)
        (expand-with-boundvars `(vec-ref ,@v*))]
      [`(make-vec)
        (expand-with-boundvars `(make-vec 0))]
      [`(make-string)
        (expand-with-boundvars `(make-string 0))]
      [`(fxadd1 ,v)
        (expand-with-boundvars `(fx+ ,v 1))]
      [`(add1 ,v)
        (expand-with-boundvars `(+ ,v 1))]
      [`(fxsub1 ,v)
        (expand-with-boundvars `(fx- ,v 1))]
      [`(sub1 ,v)
        (expand-with-boundvars `(- ,v 1))]
      [`(fxzero? ,v)
        (expand-with-boundvars `(zero? ,v))]
      [`(fixnum? ,v)
        (expand-with-boundvars `(number? ,v))]
      [(list (? prim-op? op) v* ...)
       `(,op ,@(map expand-with-boundvars v*))]
      [(? let*-e?)
       (expand-with-boundvars
         (expand-let* x boundvars))]
      [(? cond-e?)
        (expand-with-boundvars
          (expand-cond x boundvars))]
      [`(if ,test ,then ,else)
        `(if ,(expand-with-boundvars test)
           ,(expand-with-boundvars then)
           ,(expand-with-boundvars else))]
      [`(let ([,v* ,e*] ...) ,body* ...)
        `(let ,(map list v* (map expand-with-boundvars e*))
           ,(let ([new-bound-vars (set-union boundvars
                                             (list->seteq v*))])
              (expand
                `(begin
                   ,@body*)
                new-bound-vars)))]
      [(? letrec-e?)
       (expand-with-boundvars
         (expand-letrec x boundvars))]
      [`(letrec* ([,v* ,e*] ...) ,body* ...)
        (expand-with-boundvars
          `(letrec ,(map list v* e*)
             ,@body*))]
      [(? and-e?)
       (expand-with-boundvars
         (expand-and x boundvars))]
      [(? or-e?)
       (expand-with-boundvars
         (expand-or x boundvars))]
      [(? when-e?)
       (expand-with-boundvars
         (expand-when x boundvars))]
      [(? unless-e?)
        (expand-with-boundvars
          (expand-unless x boundvars))]
      [`(lambda (,v* ...) ,body* ...)
        `(lambda ,v*
           ,(let ([new-bound-vars (set-union boundvars (list->seteq v*))])
              (expand
                `(begin
                   ,@body*)
                new-bound-vars
                )))]
      [`(begin ,exp0 ,exp* ...)
        (if (null? exp*)
          (expand-with-boundvars exp0)
          `(begin
             ,@(map expand-with-boundvars (cons exp0 exp*))))]
      [`(app-proc ,rator ,rand* ...)
        ; procedure call, for debug purpose
        x]
      [`(labels ((,v* ,e*) ...) ,exp)
        ; again, for debug purpose
        x]
      [`(proc (,v* ...) ,body)
        ; procedure, for debug purpose
        x]
      [`(app ,rator ,rand* ...)
        (expand-with-boundvars
          `(,rator ,@rand*))]
      [`(,rator ,rand* ...)
        ; function call
        `(app ,(expand-with-boundvars rator)
              ,@(map expand-with-boundvars rand*))]
      [_ (error 'expand "failed:~s" x)])))


(define (let*-e? e)
  (eq? (car e) 'let*))
(define (expand-let* let-e boundvars)
  (match let-e
    [`(let* () ,body* ...)
      `(let () ,@body*)]
    [`(let* ([,v0 ,e0]) ,body* ...)
      `(let ([,v0 ,e0])
         ,@body*)]
    [`(let* ([,v0 ,e0] ,bind* ...) ,body* ...)
      `(let ([,v0 ,e0])
         ,(expand-let*
            `(let* ,bind* ,@body*)
            boundvars))]))

(define (cond-e? e)
  (eq? (car e) 'cond))
(define (expand-cond cond-e boundvars)
  (letrec ([ctx0 (lambda () '(void))]
           [not-eq-else? (lambda (e) (not (eq? e 'else)))]
           [not-eq-=>? (lambda (e) (not (eq? e '=>)))]
           [expand-cond-clause
             (lambda (clause boundvars ctx)
               (define not-bound?
                 (lambda (v)
                   (not (set-member? boundvars v))))
               (match clause
                 [`(else)
                   #:when (not-bound? 'else)
                   (error 'cond "missing expressions in `else` clause")]
                 [`(else ,then* ...)
                   #:when (not-bound? 'else)
                   (if (eq? ctx ctx0)
                     `(begin
                        ,@then*)
                     (error 'cond "else should appear in the last cond clause"))]
                 [`(,test => ,proc)
                   #:when (not-bound? '=>)
                   (let ([test-v (gensym)])
                     `(let ([,test-v ,test])
                        (if ,test-v
                          (,proc ,test-v)
                          ,(ctx))))]
                 [`(,test)
                   (let ([test-v (gensym)])
                     `(let ([,test-v ,test])
                        (if ,test-v
                          ,test-v
                          ,(ctx))))]
                 [`(,test ,then* ...)
                   `(if ,test
                      (begin
                        ,@then*)
                      ,(ctx))]
                 [_ (error 'cond "clause: ~a is not matched" clause)]))]
           [expand-cond-clauses
             (lambda (clauses boundvars)
               (match clauses
                 [(list) (ctx0)]
                 [(list clause0)
                  (expand-cond-clause
                    clause0
                    boundvars
                    ctx0)]
                 [(list clause0 clause* ...)
                  (expand-cond-clause
                    clause0
                    boundvars
                    (lambda ()
                      (expand-cond-clauses clause* boundvars)))]
                 [_ (error 'cond "~a is not matched" cond-e)]))])
    (match cond-e
      [`(cond ,clauses* ...)
        (expand-cond-clauses clauses* boundvars)]
      [_ (error 'cond "~a is not matched" cond-e)])))


(define (letrec-e? e)
  (eq? (car e) 'letrec))
(define (expand-letrec e boundvars)
  (match e
    [`(letrec ([,v* ,e*] ...) ,body* ...)
    `(let ,(map (lambda (v)
                  (list v #f)) v*)
       (begin
         ,@(map (lambda (v e)
                  `(set! ,v ,e))
                v* e*)
         ,@body*))]
    [_ (error 'letrec "~a is not matched" e)]
    ))

(define (and-e? e)
  (eq? (car e) 'and))
(define (expand-and e boundvars)
  (match e
    [`(and) #t] 
    [`(and ,v0) v0]
    [`(and ,v0 ,v* ...)
      (let ([v0-uniq (gensym)])
        ; we don't need to add v0-uniq to boundvars!!!
        `(let ([,v0-uniq ,v0])
           (if ,v0-uniq
             (and ,@v*)
             ,v0-uniq)))]
    [_ (error 'and "~a is not matched" e)]
    ))

(define (or-e? e)
  (eq? (car e) 'or))
(define (expand-or e boundvars)
  (match e
    [`(or) #f]
    [`(or ,v0) v0]
    [`(or ,v0 ,v* ...)
      (let ([v0-uniq (gensym)])
        `(let ([,v0-uniq ,v0])
           (if ,v0-uniq
             ,v0-uniq
             (or ,@v*))))]
    [_ (error 'or "~a is not matched" e)]
    ))

(define (when-e? e)
  (eq? (car e) 'when))
(define (expand-when e boundvars)
  (match e
    [`(when ,test ,then* ...)
      `(if ,test
         (begin
           ,@then*)
         (void))]
    [_ (error 'when "~a is not matched" e)]))

(define (unless-e? e)
  (eq? (car e) 'unless))

(define (expand-unless e boundvars)
  (match e
    [`(unless ,test ,else* ...)
      `(if ,test
         (void)
         (begin
           ,@else*))]
    [_ (error 'unless "~a is not matched" e)]))

(module+ test
  (expand '(let ([f (lambda (x) 
                     (set! x (fxadd1 x))
                     x)])
            (f 12)))
  (expand '(lambda (x)
            (set! x (fxadd1 x))
            x))
  (expand '(cond [1] [else 13]))
  (expand '(cond [(cons 1 2) => (lambda (x) (cdr x))]))
  (expand '(let ([=> 12])
             (cond
               [=> => =>])))
  )
