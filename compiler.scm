(load "tests-driver.scm")
#|(load "tests-1.1-req.scm")
(load "tests-1.2-req.scm")|#

(require racket/match
         (prefix-in env: "env.rkt")
         "../../base/utils.rkt"
         "base.rkt"
         "macro-expand.rkt"
         "constants.rkt"
         "closure-conversion.rkt"
         "alpha-conversion.rkt"
         "assign.rkt"
         "global.rkt")

(define (clj-cvt e)
  (closure-conversion e 'bottom-up))
(define (assign-cvt e)
  (car (assign-conversion e)))
(define (alpha-cvt e)
  ((alpha-conversion (env:empty)) e))
(define (compile-program x)
  (init-global!)
  (~> x
      macro-expand
      lift-constant
      ;alpha-cvt
      assign-cvt
      clj-cvt
      emit-program))

(define (emit-global-proc n code)
  (match code
    [`(proc (,v* ...) ,body)
      (emit-fn-header n)
      (let* ([v*-num (length v*)]
             [env (env:init v*
                            (range (- wordsize)
                                   (- wordsize)
                                   (- (* v*-num wordsize))))])
        ((emit-exp (- (- (* v*-num wordsize))
                      wordsize)
                   env
                   #t) body)
        (emit "   ret")
        )]
    [_ (void)]))

(define (emit-global-constant n code)
  (match code
    [`(datum ,v)
      (emit "# global constant")
      (emit "   .local ~a" n)
      (emit "   .comm ~a,~a,~a" n wordsize wordsize)
      ((emit-exp (- wordsize)
                 (env:empty)
                 #t) v)
      (emit-save-eax-to-global)
      (emit "   movl %eax, ~a # move global constant pointer value" n)]
    [_ (void)]
    ))

(define (emit-save-eax-to-global)
  ; save eax register value to global memory,
  ; so that the value pointed by it can be handled by garbage collection
  (emit "   movl ~a(%ebp), %ecx # fetch global pointer" global-offset)
  (emit "   movl %eax, (%ecx) # save eax to global memory")
  (emit "   addl $~a, ~a(%ebp) # increase global pointer" wordsize global-offset)
  (emit "   movl %ecx, %eax # return global pointer as result"))

(define (emit-load-global-to-eax v)
  (emit "   movl ~a, %eax" v))

(define (global-proc? code)
  (match code
    [`(proc (,v* ...) ,body)
      #t]
    [_ #f]))

(define (global-constant? code)
  (match code
    [`(datum ,v)
      #t]
    [_ #f]))

(define (emit-global-proc*)
  (for-each
    (match-lambda
      [(list n (? global-proc? code))
       (emit-global-proc n code)]
      [_ (void)])
    *global*))

(define (emit-global-constant*)
  (for-each
    (match-lambda
      [(list n (? global-constant? code))
       (emit-global-constant n code)]
      [_ (void)])
    *global*))
; lift lambda
(define-syntax def-lifted-lambda
  (syntax-rules ()
    [(_ (lambda (v* ...) body))
     (env!:ext! *proc*
                (gensym 'proc)
                `(proc (v* ...) body))]))

(define (emit-program x)
  (print x)(newline)
  (emit "   .text")
  ; We need L_scheme_entry since we need to make sure that when starting
  ; to emit-exp, the value above %esp is a return address. Otherwise,
  ; the tail-optimization will not work.
  (emit-fn-header 'L_scheme_entry)
  (emit-global-constant*)
  ((emit-exp (- wordsize) (env:empty) #t) x)
  (emit "   ret # ret from L_scheme_entry") ; if program is tail-optimized, ret will be ignored
  (emit-fn-header 'scheme_entry)
  (emit-preserve-regs)
  (emit "   movl %esp, %ecx # store esp temporarily")
  ; heap : low->high
  ; stack : high->low
  (emit "   movl 12(%ecx), %ebp # set mem pointer")
  (emit "   movl 8(%ecx), %esp # set stack pointer")
  (emit "   pushl 4(%ecx) # store ctx")
  ; It's an assumption that physical addresses on Intel's
  ; 32bit processors have 8-byte boundaries. So we don't
  ; need to aligh the heap address when start.
  ;(emit-align-heap) ; aligh the start address of heap
  (emit "   call L_scheme_entry")
  (emit-restore-regs)
  (emit "   ret # return from scheme_entry")
  (emit-global-proc*)
  )
(define (emit-preserve-regs)
  (define (si-of-i i)
    (* wordsize i))
  (emit "   movl 4(%esp), %ecx") ; ctx ptr
  (let loop ([regs registers]
             [i 0])
    (cond [(null? regs)
           'ok]
          [(scratch? (car regs))
           (loop (cdr regs)
                 (add1 i))]
          [else
            (match (car regs)
              [(cons n _)
               (emit "   movl %~a, ~a(%ecx)" n (si-of-i i))
               (loop (cdr regs)
                     (add1 i))])])))


(define (emit-restore-regs)
  (define (si-of-i i)
    (* wordsize i))
  (emit "   popl %ecx") ; get ctx ptr
  (let loop ([regs registers]
             [i 0])
    (cond [(null? regs)
           'ok]
          [(scratch? (car regs))
           (loop (cdr regs)
                 (add1 i))]
          [else
            (match (car regs)
              [(cons n _)
               (emit "   movl ~a(%ecx), %~a" (si-of-i i) n)
               (loop (cdr regs)
                     (add1 i))])])))

(define (emit-fn-header lbl)
  (emit "   .globl ~a" lbl)
  (emit "   .type ~a, @function" lbl)
  (emit "~a:" lbl))
(define-syntax gen-pairs
  (syntax-rules ()
    [(_)
     (list)]
    [(_ (op0 p* ...) pair* ...)
     (cons
       (cons `op0
             (lambda ()
               p* ...))
       (gen-pairs pair* ...))]))
(define-syntax biop-emit-pairs
  (syntax-rules ()
    [(_ p0 p* ...)
     (make-hasheq
       (gen-pairs p0 p* ...))]))

(define emit-exp
  (lambda (si env tail?)
    (define (emit-unop op v)
      ((emit-exp si env #f) v)
      (match op
        ['number->char
         ; shift left
         (emit "   shll $~s, %eax" (- charshift fxshift))
         ; change the shifted to char tag
         (emit "   orl $~s, %eax" chartag)]
        ['char->number
         (emit "   sarl $~s, %eax" (- charshift fxshift))]
        ['number?
         (emit "   andb $~s, %al" fxmask)
         (emit "   cmpb $~s, %al" fxtag)
         (emit "   sete %al")
         (emit-eax-0/1->bool)]
        ['null?
         (emit "   cmpw $~s, %ax" null_v)
         (emit "   sete %al")
         (emit-eax-0/1->bool)]
        ['boolean?
         (emit "   andb $~s, %al" boolmask)
         (emit "   cmpb $~s, %al" booltag)
         (emit "   sete %al")
         (emit-eax-0/1->bool)]
        ['char?
         (emit "   andb $~s, %al" charmask)
         (emit "   cmpb $~s, %al" chartag)
         (emit "   sete %al")
         (emit-eax-0/1->bool)]
        ['not
         (emit "   cmpw $~s, %ax" bool-f)
         ; if equal=#f, we set al to 1, then we transform it to #t.
         ; if equal to other value, we set al to 0, then transformed to #f.
         (emit "   sete %al")
         (emit-eax-0/1->bool)]
        ['zero?
         (emit "   cmpl $~s, %eax" (immediate-rep 0))
         (emit "   sete %al")
         (emit-eax-0/1->bool)]
        ['car
         ; It's -1, because tag of pair is 0x1, so the tagger pointer
         ; is just one byte bigger
         (emit "   movl -1(%eax), %eax # car of pair. Think about -1!")]
        ['cdr
         (emit "   movl ~s(%eax), %eax # cdr of pair. Think about (sub1 wordsize)" (sub1 wordsize))]
        ['pair?
         (emit "   andb $~s, %al" pairmask)
         (emit "   cmpb $~s, %al" pairtag)
         (emit "   sete %al")
         (emit-eax-0/1->bool)]
        ['string-length
         (emit-remove-strtag 'eax)
         (emit "   movl (%eax), %eax # move string length to eax")]
        ['vector?
         (emit "   andb $~s, %al" vecmask)
         (emit "   cmpb $~s, %al" vectag)
         (emit "   sete %al")
         (emit-eax-0/1->bool)]
        ['vector-length
         (emit-remove-vectag 'eax)
         (emit "   movl (%eax), %eax # move vec length to eax")]
        ['string?
         (emit "   andb $~s, %al" strmask)
         (emit "   cmpb $~s, %al" strtag)
         (emit "   sete %al")
         (emit-eax-0/1->bool)]
        ['fixnum->char
         (emit-remove-fxtag 'eax)
         (emit-add-chartag 'eax)]
        ['char->fixnum
         (emit-remove-chartag 'eax)
         (emit-add-fxtag 'eax)]
        ['procedure?
         (emit "   andb $~s, %al" cljmask)
         (emit "   cmpb $~s, %al" cljtag)
         (emit "   sete %al")
         (emit-eax-0/1->bool)]
        [_ (error 'emit-unop "~a is not an unary operator" op)]))
    (define (emit-biop op a b)
      (define (emit-biv)
        (emit-exps-leave-last si env (list a b)))
      (define (emit-*)
        (emit-biv)
        (emit "   movl ~s(%esp), %ecx # get a" si)
        (emit-remove-fxtag 'ecx)
        (emit-remove-fxtag 'eax)
        (emit "   imull %ecx, %eax # a * b")
        (emit-add-fxtag 'eax))
    (define (emit-cmp op)
      (emit-biv)
      (emit "   cmpl ~s(%esp), %eax" si)
      (case op
        ['= (emit "   sete %al")]
        ['< (emit "   setg %al")]
        ['<= (emit "  setge %al")]
        ['> (emit "   setl %al")]
        ['>= (emit "  setle %al")]
        ['char= (emit "   sete %al")]
        [else (report-not-found)])
      (emit-eax-0/1->bool))
    (define op->emitter
      (biop-emit-pairs
        [* (emit-*)]
        [fx* (emit-exp1 `(* ,a ,b))]
        [+ (emit-biv)
           ; b = a + b
           (emit "   addl ~s(%esp), %eax" si)]
        [fx+ (emit-exp1 `(+ ,a ,b))]
        [- (emit-biv) 
           ; b = a - b
           (emit "   movl %eax, %ecx")
           (emit "   movl ~s(%esp), %eax" si)
           (emit "   subl %ecx, %eax")]
        [quotient
          (emit-divide (emit-exps-push-all si env (list a b)))]
        [remainder
          (emit-divide (emit-exps-push-all si env (list a b)))
          (emit "   movl %edx, %eax")]
        [fx- (emit-exp1 `(- ,a ,b))]
        [= (emit-cmp '=)]
        [fx= (emit-exp1 `(= ,a ,b))]
        [< (emit-cmp '<)]
        [fx< (emit-exp1 `(< ,a ,b))]
        [<= (emit-cmp '<=)]
        [fx<= (emit-exp1 `(<= ,a ,b))]
        [> (emit-cmp '>)]
        [fx> (emit-exp1 `(> ,a ,b))]
        [>= (emit-cmp '>=)]
        [fx>= (emit-exp1 `(>= ,a ,b))]
        [char= (emit-cmp 'char=)]
        [cons (emit-cons a b)]
        [eq?
          ; simple treatment
          (emit-exp1 `(= ,a ,b))]
        [set-car!
          (emit-set-car! a b)]
        [set-cdr!
          (emit-set-cdr! a b)]
        ))
    (define (report-not-found)
      (error 'emit-biop "~a is not a binary operator" op))
    ;(printf "emit-op ~a ~a\n" op (hash-ref op->emitter op))
    ((hash-ref op->emitter op report-not-found)))
  (define (emit-rands rands)
    (let ([new-si (emit-exps-push-all si env rands)])
      ; swap the rands order
      ; rand-0 : si(%esp)
      ; rand-n : (+ new-si wordsize)(%esp)
      ;(printf "~a ~a ~a\n" si new-si (length rands))
      (unless (= (+ new-si
                    (* (length rands) wordsize))
                 si)
        ; for debuggin purpose
        (error 'emit-rands "some error happened"))
      (emit-swap-rands-range si new-si)
      new-si))
  (define (emit-make-vec n)
    ((emit-exp si env #f) n)
    (emit "   movl %eax, ~s(%esp) #store vec length to stack" si)
    (emit-remove-fxtag 'eax) ; %eax store length
    (emit-calc-vector-size)
    (emit-alloc-heap (- si wordsize) #t) ; vec length on stack
    (emit-stack->heap si 0)
    (emit-add-vectag 'eax))
  (define (emit-vec-ref v i)
    ; TODO: out-of-bounds check?
    (emit-exps-leave-last si env (list v i))
    ; remove i's flag
    (emit-remove-fxtag 'eax)
    (emit "   movl ~s(%esp), %ecx # transfer vec to ecx" si) ; v
    (emit-remove-vectag 'ecx)
    ; %ecx + %eax*wordsize + wordsize. extra wordsize is for vector length
    (emit "   movl ~s(%ecx, %eax, ~s), %eax # get ith(in eax) value of vec" wordsize wordsize)
    )
  (define (emit-vec-set! v i val)
    (emit-exps-leave-last si env (list v i val))
    (emit "   movl ~s(%esp), %ecx" si) ; v
    (emit-remove-vectag 'ecx)
    (emit "   movl ~s(%esp), %edx" (- si wordsize)) ; i
    ; i should be fxnum, we should remove its flags
    (emit-remove-fxtag 'edx)
    ; %ecx + %edx*wordsize + wordsize. extra wordsize is for vector length
    (emit "   movl %eax, ~s(%ecx, %edx, ~s)" wordsize wordsize)
    )
  (define (emit-vec-from-values vs)
    ; why we shouldn't evaluate one value and move it to heap?
    ; Since when evaluate the value, it may change ebp
    (emit "# emit-vec-from-values")
    (let* ([len (length vs)]
           [new-si (emit-exps-push-all si env vs)])
      (emit-alloc-heap new-si
                       (* (add1 len) wordsize))
      (emit "   movl $~s, (%eax) # move length to vector"
            (immediate-rep len))
      (let loop ([i 0])
        (unless (>= i len)
          ; move the ith item from stack to heap
          (emit-stack->heap (- si (* i wordsize))
                            (* (add1 i) wordsize))
          (loop (add1 i))))
      (emit-add-vectag 'eax)
      )
    (emit "# emit-vec-from-values end")
    )
  (define (emit-make-string n)
    ((emit-exp si env #f) n)
    (emit "   movl %eax, ~s(%esp) # store str len" si)
    (emit-remove-fxtag 'eax)
    (emit-calc-string-size)
    (emit-alloc-heap (- si wordsize) #t)
    (emit-stack->heap si 0) ; move length to heap
    (emit-add-strtag 'eax))
  (define (emit-string-ref s i)
    (emit-exps-leave-last si env (list s i))
    (emit-remove-fxtag 'eax)
    (emit "   movl ~s(%esp), %ecx # str pointer" si)
    (emit-remove-strtag 'ecx)
    (emit "   movl $0, %edx # reset edx")
    (emit "   movb ~s(%ecx, %eax), %dl # ith value in str" wordsize)
    (emit "   movl %edx, %eax")
    (emit-add-chartag 'eax)
    )
  (define (emit-string-set s i c)
    (emit-exps-leave-last si env (list s i c))
    (emit "   movl ~s(%esp), %ecx # get string pointer" si) ; s
    (emit "   movl ~s(%esp), %edx # get idx:i" (- si wordsize)) ; i
    (emit-remove-chartag 'eax)
    (emit-remove-strtag 'ecx)
    (emit-remove-fxtag 'edx)
    (emit "   movb %al, ~a(%ecx, %edx) # string-set!" wordsize)
    )
  (define (emit-string-from-values cs)
    (let* ([len (length cs)]
           [new-si (emit-exps-push-all si env cs)])
      ;(printf "~a ~a ~a\n" len si new-si)
      (emit-alloc-heap new-si
                       (+ wordsize len))
      (emit "   movl $~s, (%eax) # move len to str"
            (immediate-rep len))

      (for ([i (in-range 0 len)])
        (emit-stack->heap-by-char (- si (* i wordsize))
                                  (+ wordsize i)))
      (emit-add-strtag 'eax)))
  (define (emit-make-symbol s)
    ; make symbol from str
    s
    )
  (define (emit-void)
    (emit "   movl $~s, %eax" void-v))
  (define (emit-constant-ref v)
    (emit-load-global-to-eax v)
    (emit "   movl (%eax), %eax # constant-ref"))
  (define (emit-cons a d)
    (let ([new-si (emit-exps-push-all si env (list a d))])
      (emit-alloc-heap new-si
                       (* 2 wordsize))
      (emit-stack->heap si 0) ; move a to heap
      (emit-stack->heap (- si wordsize) wordsize) ; move d to heap
      (emit-add-pairtag 'eax)))
  (define (emit-set-car! pair v)
    (emit-exps-leave-last si env (list pair v))
    (emit "   movl ~s(%esp), %ecx # get pair ptr" si)
    (emit-remove-pairtag 'ecx)
    (emit "   movl %eax, (%ecx) # set car"))
  (define (emit-set-cdr! pair v)
    (emit-exps-leave-last si env (list pair v))
    (emit "   movl ~s(%esp), %ecx # get pair ptr" si)
    (emit-remove-pairtag 'ecx)
    (emit "   movl %eax, ~a(%ecx) # set cdr" wordsize))
  (define (emit-closure f rv)
    (emit "# emit-closure")
    ((emit-exp si env #f) rv)
    (let ([new-si (emit-eax->stack si)])
      (emit-alloc-heap new-si (* 2 wordsize)) ; store label and fvs env
      (emit "   leal ~s, %ecx # get address of closure label" f)
      ;(emit-add-fxtag 'ecx) ; we represent address as fxnum
      (emit "   movl %ecx, (%eax) # move label address to heap")
      (emit-stack->heap si wordsize) ; move fvs env to heap
      (emit-add-cljtag 'eax)
      (emit "# emit-closure end")))
  (define (emit-app rator rands)
    (emit "# emit-app")
    ;(printf "emit-app: is tail call? ~a" tail?)
    (if tail?
      (begin
      (emit "# tail-optimization")
      (let ([new-si
              (emit-exps-push-all (- si wordsize)
                                  env
                                  rands)])
        ((emit-exp new-si env #f) rator)
        (emit-remove-cljtag 'eax)
        (emit "   movl (%eax), %ecx # move label to stack")
        (emit "   movl %ecx, ~s(%esp)" new-si)
        (emit "   movl ~s(%eax), %edx # move clojure env to stack" wordsize)
        (emit "   movl %edx, ~s(%esp)" si)
        (emit-stack-move-range
          si
          (- wordsize)
          (+ new-si wordsize)
          (- wordsize))
        (emit "   jmp *~s(%esp) # tail jump" new-si)
        ))
      (begin
        (emit "#no tail optmization")
        (let ([new-si
                (emit-exps-push-all (- si (* 2 wordsize))
                                    env
                                    rands)])
          ((emit-exp new-si env #f) rator) ; get closure
          (emit-remove-cljtag 'eax)
          (emit "   movl (%eax), %ecx") ; move label to ecx
          (emit "   movl ~s(%eax), %edx" wordsize) ; movel clojure env to stack
          (emit "   movl %edx, ~s(%esp)" (- si wordsize))
          (emit "   addl $~s, %esp" (+ si wordsize))
          (emit "   call *%ecx")
          (emit "   subl $~s, %esp" (+ si wordsize)))))
    (emit "# emit-app end"))
  (define (emit-let vs es body)
    (match (emit-decls si env #f vs es)
      [(list si env)
       ((emit-exp si env tail?)
        body)]))
  (define (emit-char-seq s reg start)
    ; chars to heap, reg as pointer to heap,
    ; start is the si of heap pointer
    (for ([c (in-string s)]
          [i (in-naturals start)])
      (emit "   movb $~a, ~a(%eax)"
            (char->integer c)
            i)
      ))
  (define (emit-string s)
    (emit-alloc-heap si (+ wordsize (string-length s)))
    (emit "   movl $~a, (%eax) # str len" (immediate-rep (string-length s)))
    (emit-char-seq s 'eax wordsize)
    (emit-add-strtag 'eax))
  ; ## emit-exp1 ##
  (define emit-exp1
    (lambda (exp)
      (match exp
        [(? immediate? x)
         (emit "   movl $~s, %eax # emit immediate:~a" (immediate-rep x) x)]
        [(? string? s)
         (emit-string s)]
        [(? symbol? v)
         ; variable
         (let ([pos (env:app env v)])
           ;(printf "emit-var: ~a  ~a\n" v pos)
           (emit "   movl ~s(%esp), %eax # var:~s" pos v))]
        [`(make-vec ,n)
          (emit-make-vec n)]
        [`(vec-ref ,v ,i)
          (emit-vec-ref v i)]
        [`(vec-set! ,v ,i ,val)
          (emit-vec-set! v i val)]
        [`(vec ,v* ...)
          (emit-vec-from-values v*)]
        [`(make-string ,n)
          (emit-make-string n)]
        [`(string-ref ,s ,i)
          (emit-string-ref s i)]
        [`(string-set! ,s ,i ,c)
          (emit-string-set s i c)]
        [`(string ,c* ...)
          (emit-string-from-values c*)]
        [`(make-symbol ,s)
          (emit-make-symbol s)]
        [`(constant-ref ,v)
          (emit-constant-ref v)]
        [`(void ,v* ...)
          (emit-void)]
        [(list (? unop? op) v)
         (emit-unop op v)]
        [(list (? biop? op) a b)
         (emit-biop op a b)]
        [`(if ,test ,then ,else)
          ((emit-exp si env #f) test)
          (let ((else-lbl (gen-label))
                (endif-lbl (gen-label)))
            ; jump to else if equal to false
            ; Que: how to optimize this?
            (emit "   cmpl $~s, %eax" bool-f)
            (emit "   je ~a" else-lbl)
            (emit-exp1 then)
            (emit "   jmp ~s" endif-lbl)
            (emit "~s:" else-lbl)
            (emit-exp1 else)
            (emit "~s:" endif-lbl))]
        [`(let ((,v* ,e*) ...) ,body)
          (emit-let v* e* body)]
        [`(lambda (,v* ...) ,body)
          (error 'emit-exp "lambda should be converted to procedure")]
        [`(begin ,exp* ...)
          (let loop ([exps exp*])
            (cond
              [(null? exps)
               (error 'begin "empty body")]
              [(null? (cdr exps))
               (printf "~a\n" (car exps))
               (emit-exp1 (car exps))]
              [else
                ((emit-exp si env #f) (car exps))
                (loop (cdr exps))]))]
        [`(labels ([,f* ,proc*] ...) ,exp)
          (for-each
            (lambda (f proc)
              (add-global! f proc))
            f*
            proc*)
          (emit-exp1 exp)]
        [`(closure ,f ,rv)
          (emit-closure f rv)]
        [`(app ,rator ,rand* ...)
          (emit-app rator rand*)]
        [_ (error 'emit-exp "~a not matched" exp)]
        )))
  emit-exp1))

; eval(e) could be an address or label
(define (emit-decl si env v e)
  ((emit-exp si env) e)
  (emit "   movl %eax, ~s(%esp)" si)
  (list (- si wordsize)
        (env:ext env v si)))

; for let
(define (emit-decls si env tail? vs es)
  (let loop [(si si)
             (cur-vs vs)
             (cur-es es)
             (si-acc '())]
    (cond
      [(and (null? cur-vs)
            (null? cur-es))
       (list si
             (env:exts env vs (reverse si-acc)))]
      [(or (null? cur-vs)
           (null? cur-es))
       (error 'emit-decls "vs and es have different length")]
      [else
        ((emit-exp si env tail?) (car cur-es))
        (emit "   movl %eax, ~s(%esp) # move declared value to stack" si)
        (loop (- si wordsize)
              (cdr cur-vs)
              (cdr cur-es)
              (cons si si-acc))])))

; for let*
(define (emit-decl* si env vs es)
  (foldl
    (match-lambda*
      [(list v e (list si env))
       (emit-decl si env v e)])
    (list si env)
    vs
    es))

(define (emit-remove-fxtag reg)
  (emit "   sar $~s, %~a # remove fx tag" fxshift reg))
(define (emit-add-fxtag reg)
  ; sign extension
  (emit "   sal $~s, %~a # add fxtag" fxshift reg))
(define (emit-remove-cljtag reg)
  (emit "   subl $~s, %~a # remove cljtag" cljtag reg))
(define (emit-add-cljtag reg)
  ; we don't need to shift
  (emit "   orl $~s, %~a # add cljtag" cljtag reg))
(define (emit-remove-vectag reg)
  (emit "   subl $~s, %~a" vectag reg))
(define (emit-add-vectag reg)
  (emit "   orl $~s, %~a # add vectag" vectag reg))
(define (emit-add-pairtag reg)
  (emit "   orl $~s, %~a # add pairtag" pairtag reg))
(define (emit-remove-pairtag reg)
  (emit "   subl $~s, %~a # remove pairtag" pairtag reg))
(define (emit-add-strtag reg)
  (emit "   orl $~s, %~a # add strtag" strtag reg))
(define (emit-remove-strtag reg)
  (emit "   subl $~s, %~a # remove strtag" strtag reg))
(define (emit-add-chartag reg)
  (emit "   shl $~s, %~a # add chartag" charshift reg)
  (emit "   orl $~s, %~a" chartag reg)
  )
(define (emit-remove-chartag reg)
  (emit "   shr $~s, %~a # remove chartag" charshift reg))

(define gen-label
  (let ([count 0])
    (lambda ()
      (let ([L (format "L_~s" count)])
        (set! count (add1 count))
        (string->symbol L)))))

; after cmp operation, we can set eax to bool value according
; to the flags
(define (emit-eax-0/1->bool)
  (emit "   movzbl %al, %eax") ; movzbl set eax high-order 24bits to zero
  (emit "   sal $~s, %al" boolshift)  ; transform the result to bool
  (emit "   or $~s, %al" bool-f))

; move eax to stack; return next si
(define (emit-eax->stack si)
  (emit "   movl %eax, ~s(%esp) # save eax value to stack" si)
  (- si wordsize))

; move ah to stack; return next si
; this is used for move characters
(define (emit-al->stack si)
  (emit "   movb %al, ~s(%esp) # save ah to stack" si)
  (- si 1))

; return si
(define (emit-exps-leave-last si env exps)
  ; emit multi exps, leave the last in %eax)
  (cond
    [(null? exps)
     (error 'emit-exps-leav-last "need at least one exp")]
    [(null? (cdr exps))
     ((emit-exp si env #f) (car exps))
     si]
    [else
      (let-values ([(first rest)
                    (split-at-right exps 1)])
        (let ([si (emit-exps-push-all si env first)])
          (emit-exps-leave-last si env rest)
          si))]))

(define (emit-exps-push-all si env exps [tail? #f])
  ; emit mutli exps, all pushed to stack
  (emit "# emit-exps-push-all")
  (let ([new-si
          (foldl
            (match-lambda*
              [(list exp si)
               ((emit-exp si env tail?) exp)
               (emit-eax->stack si)])
            si
            exps)])
    (emit "# emit-exps-push-all end")
    new-si)
  )

(define (emit-exps-push-all-by-char si env exps [tail? #f])
  (emit "# emit-exps-push-all-by-char")
  (let ([new-si
          (foldl
            (match-lambda*
              [(list exp si)
               ((emit-exp si env tail?) exp)
               (emit-remove-chartag 'eax)
               (emit-al->stack si)])
            si
            exps)])
    (emit "# emit-exps-push-all-by-char end")
    (align new-si wordsize)))
; swap i(%base) j(%base)
(define (emit-swap i j [base 'esp])
  (emit "   movl ~s(%~s), %ecx" i base)
  (emit "   movl ~s(%~s), %edx" j base)
  (emit "   movl %ecx, ~s(%~s)" j base)
  (emit "   movl %edx, ~s(%~s)" i base))

; swap pos in [start end]
(define (emit-swap-range start end [step wordsize] [base 'esp])
  (emit "# emit-swap-range")
  (let* ([l (range-len start step end)]
         [l-1 (sub1 l)]
         [range-get (range-of-i start step end)]
         [n (quotient l 2)])
    (let loop ([i 0]
               )
      (cond
        [(>= i n) (void)]
        [else
          #|(printf "~a ~a\n"
                  (range-get i)
                  (range-get (- l-1 i)))|#
          (emit-swap (range-get i)
                     (range-get (- l-1 i))
                     base)
          (loop (add1 i))])))
  (emit "# emit-swap-range end")
  )

(define (emit-swap-rands-range si new-si)
  ; swap (+ new-si wordsize)(%esp) -> si(%esp)
  ;(printf "emit-swap-rands-range ~a ~a\n" si new-si)
  (emit-swap-range (+ new-si wordsize)
                   si))

(define (emit-stack->heap stack-pos heap-pos)
  ; heap ptr stored in eax
  (emit "   movl ~s(%esp), %ecx # move stack value to heap" stack-pos)
  (emit "   movl %ecx, ~s(%eax)" heap-pos))

(define (emit-stack->heap-by-char stack-pos heap-pos)
  (emit "   movl ~s(%esp), %ecx # move stack char value to ecx" stack-pos)
  (emit-remove-chartag 'ecx)
  (emit "   movb %cl, ~s(%eax) # move char to heap" heap-pos))

; move [start ... end] to [to_start ...]
(define (emit-stack-move-range start step end to_start)
  (emit "# emit-stack-move-range ~a ~a ~a ~a"
        start step end to_start)
  (let ([cmp (range-param-check start step end)])
    (let loop ([from start]
               [to to_start])
      (cond
        [(cmp from end) (void)]
        [else
          ;(printf "move ~a to ~a\n" from end)
          (emit-swap from to)
          (loop (+ from step)
                (+ to step))])))
  (emit "# emit-stack-move-range end")
  )

(define (emit-divide si)
  ; value stores on stack
  (emit "   movl ~s(%esp), %edx # dividend to [edx,eax]"
        (+ si (* 2 wordsize)))
  (emit-remove-fxtag 'edx)
  (emit "   movl %edx, %eax")
  (emit "   sarl $~s, %edx # set edx as 0 or all 1" (sub1 (* 8 wordsize)))
  (emit "   movl ~s(%esp), %ecx #divisor" (+ si wordsize))
  (emit-remove-fxtag 'ecx)
  (emit "   idivl %ecx")
  (emit-add-fxtag 'eax)
  (emit-add-fxtag 'edx)
  ;(emit "   movl ~s(%esp), %eax" (+ si (* 1 wordsize)))
  )

(define (emit-calc-vector-size)
  ; length in %eax
  (emit "   addl $1, %eax # calculate vector size with %eax store length")
  (emit "   imull $~s, %eax" wordsize))

(define (emit-calc-string-size)
  ; length in %eax
  (emit "   addl $~a, %eax # calc str byte size" wordsize))

(define (emit-foreign-call si funcname)
  (emit "   addl $~s, %esp" (+ si wordsize))
  (emit "   call ~a" funcname)
  (emit "   subl $~s, %esp" (+ si wordsize)))

(define (emit-alloc-heap1 si)
  ; heap-aligned size in eax
  ; heap_alloc(mem, stack, size)
  (emit "   movl %eax, ~s(%esp) # size to stack" si)
  (emit "   leal ~s(%esp), %ecx # stack base pointer" (+ si wordsize))
  (emit "   movl %ecx, ~s(%esp)" (- si wordsize))
  (emit "   movl %ebp, ~s(%esp) # mem to stack" (- si (* 2 wordsize)))
  (emit-foreign-call (- si (* 3 wordsize))
                     'heap_alloc)
  ;(emit "   movl ~s(%esp), %ebp # recover mem ptr" (- si (* 2 wordsize)))
  )

(define emit-alloc-heap
  (match-lambda*
    [(list si (? boolean? align?))
     ; %eax store the size,
     (when align?
       (emit "   addl $~s, %eax # heap align calculation" (sub1 heap-align))
       (emit "   andl $~s, %eax" (- heap-align)))
     (emit-alloc-heap1 si)]
    [(list si (? number? size))
     ; size is the num of bytes to allocate
     (let ([aligned-size (align-heap-size size)])
       (emit "   movl $~s, %eax # aligned size to eax" aligned-size)
       (emit-alloc-heap1 si))]))

#|(load "tests-1.3-req1.scm")
(load "tests-1.4-req.scm")
(load "tests-1.6-req.scm")
(load "tests-1.6-opt.scm")
(load "tests-1.5-req.scm")
(load "tests-1.5-opt.scm")|#

;(load "tests-1.8-opt.scm") ; test pair
;(load "tests-print.scm")
;(load "tests-proc.scm")
;(load "tests-constant.scm")
;(load "tests-1.8-req.scm")
;(load "tests-1.9-req.scm") ; test begin, vector, string, set-car!/set-cdr!
;(load "tests-2.1-req.scm") ; closure test
;(load "tests-2.2-req.scm") ; test set!
;(load "tests-2.3-req.scm") ; test constants
(load "tests-2.4-req.scm")
;(load "seck-tests.scm")
