#lang racket

(provide (all-defined-out))

(define fxshift 2)
(define fxmask #x03)
(define fxtag #x00)
(define bool-f #x2F)
(define bool-t #x3F)
(define null-v #x4F)
(define void-v #x6F)
(define charmask #xFF)
(define chartag #x0F)
(define charshift 8)
(define boolshift 6)
(define boolmask #x3F)
(define booltag #x2F)
(define wordsize 4) ; 4byte
; pair,closure,symbol,vector,string...are object
(define objshift 3)
(define objmask #x07)
(define objtag #x07)
(define pairmask objmask)
(define pairtag #x01)
(define cljmask objmask)
(define cljtag #x02)
(define cljshift objshift)
; symbol is also atom
(define symmask objmask)
(define symtag #x03)
(define symshift objshift)
(define vecmask objmask)
(define vectag #x05)
(define vecshift objshift)
(define strmask objmask)
(define strtag #x06)
(define heap-align 8)

; offset of global
(define global-offset 4)

(define registers
  '((eax . scratch)
    (ebx . preserve)
    (ecx . scratch)
    (edx . scratch)
    (esi . preserve)
    (edi . preserve)
    (ebp . preserve)
    (esp . preserve)))

(define (scratch? reg)
  (and (pair? reg)
       (eq? (cdr reg) 'scratch)))

; Data representation:
; Integer: ....00
; Bool: bool-f | bool-t
; Char: ....00001111

(define fixnum-bits (- (* wordsize 8) fxshift))
(define fxlower (- (expt 2 (- fixnum-bits 1))))
(define fxupper (sub1 (expt 2 (- fixnum-bits 1))))
(define (fixnum? x)
  (and (number? x)
       (exact? x)
       (<= fxlower x fxupper)))

(define (immediate? x)
  (or (fixnum? x)
      (boolean? x)
      (char? x)
      (null? x)
      ))

(define (immediate-rep x)
  (match x
    [(? fixnum?)
     (arithmetic-shift x fxshift)]
    [(? boolean?)
     (if (eq? x #t)
       bool-t
       bool-f)]
    [(? char?)
     (+ (arithmetic-shift (char->integer x) charshift)
        chartag)]
    ['() null-v]
    [_ (error 'immediate-rep "~a is not an immediate" x)]
    ))

(define (unop? op) (memq op '(number->char char->number
                                           fixnum?  number? char? null?
                                           boolean? not
                                           fxzero? zero?
                                           symbol? string? vector?
                                           car cdr pair?
                                           print
                                           constant-ref
                                           string-length
                                           vector-length
                                           fixnum->char char->fixnum
                                           procedure?
                                           )))

(define (biop? op)
  (memq op '(cons
              + fx+
              - fx-
              * fx*
              quotient
              remainder
              = fx=
              < fx<
              <= fx<=
              > fx>
              >= fx>=
              char=
              eq?
              set-car!
              set-cdr!
              )))

(define (prim-op? op)
  (or (unop? op)
      (biop? op)
      (memq op '(print set! void
                 make-vec vec-ref vec-set! vec
                 make-string string-ref string-set! string
                 make-symbol))))

(define (align size align-size)
  (bitwise-and (+ size (sub1 align-size))
               (- align-size)))

(define (align-heap-size size)
  (align size heap-align))
