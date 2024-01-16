;;;
;;; macros.rkt
;;;
;;; by Zachary Gault
;;;
;;; This file contains our solutions to the Rose-Hulman CSSE304 Programming Language
;;; Concepts Macros Assignment.

#lang racket

; only functions in racket/base can be used by default in macros
; this adds some other useful prodcedures
(require (for-syntax racket/list)) 

(provide my-let my-or let-destruct range-cases ifelse define-object define-method)

;; (my-let stx) - converts (my-let ((var val) ...) bodies ...) to an equivalent
;; lambda expression and (my-let name ((var val) ...) bodies ...) to an equivalent
;; letrec. Like (let stx).
(define-syntax (my-let stx)
  (syntax-case stx ()
    [(my-let ((var val) ...) bodies ...)
     #'((lambda (var ...) bodies ...) val ...)]
    [(my-let name ((var val) ...) bodies ...)
     #'(letrec ((name (lambda (var ...) bodies ...)))
         (name val ...))]))

;; (my-or stx) - converts (my-or exp exps ...) to create the or operator without
;; evaluating the same expression twice.
(define-syntax (my-or stx)
  (syntax-case stx ()
    [(my-or) #'#f]
    [(my-or exp exps ...)
     #'(let ((exp-res exp))
         (if exp-res
           exp-res
           (my-or exps ...)))]))

;;(range-cases stx) - Converts
;; (range-cases val-exp (< cutoff-exp body-exps) ... (else else-body-exp)) to
;; only execute the first body-exp where cutoff-exp is true for val-exp or
;; else if all are false. Ensures that val-exp is only called once.
(define-syntax (range-cases stx)
      (syntax-case stx (< else)
        [(range-cases val-exp (else else-body-exp))
         #'else-body-exp]
        [(range-cases val-exp (< cutoff-exp body-exp) (else else-body-exp))
         #'(let ((val val-exp))
             (if (< val cutoff-exp)
                 body-exp
                 (range-cases val (else else-body-exp))))]
        [(range-cases val-exp (< cutoff-exp body-exp) (< cutoff-exps body-exps) ... (else else-body-exp))
         #'(let ((val val-exp))
             (if (< val cutoff-exp)
                 body-exp
                 (range-cases val (< cutoff-exps body-exps) ... (else else-body-exp))))]))

;; (let-destruct stx) - Converts (let-destruct vars value bodies ...) to a
;; series of let expressions where each symbol in the struct vars is given
;; the value in the same location in value.
;;
(define-syntax (let-destruct stx)
  (syntax-case stx ()
    [(let-destruct (var var2 vars ...) value bodies ...)
     #'(let-destruct var (car value) (let-destruct (var2 vars ...) (cdr value) bodies ...))]
    [(let-destruct (var) value bodies ...)
     #'(let-destruct var (car value) bodies ...)]
    [(let-destruct () value bodies ...) #'(let () bodies ...)]
    [(let-destruct var value bodies ...)
     #'(let ((var value)) bodies ...)]))
    
;; (if-else stx) - Converts (ifelse testexp thenexps else eleseexps) to
;; (if testexp (begin then-exps) (begin elseexps)) where there is at least one
;; exp in both thenexps and elseexps.
(define-syntax ifelse
  (lambda (stx)
    (let ((data (cdr (syntax->datum stx))))
      (let recur ((obj (cdr data))
                  (then-exps '())
                  (else-exps '())
                  (in-else #f))
      (cond
        [(null? obj) (datum->syntax stx (list 'if (first data)
                                              (cons 'begin (reverse then-exps))
                                              (cons 'begin (reverse else-exps))))]
        [(eqv? 'else (first obj)) (recur (cdr obj) then-exps else-exps #t)]
        [in-else (recur (cdr obj) then-exps (cons (first obj) else-exps) #t)]
        [else (recur (cdr obj) (cons (first obj) then-exps) else-exps #f)])))))

(define-syntax (define-object stx)
    #''nyi
    )

(define-syntax (define-method stx)
  (syntax-case stx ()
    [(define-method _ name _ ...)
     #'(define name (lambda x (error "nyi")))])) ; <- had to do a little more work to prevent the test cases
                                        ; from failing all the other tests.  You'll have to change this
                                        ; template quite a bit though.
