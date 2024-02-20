;;;
;;; typing.rkt
;;;
;;; by Zachary Gault
;;;
;;; Starter Code Provided by Rose-Hulman CSSE304
;;;
;;; This file contains my solutions to the Rose-Hulman CSSE304 Programming Language
;;; Concepts Typing Assignment.

#lang racket

(require "../chez-init.rkt")
(provide typecheck)

(define-datatype type type?
  [number-t]
  [boolean-t]
  [proc-t (param type?) (ret type?)])

(define-datatype type-environment type-environment?
  [empty-tenv-record]
  [extended-tenv-record
   (syms (list-of? symbol?))
   (types (list-of? type?))
   (tenv type-environment?)])

;; (empty-tenv) - Returns an empty type-environment.
(define empty-tenv
  (lambda ()
    (empty-tenv-record)))

;; (extend-env syms types tenv) - Returns a new type-environment with the new
;; syms and types added.
(define extend-tenv
  (lambda (syms types tenv)
    (extended-tenv-record syms types tenv)))

;; (apply-tenv tenv sym) - Returns the type of the symbol sym in the
;; type-environment tenv. Falls back to global type-environment if
;; not found.
(define apply-tenv
  (lambda (tenv sym)
    (cases type-environment tenv 
      [empty-tenv-record ()
                        (apply-global-tenv global-tenv sym)]
      [extended-tenv-record (syms types tenv)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref types pos)
                                 (apply-tenv tenv sym)))])))

;; (apply-global-tenv tenv sym) - Returns the type of the symbol sym in the
;; type-environment tenv. Should be called with global-tenv as tenv. Raises
;; error if not found.
(define apply-global-tenv
  (lambda (tenv sym)
    (cases type-environment tenv 
      [empty-tenv-record ()
                        (raise 'unbound-var)]
      [extended-tenv-record (syms types tenv)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref types pos)
                                 (apply-global-tenv tenv sym)))])))

;; (make-init-tenv) - Returns the initial gloval type-environment
(define make-init-tenv
  (lambda ()
    (extend-tenv (list 'zero? '-)
                 (list (proc-t (number-t) (boolean-t))
                       (proc-t (number-t) (proc-t (number-t) (number-t))))
                 (empty-tenv))))

(define global-tenv (make-init-tenv))

;; (list-find-position sym position) - Returns the position of sym in the
;; list of symbols los.
(define list-find-position
  (lambda (sym los)
    (let loop ([los los] [pos 0])
      (cond [(null? los) #f]
            [(eq? sym (car los)) pos]
            [else (loop (cdr los) (add1 pos))]))))

;; (unparse-type t) - Returns an unparsed version of the type t.
(define unparse-type
  (lambda (t)
    (if (eqv? t 'unknown-expression)
        'unknown-expression ; just allow our little error type to pass through
        (cases type t
          [number-t () 'num]
          [boolean-t () 'bool]
          [proc-t (p r) (list (unparse-type p) '-> (unparse-type r))]))))
            
;; (parse-type type-exp)- Returns a parsed type from the provided
;; type expression type-exp
(define parse-type
  (lambda (type-exp)
    (cond [(eqv? 'num type-exp) (number-t)]
          [(eqv? 'bool type-exp) (boolean-t)]
          [(and (list? type-exp)
                (= (length type-exp) 3)
                (eqv? '-> (second type-exp)))
           (proc-t (parse-type (first type-exp))
                   (parse-type (third type-exp)))]
          [else (error 'parse-type "unknown type ~s" type-exp)])))

(define-datatype expression expression?
  [var-exp (name symbol?)]
  [lit-exp (val (lambda(x) (or (number? x) (boolean? x))))]
  [if-exp (test-exp expression?) (then-exp expression?) (else-exp expression?)]
  [lam-exp (var symbol?) (ptype type?) (body expression?)]
  [letrec-exp (recurse-var symbol?) (ret-type type?) (lambda lam-exp?) (body expression?)]
  [app-exp (rator expression?) (rand expression?)])

;; (lam-exp? exp) - Returns a boolean indicating whether an expression
;; is a valid lambda expression for our language.
(define lam-exp?
  (lambda (exp)
    (if (expression? exp)
        (cases expression exp
          [lam-exp (var ptype body) #t]
          [else #f])
        #f)))

;; (parse code) - Returns a expression of the parsed code.
(define parse
  (lambda (code)
    (cond [(symbol? code) (var-exp code)]
          [(number? code) (lit-exp code)]
          [(boolean? code) (lit-exp code)]
          [(list? code)
           (when (< (length code) 2) (error 'short-app "param list too short ~s" code))
           (if (symbol? (car code))
               (case (car code)
                 [(if) (unless (= (length code) 4) (error 'bad-if "bad if"))
                       
                       (if-exp (parse (second code))
                               (parse (third code))
                               (parse (fourth code)))]
                 [(lambda) (unless (= (length code) 4) (error 'bad-lambda "bad lambda"))
                           (let ([type (second code)]
                                 [param (third code)])
                             (unless (and
                                      (pair? param)
                                      (symbol? (car param)))
                               (error 'bad-param "bad lambda param ~s" (cadr code)))
                             (lam-exp (car param) (parse-type type) (parse (fourth code))))
                                 ]
                 [(letrec) (unless (= (length code) 5) (error 'bad-letrec "wrong length"))
                           (let [(ret (parse-type (second code)))
                                 (var (third code))
                                 (lam (parse (fourth code)))
                                 (body (parse (fifth code)))]
                             (unless (symbol? var) (error 'bad-lectrec "bad var"))
                             (unless (lam-exp? lam) (error 'bad-lectrec "lamdba required"))
                             (letrec-exp var ret lam body))]
                             
                 [else (parse-app code)])
               (parse-app code))]
           )))

;; (parse-app code) - Returns an app-exp of the parsed code.
(define parse-app
  (lambda (code)
    (app-exp (parse (first code))
                   (parse (second code)))))

;; (typecheck code) - Returns the unparsed type of the parsed code.
(define typecheck
  (lambda (code)
    (unparse-type (typecheck-exp (parse code) (empty-tenv)))))

;; (typecheck-exp exp tenv) - Returns the parsed type of the provided
;; expression exp given the type-environment tenv.
(define typecheck-exp
  (lambda (exp tenv)
    (cases expression exp
      [var-exp (sym) (apply-tenv tenv sym)]
      [lit-exp (value) (if (number? value) (number-t) (boolean-t))]
      [if-exp (test-exp then-exp else-exp) (let ((test-type (typecheck-exp test-exp tenv))
                                                 (then-type (typecheck-exp then-exp tenv))
                                                 (else-type (typecheck-exp else-exp tenv)))
                                             (cases type test-type
                                               [boolean-t () (if (equal? then-type else-type)
                                                                 then-type
                                                                 (raise 'bad-if-branches))]
                                               [else (raise 'bad-if-test)]))]
      [lam-exp (var ptype body) (proc-t ptype (typecheck-exp body (extend-tenv (list var) (list ptype) tenv)))]
      [letrec-exp (recurse-var ret-type lambda body)
                  (cases expression lambda
                    [lam-exp (var ptype lam-body)
                                (let ((lambda-type (typecheck-exp lambda (extend-tenv (list recurse-var)
                                                                                      (list (proc-t ptype ret-type))
                                                                                      tenv))))
                                  (cases type lambda-type
                                    [proc-t (param ret) (if (equal? ret ret-type)
                                                            (typecheck-exp body (extend-tenv (list recurse-var)
                                                                                             (list lambda-type)
                                                                                             tenv))
                                                            (raise 'bad-letrec-types))]
                                    [else (raise 'bad-letrec-types)]))]
                    [else (raise 'bad-letrec-lambda)])]
      [app-exp (rator rand) (let ((rator-type (typecheck-exp rator tenv))
                                  (rand-type (typecheck-exp rand tenv)))
                              (cases type rator-type
                                [proc-t (param ret) (if (equal? rand-type param)
                                                        ret
                                                        (raise 'bad-parameter))]
                                [else (raise 'bad-procedure)]))]
      [else 'unknown-expression])))