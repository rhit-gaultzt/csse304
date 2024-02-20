;;;
;;; A10.rkt
;;;
;;; by Zachary Gault
;;;
;;; This file contains my solutions to the Rose-Hulman CSSE304 Programming Language
;;; Concepts Assignment 10.

#lang racket

(provide free-vars bound-vars lexical-address un-lexical-address convert-multip-calls convert-multip-lambdas convert-ifs)

(require racket/trace)

;; (free-vars e) - Returns a list of all of the variables that occur free in in the
;; LcExp e.
(define free-vars
  (lambda (e)
    (remove-duplicates
     (let recur ([e e]
                 [declared '()])
       (cond
         [(symbol? e)
          (if (not (member e declared))
              (list e)
              '())]
         [(eqv? 'lambda (first e))
          (recur (third e) (cons (caadr e) declared))]
         [else
          (append (recur (first e) declared)
                  (recur (second e) declared))])))))

;; (free-vars e) - Returns a list of all of the variables that occur bound in in the
;; LcExp e.
(define bound-vars
  (lambda (e)
    (remove-duplicates
     (let recur ([e e]
                 [declared '()])
       (cond
         [(symbol? e)
          (if (member e declared)
              (list e)
              '())]
         [(eqv? 'lambda (first e))
          (recur (third e) (cons (caadr e) declared))]
         [else
          (append (recur (first e) declared)
                  (recur (second e) declared))])))))

;; (remove-duplicates ls) - Removes all duplicates in the list ls. Helper procedure.
(define remove-duplicates
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(member (first ls) (cdr ls))
       (remove-duplicates (cdr ls))]
      [else
       (cons (first ls)
             (remove-duplicates (cdr ls)))])))
    
;; (convert-multip-calls lcexp) - Returns an equivalent LcExp to the LcExprMultiP
;; program with calls of arbitrary size e.
(define convert-multip-calls
  (lambda (e)
    (cond
      [(symbol? e) e]
      [(eqv? 'lambda (first e))
       (list 'lambda
             (list (caadr e))
             (convert-multip-calls (third e)))]
      [else
       (let recur ([e (cddr e)]
                   [built (list (convert-multip-calls (first e))
                                (convert-multip-calls (second e)))])
         (if (null? e)
             built
             (recur (cdr e) (list built
                                  (convert-multip-calls (first e))))))])))
         
;; (convert-multip-lambdas lcexp) - Returns an equivalent LcExp to the LcExprMultiP
;; program with lambas of arbitrary parameters e.
(define convert-multip-lambdas
  (lambda (e)
    (cond
      [(symbol? e) e]
      [(eqv? 'lambda (first e))
       (let recur ([params (second e)])
         (if (null? params)
             (convert-multip-lambdas (third e))
             (list 'lambda
                   (list (first params))
                   (recur (cdr params)))))]
      [else
       (list (convert-multip-lambdas (first e))
             (convert-multip-lambdas (second e)))])))

;; (convert-ifs e) - Returns an equivalent LcExprMultiP to the IfExp e.
(define convert-ifs
  (lambda (e)
    (cond
      [(eqv? '#t e) '(lambda (thenval elseval) thenval)]
      [(eqv? '#f e) '(lambda (thenval elseval) elseval)]
      [(symbol? e) e]
      [(eqv? 'lambda (first e))
       (list 'lambda
             (second e)
             (convert-ifs (third e)))]
      [(eqv? 'if (first e))
       (list (convert-ifs (second e))
             (convert-ifs (third e))
             (convert-ifs (fourth e)))]
      [else (map convert-ifs e)])))
             
;; (lexical-address e) - Returns a copy of the SchemliteExpr e that
;; replaces every bound variable occurrence with the list (: d p) where
;; d and p are the lexical depth and position of the variable occurrence
;; and every free variable occurrence xyz with the list (: free xyz).
(define lexical-address
  (lambda (e)
    (let recur ([e e]
                [bounds '()])
      (if (symbol? e) (get-lexical-address e bounds)
          (case (first e)
            ['lambda (list 'lambda
                           (second e)
                           (recur (third e) (get-bounds (second e) 0 bounds)))]
            ['if (cons 'if (map (lambda (x)
                                  (recur x bounds))
                                (cdr e)))]
            ['let (list 'let
                        (map (lambda (x)
                               (list (first x)
                                     (recur (second x) bounds)))
                             (second e))
                        (recur (third e) (get-bounds (map first (second e)) 0 bounds)))]                           
            [else (map (lambda (x)
                         (recur x bounds))
                       e)])))))

;; (get-lexical-address sym bounds) - Returns a list representing the
;; lexical address of the symbol sym in the address list bounds.
;; Helper procedure for lexical-address.
(define get-lexical-address
  (lambda (sym bounds)
    (cond [(null? bounds)
           (list ':
                 'free
                 sym)]
          [(eqv? sym (caar bounds))
           (list ':
                 (cadar bounds)
                 (caddar bounds))]
          [else (get-lexical-address sym (cdr bounds))])))

;; (get-bounds params pos previous) - Returns an updated address list
;; for bound variables when recursing one level deeper. Helper procedure.
(define get-bounds
  (lambda (params pos previous)
    (if (null? params)
        (map increment-depth previous)
        (cons (list (car params)
                    0
                    pos)
              (get-bounds (cdr params)
                          (add1 pos)
                          previous)))))

;; (increment-depth bound) - Increments the lexical depth given the
;; lexical address bound. Helper procedure for get-bounds.
(define increment-depth
  (lambda (bound)
    (if (null? bound)
        '()
        (list (first bound)
              (add1 (second bound))
              (third bound)))))

;; (un-lexical-address e) - Returns a copy of the lexical address replaced
;; SchemliteExpr e that replaces the lexical addresses with their original
;; variables. i.e. (un-lexical-address (lexical-address e)) -> e.
(define un-lexical-address
  (lambda (e)
    (let recur ([e e]
                [bounds '()])
      (case (first e)
        [': (if (eqv? 'free (second e))
                (third e)
                (get-sym-from-address (cdr e) bounds))]
        ['lambda
         (list 'lambda
               (second e)
               (recur (third e) (get-bounds (second e) 0 bounds)))]
        ['if (cons 'if (map (lambda (x)
                              (recur x bounds))
                            (cdr e)))]
        ['let
         (list 'let
               (map (lambda (x)
                      (list (first x)
                            (recur (second x) bounds)))
                    (second e))
               (recur (third e) (get-bounds (map first (second e)) 0 bounds)))]
        [else (map (lambda (x)
                     (recur x bounds))
                   e)]))))

;; (get-sym-from-address address bounds) - Returns the symbol associated
;; with the given lexical address address in the address list bounds.
;; Helper procedure for un-lexical-address.
(define get-sym-from-address
  (lambda (address bounds)
    (cond [(null? bounds) 'unknown-bound-variable]
          [(and (eq? (first address) (cadar bounds))
               (eq? (second address) (caddar bounds)))
           (caar bounds)]
           [else (get-sym-from-address address (cdr bounds))])))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
