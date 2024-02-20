;;;
;;; A09.rkt
;;;
;;; by Zachary Gault
;;;
;;; This file contains my solutions to the Rose-Hulman CSSE304 Programming Language
;;; Concepts Assignment 9.

#lang racket

(provide sn-list-recur sn-list-sum sn-list-map sn-list-paren-count sn-list-reverse sn-list-occur sn-list-depth bt-recur bt-sum bt-inorder)

;; (sn-list-recur base-value list-proc sn-proc) - Procedural Abstraction for common
;; sn-list procedures.
(define sn-list-recur
  (lambda (base-value list-proc sn-proc)
    (letrec
        ([helper
          (lambda (snlst)
            (cond [(null? snlst) base-value]
                  [(list? (first snlst))
                   (list-proc (helper (first snlst))
                              (helper (cdr snlst)))]
                  [else
                   (sn-proc (first snlst)
                            (helper (cdr snlst)))]))])
      helper)))                                        

;; (sn-list-sum snlst) - Returns the sum of all of the numbers within the
;; sn-list snlst. 
(define sn-list-sum
  (sn-list-recur 0 + +))

;; (sn-list-map proc snlst) - Returns an sn-list with the result of proc
;; applied to each element of the sn-list snlst.
(define sn-list-map
  (lambda (proc snlst)
    ((sn-list-map-c proc) snlst)))

;; (sn-list-map-c proc) - Curried helper procedure for sn-list-map.
(define sn-list-map-c
  (lambda (proc)
    (sn-list-recur '()
                   cons
                   (lambda (cur rest)
                     (cons (proc cur) rest)))))
  
;; (sn-list-paren-count snlst) - Returns the number of parentheses needed to
;; print the sn-list snlst.
(define sn-list-paren-count
  (sn-list-recur 2 + (lambda (cur rest)
                       rest)))

;; (sn-list-reverse snlst) - Returns an sn-lst where snlst and all sublists
;; are reversed.
(define sn-list-reverse
  (sn-list-recur '()
                 (lambda (in rest)
                   (append rest (list in)))
                 (lambda (cur rest)
                   (append rest (list cur)))))

;; (sn-list-occur s snlst) - Returns the number of occurrences of s in the
;; sn-list snlst.
(define sn-list-occur
  (lambda (s snlst)
    ((sn-list-occur-c s) snlst)))

;; (sn-list-occur-c) - Curried helper procedure for sn-list-occur.
(define sn-list-occur-c
  (lambda (s)
    (sn-list-recur 0 + (lambda (cur rest)
                         (if (and (symbol? cur)
                                  (eqv? cur s))
                             (add1 rest)
                             rest)))))

;; (sn-list-depth snlst) - Returns the maximum nesting level of the sn-list
;; snlst.
(define sn-list-depth
  (sn-list-recur 1
                 (lambda (in rest)
                   (max (add1 in) rest))
                 (lambda (cur rest)
                   rest)))

;; (bt-recur leaf-proc inner-proc)- Procedural Abstraction for common bintree
;; procedures.
(define bt-recur
  (lambda (leaf-proc inner-proc)
    (letrec
        ([helper
          (lambda (T)
            (if (number? T)
                (leaf-proc T)
                (inner-proc (first T)
                            (helper (second T))
                            (helper (third T)))))])
      helper))) 

;; (bt-sum T) - Returns the sum of all leaf numbers of the bintree T.
(define bt-sum
  (bt-recur (lambda (num)
              num)
            (lambda (s left right)
              (+ left right))))

;; (bt-inorder T) - Returns a list of the symbols from the interior nodes of T
;; if visited in an inorder traversal.
(define bt-inorder
  (bt-recur (lambda (num)
              '())
            (lambda (s left right)
              (append left (list s) right))))
                                 

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
