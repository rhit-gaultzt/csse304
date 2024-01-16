;;;
;;; A08.rkt
;;;
;;; by Zachary Gault
;;;
;;; This file contains my solutions to the Rose-Hulman CSSE304 Programming Language
;;; Concepts Assignment 8.

#lang racket

(provide make-slist-leaf-iterator subst-leftmost)

;; (make-slist-leaf-iterator slist) - Returns an iterator of the leaves of slist.
;; which does not preprocess the entire slist. Call 'next to get next leaf.
(define make-slist-leaf-iterator
  (lambda (slist)
    (let ([slist slist]
          [stack (make-stack)])
      (lambda (command)
        (if (equal? command 'next)
            (let next-recur ()
              (cond [(null? slist)
                     (if (stack 'empty?)
                         #f
                         (begin
                           (set! slist (stack 'pop))
                           (next-recur)))]
                    [(symbol? (first slist))
                     (let ([next (first slist)])
                       (set! slist (cdr slist))
                       next)]
                    [else
                     (begin
                       (stack 'push (cdr slist))
                       (set! slist (first slist))
                       (next-recur))]))
            'invalid-command)))))
                         
;; (make-stack) - Returns a new stack which supports empty?, push, and pop.
;; Provided.
(define make-stack
    (lambda ()
        (let ([stk '()])
            (lambda (msg  . args ) 
                (case msg   ; Scheme's case is a similar to switch in some other languages.
                    [(empty?) (null? stk)]
                    [(push)   (set! stk (cons (car args) stk))]
                    [(pop)    (let ([top (car stk)])
                            (set! stk (cdr stk))
                            top)]
                    [else 'invalid-command])))))

;; (subst-leftmost new old slist equality-pred?) - Returns an slistwhere the leftmost
;; occurrance of old in slist is replaced with new where equality-pred? is the
;; equalty predicate.
(define subst-leftmost
   (lambda (new old slist equality-pred?)
     (first
      (let recur ([slist slist])
        (cond [(null? slist) (list '() #f)]
              [(symbol? (first slist))
               (if (equality-pred? old (first slist))
                   (list (cons new (cdr slist)) #t)
                   (let ([rest (recur (cdr slist))])
                     (list (cons (first slist)
                                 (first rest))
                           (second rest))))]
              [else
               (let* ([left-result (recur (first slist))]
                      [right-result (if (second left-result)
                                        (list (cdr slist) #f)
                                        (recur (cdr slist)))])
                 (list (cons (first left-result)
                             (first right-result))
                       (or (second left-result)
                           (second right-result))))])))))



;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
