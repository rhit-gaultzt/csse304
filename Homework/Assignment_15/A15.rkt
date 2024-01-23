;;;
;;; A15.rkt
;;;
;;; by Zachary Gault
;;;
;;; This file contains my solutions to the Rose-Hulman CSSE304 Programming Language
;;; Concepts Assignment 15.

#lang racket

(require "../chez-init.rkt")
(provide slist-subst-cps remove-cps free-vars-cps continuation? init-k list-k)

;; datatype continuation
(define-datatype continuation continuation? 
  [init-k] 
  [list-k]
  [slist-step1 (slist list?)
               (old symbol?)
               (new symbol?)
               (k continuation?)]
  [slist-step2 (slist list?)
               (old symbol?)
               (new symbol?)
               (k continuation?)]
  [slist-step3 (slist list?)
               (old symbol?)
               (new symbol?)
               (other list?)
               (k continuation?)]
  [remove-step1 (los (list-of? symbol?))
                (k continuation?)]
  [free-vars-step1 (exp list?)
                   (k continuation?)]
  [free-vars-step2 (exp list?)
                   (k continuation?)]
  [free-vars-step3 (exp list?)
                   (other list?)
                   (k continuation?)]
  )

;; (apply-k k v) - Applies the continuation k with the precomputed v.
(define apply-k
  (lambda (k v)
    (cases continuation k
      [init-k () v]
      [list-k () (list v)] ; <- leave this in we use it for testing
      [slist-step1 (slist old new k)
                   (apply-k k 
                            (cons (if (eqv? (car slist) old) new (car slist))
                                  v))]
      [slist-step2 (slist old new k)
                   (slist-subst-cps (cdr slist) old new (slist-step3 slist old new v k))]
      [slist-step3 (slist old new other k)
                   (apply-k k
                            (cons other v))]
      [remove-step1 (los k)
                    (apply-k k (cons (car los)
                                     v))]
      [free-vars-step1 (exp k)
                       (remove-cps (car (second exp)) 
                                   v k)]
      [free-vars-step2 (exp k)
                       (free-vars-cps (second exp) (free-vars-step3 exp v k))]
      [free-vars-step3 (exp other k)
                       (apply-k k (union other v))]
      )))

;; (slist-subst-cps slst old new k) - Returns an slist where all
;; occurrences of old in slist are replaced with new where k is
;; the initial continuation (init-k). Uses Continuation Passing
;; Style.
(define slist-subst-cps
  (lambda (slist old new k)
    (cond [(null? slist) (apply-k k '())]
         [(symbol? (car slist))
          (slist-subst-cps (cdr slist) old new (slist-step1 slist old new k))]
         [else (slist-subst-cps (car slist) old new (slist-step2 slist old new k))])))

;; (union s1 s2) - Returns the union of the sets s1 and s2.
;; Treated as a built in function for CPS.
(define union 
    (lambda (s1 s2)    
        (let loop ([s1  s1])
            (cond [(null? s1) s2]
                [(memq (car s1) s2) (loop (cdr s1))]
                [else (cons (car s1) (loop (cdr s1)))]))))

;; (free-vars-cps exp k) - Returns a list of the free variables
;; in the expression exp where k is the initial continuation (init-k).
;; Uses Continuation Passing Style.
(define free-vars-cps
  (lambda (exp k)
    (cond [(symbol? exp) (apply-k k (list exp))]
            [(eq? (first exp) 'lambda)       
            (free-vars-cps (third exp) (free-vars-step1 exp k))]      
            [else (free-vars-cps (first exp) (free-vars-step2 exp k))])))

;; (remove-cps sym los k) - Returns a list of symbols where sym is
;; removed from the list of symbols los where k is the initial
;; continuation (init-k). Uses Continuatin Passing Style
(define remove-cps
  (lambda (sym los k)
    (cond [(null? los) (apply-k k '())]
            [(eq? sym (car los)) (apply-k k (cdr los))]
            [else (remove-cps sym (cdr los) (remove-step1 los k))])))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
