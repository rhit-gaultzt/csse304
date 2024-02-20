;;;
;;; A03.rkt
;;;
;;; by Zachary Gault
;;;
;;; This file contains my solutions to the Rose-Hulman CSSE304 Programming Language
;;; Concepts Assignment 3.

#lang racket

(provide intersection subset? relation? domain reflexive? multi-set? ms-size last all-but-last)

;; (intersection s1 s2) - Returns the intersection of the sets s1 and s2.
(define intersection
  (lambda (s1 s2)
    (cond [(null? s1) '()]
          [(contains? s2 (car s1)) (cons (car s1) (intersection (cdr s1) s2))]
          [else (intersection (cdr s1) s2)])))

;; (contains? ls o) - Returns a boolean value that indicates whether the list ls
;; contains an object with the value of o. Used as a helper procedure.
(define contains?
  (lambda (ls o)
    (cond [(null? ls) #f]
          [(equal? (car ls) o) #t]
          [else (contains? (cdr ls) o)])))

;; (subset? s1 s2) - Returns a boolean value that indicates whether the set s1 is
;; a subset of s2.
(define subset?
  (lambda (s1 s2)
    (cond [(null? s1) #t]
          [(contains? s2 (car s1)) (subset? (cdr s1) s2)]
          [else #f])))

;; (relation? obj) - Returns a boolean value that indicates whether obj is a relation.
;; i.e. obj is a set of order pairs.
(define relation?
  (lambda (obj)
    (cond [(null? obj) #t]
          [(not (set? obj)) #f]
          [(null? (car obj)) #f]
          [(not (list? (car obj))) #f]
          [(null? (cdar obj)) #f]
          [(null? (cddar obj)) (relation? (cdr obj))]
          [else #f])))

;; (set? ls) - Returns a boolean value that indicates whether the list ls
;; is a set. i.e. ls has no duplicates. Used as a helper procedure.
(define set?
  (lambda (ls)
    (cond [(null? ls) #t]
          [(not(list? ls)) #f]
          [(contains? (cdr ls) (car ls)) #f]
          [else (set? (cdr ls))])))

;; (domain r) - Returns the domain of the relation r. i.e. the first element
;; in each ordered pair of the relation.
(define domain
  (lambda (r)
    (if (null? r)
        '()
        (cons (caar r) (domain (cdr r))))))

;; (reflexive r) - Returns a boolean value that indicates whether the relation
;; r is reflexive. i.e. every element of domain and range is related to self.
(define reflexive?
  (lambda (r)
    (reflexive-helper r (self-relations r '()))))

;; (self-relations r r-self-relations) - Returns the list of all elements in r
;; that are related to themselves. Used as a helper procedure.
(define self-relations
  (lambda (r r-self-relations)
    (cond [(null? r) r-self-relations]
          [(equal? (caar r) (cadar r))
           (self-relations (cdr r) (cons (caar r) r-self-relations))]
          [else (self-relations (cdr r) r-self-relations)])))

;; (reflexive-helper  r-self-relations) - Returns a boolean value that indicates
;; whether the relation r is reflexive using the provided r-self-relations.
;; Used as a helper procedure.
(define reflexive-helper
  (lambda (r r-self-relations)
    (cond [(null? r) #t]
          [(not (contains? r-self-relations (caar r))) #f]
          [(not (contains? r-self-relations (cadar r))) #f]
          [else (reflexive-helper (cdr r) r-self-relations)])))
    
;; (multi-set? obj) - Returns a boolean value that indicates whether obj is a
;; multiset - i.e. list of 2-lists containing symbol and positive integer.
(define multi-set?
  (lambda (obj)
    (cond [(null? obj) #t]
          [(not (relation? obj)) #f]
          [(not (symbol? (caar obj))) #f]
          [(contains? (domain (cdr obj)) (caar obj)) #f]
          [(and (integer? (cadar obj)) (> (cadar obj) 0))
           (multi-set? (cdr obj))]
          [else #f])))
          
;; (ms-size ms) - Returns the total number of elements in the multi-set ms.  
(define ms-size
  (lambda (ms)
    (if (null? ms)
        0
        (+ (cadar ms) (ms-size (cdr ms))))))

;; (last ls) - Returns the last element of the list ls.  Assumes that ls is
;; a non-empty proper list.
(define last
  (lambda (ls)
    (if (null? (cdr ls))
        (car ls)
        (last (cdr ls))))) 

;; (all-but-last ls) - Returns a list containing all of the elements of ls
;; except for the last element, in original order.
(define all-but-last
  (lambda (ls)
    (if (null? (cdr ls))
        '()
        (cons (car ls) (all-but-last (cdr ls))))))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
