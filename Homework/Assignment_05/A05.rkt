;;;
;;; A05.rkt
;;;
;;; by Zachary Gault
;;;
;;; This file contains my solutions to the Rose-Hulman CSSE304 Programming Language
;;; Concepts Assignment 5.

#lang racket

(provide minimize-interval-list exists? product best remove-last)

;; (minimize-interval list ls) - Returns the set of intervals with the smallest
;; cardinality with the same union as the union of the list of intervals ls.
(define minimize-interval-list
  (lambda (ls)
    (if (null? ls)
        '()
        (let ([ls (sort ls first-<?)])
          (merge-intervals (cdr ls) (car ls))))))

;; (first-<? ls1 ls2) - Returns a boolean indicating whether the first element of
;; the 2-list ls1 is less than the first element of the 2-list ls2. Used as a helper
;; procedure for minimize-interval-list
(define first-<?
  (lambda (ls1 ls2)
    (< (first ls1) (first ls2))))

;; (merge-intervals ls previous) - Returns the set of intervals with the smallest
;; cardinality with the same union as the union of the list of intervals ls
;; where previous is the last unmerged interval. Used as a helper procedure for
;; minimize-interval.
(define merge-intervals
  (lambda (ls previous)
    (cond [(null? ls) (list previous)]
          [(<= (first (car ls)) (second previous))
           (merge-intervals
            (cdr ls)
            (list (first previous) (max (second previous) (second (car ls)))))]
          [else (cons previous (merge-intervals (cdr ls) (car ls)))])))

;; (exists? pred ls) - Returns a boolean indicating whether any element of the
;; list ls satisfies the predicate pred. Assumes each element of ls is in the
;; domain of pred.
(define exists?
  (lambda (pred ls)
    (cond [(null? ls) #f]
          [(pred (car ls)) #t]
          [else (exists? pred (cdr ls))])))

;; (best proc lst) - Returns the element in lst with the greatest value from proc.
;; Assumes that lst has at least 1 element.
(define best
  (lambda (proc lst)
    (let ((greatest (apply max (map proc lst))))
      (car (filter (lambda (el)
                       (equal? greatest (proc el)))
                     lst)))))
          
;; (product set1 set2) - Returns the cartesian product of set1 and set2 as a set
;; of 2-lists.
(define product
  (lambda (set1 set2)
    (let product-recur ([set1 set1]
                        [set2 set2]
                        [set2-orig set2])
      (cond [(null? set1) '()]
            [(null? set2)
             (product-recur (cdr set1) set2-orig set2-orig)]
            [else (cons (list (car set1) (car set2))
                        (product-recur set1 (cdr set2) set2-orig))]))))

;; (remove-last element ls) - Returns a list with every element of ls except for
;; the last occurrence of element.
(define remove-last
  (lambda (element ls)
    (cond [(null? ls) '()]
          [(and (equal? (car ls) element)
                (not (contains? (cdr ls) element)))
           (remove-last element (cdr ls))]
          [else (cons (car ls) (remove-last element (cdr ls)))])))

;; (contains? ls o) - Returns a boolean value that indicates whether the list ls
;; contains an object with the value of o. Used as a helper procedure.
(define contains?
  (lambda (ls o)
    (cond [(null? ls) #f]
          [(equal? (car ls) o) #t]
          [else (contains? (cdr ls) o)])))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
