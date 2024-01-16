;;;
;;; A04.rkt
;;;
;;; by Zachary Gault
;;;
;;; This file contains my solutions to the Rose-Hulman CSSE304 Programming Language
;;; Concepts Assignment 4.

#lang racket

(provide pop-song? running-sum invert combine-consec)

;; (pop-song? lst) - Returns a boolean indicating whether the list lst
;; represents a valid pop-song.
(define pop-song?
  (lambda (lst)
    (letrec ([start-recur
              (lambda (lst)
                (cond [(null? lst) #f]
                      [(equal? 'verse (car lst)) (finish-verse-recur (cdr lst))]
                      [else #f]))]
             [finish-verse-recur
              (lambda (lst)
                (cond [(null? lst) #f]
                      [(equal? 'refrain (car lst)) (finish-refrain-recur (cdr lst))]
                      [(equal? 'guitar-solo (car lst)) (finish-verse-recur (cdr lst))]
                      [else #f]))]
             [finish-refrain-recur
              (lambda (lst)
                (cond [(null? lst) #f]
                      [(and (equal? 'refrain (car lst)) (null? (cdr lst))) #t]
                      [(equal? 'guitar-solo (car lst)) (finish-refrain-recur (cdr lst))]
                      [(equal? 'verse (car lst)) (finish-verse-recur (cdr lst))]
                      [else #f]))])
      (start-recur lst))))
                                                                         
;; (running-sum lst) - Returns a list where every element of the list of
;; numbers lst is replaced with the sum of it and all previous elements.
(define running-sum
  (lambda (lst)
    (let running-recur ([lon lst] [sum 0])
      (if (null? lon)
          '()
          (cons (+ sum (car lon))
                (running-recur (cdr lon) (+ sum (car lon))))))))

;; (invert lst) - Returns a list of 2-lists where every element of the
;; list of 2-lists lst is reversed. 
(define invert
  (lambda (lst)
    (map (lambda (pair)
           (list (second pair) (first pair)))
         lst)))

;; (combine-consec lst) - Returns a list of 2-lists that combines the
;; ranges of the sorted list of numbers lst.
(define combine-consec
  (lambda (lst)
    (if (null? lst)
        '()
        (combine-consec-recur (cdr lst) (car lst) (car lst)))))

;; (combine-consec-recur lst start previous) - Returns a list of 2-lists
;; that combines the ranges of the sorted list of numbers lst where start
;; is the start of the range and previous was the last element of that
;; range. Used as a helper procedure for combine-consec.
(define combine-consec-recur
  (lambda (lst start previous)
    (cond [(null? lst) (list (list start previous))]
          [(eqv? (car lst) (add1 previous))
           (combine-consec-recur (cdr lst) start (car lst))]
          [else (cons (list start previous)
                      (combine-consec-recur (cdr lst) (car lst) (car lst)))])))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
