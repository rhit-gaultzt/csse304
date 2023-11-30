;;;
;;; A02.rkt
;;;
;;; by Zachary Gault
;;;
;;; This file contains my solutions to the Rose-Hulman CSSE304 Programming Language
;;; Concepts Assignment 2.

#lang racket

(provide sum-of-squares range my-set? union more-positives? add-quotes get-304-quine)

;; (sum-of-squares lon) - Returns the sum of squares of the list of numbers lon
(define sum-of-squares
  (lambda (lon)
    (if (null? lon)
        0
        (+ (* (car lon) (car lon))
           (sum-of-squares (cdr lon))))))

;; (range m n) - Returns the ordered list of integers in the range [m, n).
;; If n <= m, returns the empty list.
(define range
  (lambda (m n)
    (if (<= n m)
    '()
    (cons m (range (+ 1 m) n)))))
    
;; (my-set? ls) - Returns a boolean value that indicates whether the list ls
;; is a set. i.e. ls has no duplicates.
(define my-set?
  (lambda (ls)
    (cond [(null? ls) #t]
          [(contains? (cdr ls) (car ls)) #f]
          [else (my-set? (cdr ls))])))

;; (contains? ls o) - Returns a boolean value that indicates whether the list ls
;; contains an object with the value of o. Used as a helper procedure.
(define contains?
  (lambda (ls o)
    (cond [(null? ls) #f]
          [(equal? (car ls) o) #t]
          [else (contains? (cdr ls) o)])))
  
;; (union s1 s2) - Returns the union of the sets s1 and s2
(define union
  (lambda (s1 s2)
    (cond [(null? s1) s2]
          [(contains? s2 (car s1)) (union (cdr s1) s2)]
          [else (cons (car s1) (union (cdr s1) s2))])))

;; (more-positives? lon) - Returns a boolean value that indicates whether the list
;; of numbers lon has more positive numbers than non-positive numbers.
(define more-positives?
  (lambda (lon)
    (positive? (running-positives lon 0))))

;; (running-positives lon acc) - Returns the number of positive numbers minus the number
;; of non-positive numbers in lon. Used as a helper procedure for more-positives?.
(define running-positives
  (lambda (lon acc)
    (cond [(null? lon) acc]
          [(positive? (car lon)) (running-positives (cdr lon) (+ 1 acc))]
          [else (running-positives (cdr lon) (- acc 1))])))

;; (add-quotes val num) - Returns val with num number of quotes added to the front.
(define add-quotes
  (lambda (val num)
    (if (<= num 0)
        val
        (list (quote quote)
              (add-quotes val (- num 1))))))
           
; Stuff for the final quine problem

;; (get-304-quine) returns quine with added 304 based on the provided quine
;; ((lambda (f) (list f (list (quote quote) f))) (quote (lambda (f) (list f (list (quote quote) f)))))
(define get-304-quine
  (lambda ()
    "((lambda (f num) (list f (list (quote quote) f) num)) (quote (lambda (f num) (list f (list (quote quote) f) num))) 304)"))


(define eval-string
  (lambda (str)
    (let ((outp (open-output-string)))
      (parameterize ([current-output-port outp])
        (printf "~s" (eval (read (open-input-string str)) (make-base-namespace))))
      (get-output-string outp))))

(define is-quine-string?
 (lambda (str)
   (let ((result (eval-string str)))
     (if (equal? result str)
         #t
         (begin
           (printf "NOT QUINE~nIn : ~s~nOut: ~s" str result)
           #f)))))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
