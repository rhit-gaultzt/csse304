#lang racket

(require racket/trace)

; num-positive - returns the number of positive elements in a list
; implement this in a tail recursive way, similar to fact2
;
; (num-positive '(1 -2 0 100 77)) -> 3

(define num-positive
  (lambda (lon)
    (num-positive-recur lon 0)))

; you'll want a helper function
(define num-positive-recur
  (lambda (lon acc)
    (cond [(null? lon) acc]
          [(> (car lon) 0) (num-positive-recur (cdr lon) (+ acc 1))]
          [else (num-positive-recur (cdr lon) acc)])))
        
        


; second largest - returns the second largest element in a list of numbers
;
; (second-largest '( 7 4 5 3 6 2 1)) -> 6
;
; you can assume the list has 2 elements
; implement this in a tail recursive way with a helper function

(define second-largest
  (lambda (lon)
    'nyi))

(trace num-positive num-positive-recur second-largest)
