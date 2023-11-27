#lang racket

(provide interval-contains? interval-intersects? interval-union make-vec-from-points dot-product vector-magnitude distance)

;; (interval-contain?s a b) - Returns a boolean value that indicates
;; whether the integer b is in the closed interval a.
(define interval-contains?
  (lambda (a b)
    (and (<= (car a) b)
         (>= (cadr a) b))))

;; (interval-intersects? a b) - Returns a boolean value that indicates
;; whether the closed intervals a and b have a nonempty intersection.
(define interval-intersects?
  (lambda (a b)
    (or (interval-contains? a (car b))
        (interval-contains? a (cadr b))
        (interval-contains? b (car a))
        (interval-contains? b (cadr a)))))

;; (interval-union a b) - Returns the union of closed intervals a and b.
(define interval-union
  (lambda (a b)
    (if (interval-intersects? a b)
        (list (list (min (car a) (car b))
                    (max (cadr a) (cadr b))))
        (list a b))))

;; (make-vec-from-points a b) - Returns a vector that goes from point a
;; to point b. Assumes a and b have equal length.
(define make-vec-from-points
  (lambda (a b)
    (if (null? a)
        '()
        (cons (- (car b) (car a))
              (make-vec-from-points (cdr a) (cdr b))))))

;; (dot-product a b) - Returns the dot product of vectors a and b. Assumes
;; a and b have equal length.
(define dot-product
  (lambda (a b)
    (if (null? a)
        0
        (+ (* (car a) (car b))
           (dot-product (cdr a) (cdr b))))))

;; (vector-magnitude a) - Returns the magnitude of the vector a. Assumes the
;; magnitude of the vector is an integer.
(define vector-magnitude
  (lambda (a)
    (sqrt (vector-sum-squares a))))

;; (vector-sum-squares a) - Returns the sum of squares of the vector a. Helper
;; procedure for vector-magnitude.
(define vector-sum-squares
  (lambda (a)
    (if (null? a)
        0
        (+ (* (car a) (car a))
           (vector-sum-squares (cdr a))))))

;; (distance a b) - Returns the distance between points a and b. Assumes the
;; distance is an integer.
(define distance
  (lambda (a b)
    (vector-magnitude (make-vec-from-points a b))))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
