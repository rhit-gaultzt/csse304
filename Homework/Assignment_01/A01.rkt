#lang racket

(provide interval-contains? interval-intersects? interval-union make-vec-from-points dot-product vector-magnitude distance)

;; (interval-contain?s interval number) - Returns a boolean value that indicates
;; whether number is in the closed interval a.
(define interval-contains?
  (lambda (interval number)
    (and (<= (car interval) number)
         (>= (cadr interval) number))))

;; (interval-intersects? i1 i2) - Returns a boolean value that indicates whether
;; the closed intervals i1 and i2 have a nonempty intersection.
(define interval-intersects?
  (lambda (i1 i2)
    (or (interval-contains? i1 (car i2))
        (interval-contains? i1 (cadr i2))
        (interval-contains? i2 (car i1))
        (interval-contains? i2 (cadr i1)))))

;; (interval-union i1 i2) - Returns the union of closed intervals i1 and i2.
(define interval-union
  (lambda (i1 i2)
    (if (interval-intersects? i1 i2)
        (list (list (min (car i1) (car i2))
                    (max (cadr i1) (cadr i2))))
        (list i1 i2))))

;; (make-vec-from-points p1 p2) - Returns a vector that goes from point p1 to
;; point p2. Assumes p1 and p2 have equal length.
(define make-vec-from-points
  (lambda (p1 p2)
    (if (null? p1)
        '()
        (cons (- (car p2) (car p1))
              (make-vec-from-points (cdr p1) (cdr p2))))))

;; (dot-product v1 v2) - Returns the dot product of vectors v1 and v2. Assumes
;; v1 and v2 have equal length.
(define dot-product
  (lambda (v1 v2)
    (if (null? v1)
        0
        (+ (* (car v1) (car v2))
           (dot-product (cdr v1) (cdr v2))))))

;; (vector-magnitude v) - Returns the magnitude of the vector v. Assumes the
;; magnitude of the vector is an integer.
(define vector-magnitude
  (lambda (v)
    (sqrt (vector-sum-squares v))))

;; (vector-sum-squares v) - Returns the sum of squares of the vector v. Helper
;; procedure for vector-magnitude.
(define vector-sum-squares
  (lambda (v)
    (if (null? v)
        0
        (+ (* (car v) (car v))
           (vector-sum-squares (cdr v))))))

;; (distance p1 p2) - Returns the distance between points p1 and p2. Assumes the
;; distance is an integer.
(define distance
  (lambda (p1 p2)
    (vector-magnitude (make-vec-from-points p1 p2))))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
