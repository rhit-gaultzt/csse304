;;;
;;; A07.rkt
;;;
;;; by Zachary Gault
;;;
;;; This file contains my solutions to the Rose-Hulman CSSE304 Programming Language
;;; Concepts Assignment 7.

#lang racket

(provide group-by-two group-by-n bt-leaf-sum bt-inorder-list bt-max bt-max-interior slist-map slist-reverse slist-paren-count slist-depth slist-symbols-at-depth path-to make-c...r)

;; (group-by-two ls) - Returns a list of lists that groups the elements of ls into
;; groups of 2. If ls has an odd number of elements, the last sublist will have 1
;; element.
(define group-by-two
  (lambda (ls)
    (cond [(null? ls) '()]
          [(null? (cdr ls)) (list (list (first ls)))]
          [else (cons (list (first ls) (second ls))
                      (group-by-two (cddr ls)))])))

;; (group-by-n ls n) - Returns a list of lists that groups the elements of ls into
;; groups of size n. If the length of ls is not a multiple of n, the last sublist
;; will have less than n elements.
(define group-by-n
  (lambda (ls n)
    (if (null? ls)
        '()
        (let group-recur ([ls ls] [n n] [left n] [cur '()])
          (cond [(null? ls) (list cur)]
                [(> left 0)
                 (group-recur (cdr ls) n (sub1 left) (append cur (list (first ls))))]
                [else
                 (cons cur (group-recur (cdr ls) n (sub1 n) (list (first ls))))])))))

;; (bt-leaf-sum T) - Returns the sum of all the numbers in the leaves of the bintree
;; T.
(define bt-leaf-sum
  (lambda (T)
    (if (integer? T)
        T
        (+ (bt-leaf-sum (second T))
           (bt-leaf-sum (third T))))))

;; (bt-inorder-list T) - Returns a list of the symbols from the interior nodes of T
;; if visited in an inorder traversal.
(define bt-inorder-list
  (lambda (T)
    (if (integer? T)
        '()
        (append (bt-inorder-list (second T))
                (list (first T))
                (bt-inorder-list (third T))))))
        
;; (bt-max T) - Returns the largest integer in the bintree T.
(define bt-max
  (lambda (T)
    (if (integer? T)
        T
        (max (bt-max (second T))
             (bt-max (third T))))))

;; (bt-max-interior T) - Returns the leftmost interior node of T with a maximal leaf
;; sum. Assumes T is a bintree with at least 1 interior node.
(define bt-max-interior
  (lambda (T)
    (first (let recur ([T T])
             (if (integer? T)
                 (list '() T T)
                 (let* ([left-result (recur (second T))]
                        [right-result (recur (third T))]
                        [cur-sum (+ (second left-result)
                                    (second right-result))]
                        [max-sum (max cur-sum
                                      (if (null? (first left-result))
                                          cur-sum
                                          (third left-result))
                                      (if (null? (first right-result))
                                          cur-sum
                                          (third right-result)))]
                        [max-symbol
                         (cond [(and (eqv? max-sum (third left-result))
                                     (not (null? (first left-result))))
                                (first left-result)]
                               [(or (eqv? max-sum cur-sum)
                                    (null? (first right-result)))
                                (first T)]
                               [else (first right-result)])])
                   (list max-symbol cur-sum max-sum)))))))
          
;; (slist-map proc slist) - Returns a nested list of the shape of slist where proc is
;; applied to each symbol.
(define slist-map
  (lambda (proc slist)
    (cond [(null? slist) '()]
          [(symbol? (first slist))
           (cons (proc (first slist))
                 (slist-map proc (cdr slist)))]
          [else
           (cons (slist-map proc (first slist))
                 (slist-map proc (cdr slist)))])))

;; (slist-reverse slist) - Returns a reversed version of slist.
(define slist-reverse
  (lambda (slist)
    (cond [(null? slist) '()]
          [(symbol? (first slist))
           (append (slist-reverse (cdr slist))
                   (list (first slist)))]
          [else
           (append (slist-reverse (cdr slist))
                   (list (slist-reverse (first slist))))])))
          
;; (slist-paren-count slist) - Returns the number of parenthesis needed to
;; print slist.
(define slist-paren-count
  (lambda (slist)
    (cond [(null? slist) 2]
          [(symbol? (first slist))
           (slist-paren-count (cdr slist))]
          [else (+ (slist-paren-count (first slist))
                   (slist-paren-count (cdr slist)))])))

;; (slist-depth slist) - Returns the maximum nesting level of slist.
(define slist-depth
  (lambda (slist)
    (cond [(null? slist) 1]
          [(symbol? (first slist))
           (slist-depth (cdr slist))]
          [else (max (add1 (slist-depth (first slist)))
                     (slist-depth (cdr slist)))])))

;; (slist-symbols-at-depth slist d) - Returns a list of all the symbols of
;; slist that are at a nesting level of d.
(define slist-symbols-at-depth
  (lambda (slist d)
    (let symbol-depth-recur ([slist slist] [d d] [cur-d 1])
      (cond [(null? slist) '()]
            [(symbol? (first slist))
             (if (eqv? cur-d d)
                 (cons (first slist)
                       (symbol-depth-recur (cdr slist) d cur-d))
                 (symbol-depth-recur (cdr slist) d cur-d))]
            [else (append (symbol-depth-recur (first slist) d (add1 cur-d))
                          (symbol-depth-recur (cdr slist) d cur-d))]))))
             
;; (path-to slist sym) - Returns a list of cars and cdrs that take us to the
;; position of the leftmost occurrance of sym in slist. Returns #f if sym is
;; not in slist.
(define path-to
  (lambda (slist sym)
    (let path-to-recur ([slist slist] [sym sym] [path '()])
      (cond [(null? slist) #f]
            [(symbol? (first slist))
             (if (eqv? (first slist) sym)
                 (reverse (cons 'car path))
                 (path-to-recur (cdr slist) sym (cons 'cdr path)))]
            [else (or (path-to-recur (first slist) sym (cons 'car path))
                      (path-to-recur (cdr slist) sym (cons 'cdr path)))]))))

;; (make-c...r str) - Returns a procedure c<str>r that functions like the
;; pre-defined c...r procedures.
(define make-c...r
  (lambda (str)
    (apply compose (map (lambda (char)
                          (cond [(equal? char #\a) car]
                                [(equal? char #\d) cdr]
                                [else 'invalid-char]))
                        (string->list str)))))
                   

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
