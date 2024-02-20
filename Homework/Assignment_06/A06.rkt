;;;
;;; A06.rkt
;;;
;;; by Zachary Gault
;;;
;;; This file contains my solutions to the Rose-Hulman CSSE304 Programming Language
;;; Concepts Assignment 6.

#lang racket

(provide curry2 curried-compose compose make-list-c reverse-it map-by-position empty-BST
         empty-BST? BST-insert BST-inorder BST? BST-element BST-left BST-right
         BST-insert-nodes BST-contains? BST-height let->application let*->let qsort
         sort-list-of-symbols)

;; (curry2 fn) - Returns a curried version of the 2 argument procedure proc that
;; takes the first argument of proc and returns a procedure that takes the second
;; argument.
(define curry2
  (lambda (proc)
    (lambda (arg1)
      (lambda (arg2)
        (proc arg1 arg2)))))

;; (curried-compose proc1) - Returns a curried version of proc1 that takes a procedure
;; as an argument which is used to create a composed procedure.
(define curried-compose
  (lambda (proc1)
    (lambda (proc2)
      (lambda (arg)
        (proc1 (proc2 arg))))))

;; (compose list-of-functions) - Returns a composed procedure of the 2 or 3 procedures in
;; list-of-functions.
(define compose
  (lambda list-of-functions
    (if (null? (cddr list-of-functions))
        (lambda (arg)
          ((car list-of-functions)
           ((cadr list-of-functions) arg)))
        (lambda (arg)
          ((car list-of-functions)
           ((cadr list-of-functions)
            ((caddr list-of-functions) arg)))))))
          

;; (make-list-c count) - Returns a procedure that takes an element and returns a list
;; of that element repeated count times.
(define make-list-c
  (lambda (count)
    (lambda (el)
      (let recur ([count count] [el el])
        (if (positive? count)
            (cons el (recur (sub1 count) el))
            '())))))

;; (reverse-it lst) - Returns a list that is the reverse of lst.
(define reverse-it
  (lambda (lst)
    (if (null? lst)
        '()
        (append (reverse-it (cdr lst)) (list (car lst))))))

;; (map-by-position fn-list arg-list) - Returns a list of outputs from the outputs of
;; each one parameter procedure in fn-list is given the corresponding parameter in
;; arg-list.
(define map-by-position
  (lambda (fn-list arg-list)
    (map (lambda (fn arg) (fn arg)) fn-list arg-list)))

;; (empty-BST) - returns an empty BST.
(define empty-BST
  (lambda ()
    '()))

;; (empty-BST? obj) - Returns a boolean indicating whether obj is an empty BST.
(define empty-BST?
  (lambda (obj)
    (equal? (empty-BST) obj)))

;; (BST-insert num bst) - Returns a BST with num inserted into the BST bst.
(define BST-insert
  (lambda (num bst)
    (cond [(empty-BST? bst) (list num '() '())]
          [(= num (BST-element bst)) bst]
          [(< num (BST-element bst))
           (list (BST-element bst)
                 (BST-insert num (BST-left bst))
                 (BST-right bst))]
          [else (list (BST-element bst)
                      (BST-left bst)
                      (BST-insert num (BST-right bst)))])))

;; (BST-inorder bst) - Returns a list representing an inorder traversal of the BST bst.
(define BST-inorder
  (lambda (bst)
    (if (empty-BST? bst)
        '()
        (append (BST-inorder (BST-left bst))
                (list (BST-element bst))
                (BST-inorder (BST-right bst))))))

;; (BST? obj) - Returns a boolean indicating whether obj is a BST.
(define BST?
  (lambda (obj)
    (let bst-recur ([obj obj] [preds '()] [nums '()])
      (cond [(empty-BST? obj) #t]
            [(not (list? obj)) #f]
            [(not (number? (BST-element obj))) #f]
            [(not (andmap
                   (lambda (pred num)
                     (pred (BST-element obj) num))
                   preds nums)) #f]
            [(null? (cdr obj)) #f]
            [(null? (cddr obj)) #f]
            [(not (null? (cdddr obj))) #f]
            [else (and (bst-recur (BST-left obj)
                                  (cons < preds)
                                  (cons (BST-element obj) nums))
                       (bst-recur (BST-right obj)
                                  (cons > preds)
                                  (cons (BST-element obj) nums)))]))))

;; (BST-element bst) - Returns the element at the top level node of the BST bst.
(define BST-element
  (lambda (bst)
    (car bst)))

;; (BST-left bst) - Returns the left subtree BST of the BST bst.
(define BST-left
  (lambda (bst)
    (cadr bst)))

;; (BST-right bst) - Returns the right subtree BST of the BST bst.
(define BST-right
  (lambda (bst)
    (caddr bst)))

;; (BST-insert-nodes bst nums)- Returns a BST with all of the elements of the list
;; of numbers nums inserted into the BST bst in order.
(define BST-insert-nodes
  (lambda (bst nums)
    (if (null? nums)
        bst
        (BST-insert-nodes (BST-insert (car nums) bst)
                          (cdr nums)))))

;; (BST-contains? bst num) - Returns a boolean indicating whether the BST bst
;; contains the number num.
(define BST-contains?
  (lambda (bst num)
    (cond [(empty-BST? bst) #f]
          [(eqv? num (BST-element bst)) #t]
          [(< num (BST-element bst))
           (BST-contains? (BST-left bst) num)]
          [else (BST-contains? (BST-right bst) num)])))

;; (BST-height bst) - Returns the height of the BST bst.
(define BST-height
  (lambda (bst)
    (if (empty-BST? bst)
        -1
        (add1 (max (BST-height (BST-left bst))
                   (BST-height (BST-right bst)))))))

;; (let->application let-expression) - Returns an equivalent expression to
;; let-expression replacing the most outer let with a lambda expression.
(define let->application
  (lambda (let-expression)
    (cons (list 'lambda
                (map first (cadr let-expression))
                (caddr let-expression))
          (map second (cadr let-expression)))))

;; (let*->let let*-expression) - Returns an equivalent expression to
;; let*-expression replacing the most outer let* with a nested let.
(define let*->let
  (lambda (let*-expression)
    (let recur ([param_names (map first (cadr let*-expression))]
                [param_vals (map second (cadr let*-expression))]
                [body (caddr let*-expression)])
      (if (null? param_names)
          body
          (list 'let
                (list (list(car param_names) (car param_vals)))
                (recur (cdr param_names) (cdr param_vals) body))))))
      
;; (qsort pred ls) - Returns a list which is a sorted version of the list ls
;; based on the predicate pred.
(define qsort
  (lambda (pred ls)
    (if (null? ls)
        '()
        (let partition-recur ([pivot (car ls)]
                              [ls (cdr ls)]
                              [less '()]
                              [greater '()])
          (cond [(null? ls)
                 (append (qsort pred less)
                         (list pivot)
                         (qsort pred greater))]
                [(pred (car ls) pivot)
                 (partition-recur pivot (cdr ls) (cons (car ls) less) greater)]
                [else (partition-recur pivot (cdr ls) less (cons (car ls) greater))])))))
            

;; (sort-list-of-symbols los) - Returns a list of the same symbols as los
;; which are sorted as if they were strings.
(define sort-list-of-symbols
  (lambda (los)
    (sort los (lambda (s1 s2)
                (string<? (symbol->string s1)
                          (symbol->string s2))))))


;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
