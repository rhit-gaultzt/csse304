;;;
;;; callcc.rkt
;;;
;;; by Zachary Gault
;;;
;;; Starter Code Provided by Rose-Hulman CSSE304
;;;
;;; This file contains my solutions to the Rose-Hulman CSSE304 Programming Language
;;; Concepts CallCC Assignment.

#lang racket

(provide make-retlam return jobs switch-job go-home)

(require racket/trace)


;; global for make-retlam
(define return-ks '())

;; (return retval) - Immediately returns from the calling procedure with the value
;; retval. Requires the calling procedure to be surrounded by make-retlam.
(define return
  (lambda (retval)
    (let ((return-k (first return-ks)))
      (set! return-ks (cdr return-ks))
      (return-k retval))))

;; (make-retlam lam) - Returns a procedure that allows return to be called within
;; the lambda.
(define make-retlam
  (lambda (lam)
    (lambda args
      (call/cc (lambda (k)
                 (set! return-ks (cons k return-ks))
                 (let ((result (apply lam args)))
                   (set! return-ks (cdr return-ks))
                   result))))))


;; globals for jobs
(define go-home-k #f)
(define current-job 0)
(define job-lambdas '#())
(define job-ks '#())

;; (go-home) - Causes all jobs to stop executing)
(define go-home
  (lambda ()
    (go-home-k)))

;;(switch-job index) - Stores the current continuation of the current job
;; and starts or continues the job specified by index
(define switch-job
  (lambda (index)
    (call/cc (lambda (k)
               (vector-set! job-ks current-job k)
               (set! current-job index)
               (let ((next-k (vector-ref job-ks index)))
                 (if next-k
                     (next-k)
                     ((vector-ref job-lambdas index))))))))

;;(jobs vars) - Where vars is a list of jobs, it starts by running the first
;; job and allows job to call switch-job to switch between jobs or go-home to
;; stop executing.
(define jobs
  (lambda vars
    (call/cc (lambda (k)
               (set! go-home-k k)
               (set! current-job 0)
               (set! job-lambdas (list->vector vars))
               (set! job-ks (list->vector (map (lambda (x) #f) vars)))
               ((first vars))))))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
