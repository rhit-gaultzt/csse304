#lang racket

(define occurs-bound?
  (lambda (var lc-exp)
    (cond
      ((symbol? lc-exp) #f)
      ((eqv? (first lc-exp) 'lambda)
       (or (occurs-bound? var (third lc-exp))
           (and (eqv? (caadr lc-exp) var)
                (occur-free? var (second lc-exp)))))
      (else (or (occurs-bound? var (first lc-exp))
                (occurs-bound? var (second lc-exp)))))))
