#|------------------------------------------------------------------------------
                             UTIL ALGORITHMS

  In this module some algorithms are implemented that may be useful for the project.

  Some algorithms implemented are:
    * apply-fun
    * sort-by-weight

  Developed by @estalvgs1999
------------------------------------------------------------------------------|#
#lang racket

(require "../logic-scr/graph.rkt")
(provide apply-fun
         sort-by-weight)

;; (apply-fun f list)
;; Applies a function to elements of a list
;; [ Map equivalent ]
> (define (apply-fun f list)
    (cond ((null? list)
            '())
          (else (cons (f (car list))
                      (apply-fun f (cdr list))))))

;; (sort-by-weight list graph)
;; Order all routes from least to greatest according to your weight.
;; Implemented as a quick sort
(define (sort-by-weight list_ graph)
  (cond ((null? list_)
          '())
        (else
          (append (sort-by-weight (less-than (car list_) (cdr list_) graph) graph)
                  (list (car list_))
                  (sort-by-weight (greater-than (car list_) (cdr list_) graph) graph)))))

(define (less-than pivot list_ graph)
  (cond ((null? list_)
          '())
        ((> (path-weight pivot graph)
            (path-weight (car list_) graph))
          (cons (car list_) (less-than pivot (cdr list_) graph)))
        (else
          (less-than pivot (cdr list_) graph))))

(define (greater-than pivot list_ graph)
  (cond ((null? list_)
          '())
        ((<= (path-weight pivot graph)
             (path-weight (car list_) graph))
          (cons (car list_) (greater-than pivot (cdr list_) graph)))
        (else
          (greater-than pivot (cdr list_) graph))))
