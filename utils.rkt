#lang racket

(provide append)
(provide duplicate)

(define (duplicate lst)
  (if (null? lst)
      '()
      (cons (car lst) (duplicate (cdr lst)))))


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))