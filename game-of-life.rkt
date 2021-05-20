#lang racket

(require rackunit)
(require rackunit/text-ui) 

(define EMPTY_CELL '\ )



(define (make-world rows cols)
  (define (make-row cols)
    (if (= 0 cols)
        '(EMPTY_CELL)
        (cons '() (make-row (- cols 1)))))
  (if (= 0 rows)
      '()
      (cons (make-row cols) (make-world (- rows 1) cols))))




(define game-of-life-tests 
  (test-suite "make world"
              (test-case "make empty world"
                         (check-equal? (make-world 0 0) '() ))
              (test-case "make empty world 2x2"
                         (check-equal? (make-world 2 2) '( (() ()) (() ()) ) ))
              )

  )
   
(run-tests game-of-life-tests)

