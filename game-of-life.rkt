#lang racket

(require rackunit)
(require rackunit/text-ui)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))


(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))


(define DEAD_CELL 0 )
(define LIVE_CELL 1 )


(define (make-world rows)
  (define (make-row cols)
    (if (= 0 cols)
        '()
        (cons 'D (make-row (- cols 1)))))
  (define (make-aa rows cols)
    (if (= 0 rows)
        '()
        (cons (make-row cols) (make-aa (- rows 1) cols))))
  (make-aa rows rows))


(define (live-cell world row col)

  (define (live-cell-col row acc-col)
    (if (= acc-col col)
        (append (list 'L) (cdr row))
        (append (list (car row)) (live-cell-col (cdr row) (+ 1 acc-col)))))

  (define (live-cell-rec world acc-row)
    (if (= acc-row row)
        (cons (live-cell-col (car world) 1) (cdr world))
        (cons (car world) (live-cell-rec (cdr world) (+ 1 acc-row)))))

  (live-cell-rec world 1))




(define game-of-life-tests 
  (test-suite "make world"
              (test-case "make empty world"
                         (check-equal? (make-world 0) '() ))
              (test-case "make empty world 2x2"
                         (check-equal? (make-world 2) '( (D D)
                                                         (D D) )  ))
              (test-case "live cell in the middle for 3x3 world"
                         (let [(world (make-world 3))]
                         (set! world (live-cell world 2 2))
                         (check-equal? world    '( (D D D)
                                                   (D L D)
                                                   (D D D))  ))

              )

  ))
   
(run-tests game-of-life-tests)

