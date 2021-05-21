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


(define (change-cell world row col state)

  (define (change-cell-col row acc-col)
    (if (= acc-col col)
        (append (list state) (cdr row))
        (append (list (car row)) (change-cell-col (cdr row) (+ 1 acc-col)))))

  (define (change-cell-rec world acc-row)
    (if (= acc-row row)
        (cons (change-cell-col (car world) 1) (cdr world))
        (cons (car world) (change-cell-rec (cdr world) (+ 1 acc-row)))))

  (change-cell-rec world 1))


(define (get-cell-value world row col)
  (define (get-cell-col row acc-col)
    (if (= acc-col col)
        (car row)
        (get-cell-col (cdr row) (+ 1 acc-col))))

  (define (get-cell-rec world acc-row)
    (if (= acc-row row)
        (get-cell-col (car world) 1) 
        (get-cell-rec (cdr world) (+ 1 acc-row))))

  (get-cell-rec world 1))


(define (reverse-cell world row col)
  (let* [(cell-value (get-cell-value world row col))
         (reversed-cell (if (equal? cell-value 'L) 'D 'L))]
    (define (change-cell-col row acc-col)
      (if (= acc-col col)
          (append (list reversed-cell) (cdr row))
          (append (list (car row)) (change-cell-col (cdr row) (+ 1 acc-col)))))

    (define (change-cell-rec world acc-row)
      (if (= acc-row row)
          (cons (change-cell-col (car world) 1) (cdr world))
          (cons (car world) (change-cell-rec (cdr world) (+ 1 acc-row)))))

    (change-cell-rec world 1)))


(define (get-world-size world)
  (length (car world)))


(define (generate-neighbour-cells row col world-size)

  (define (gen-cells cell)
    (cons (+ row (car cell)) (+ col (cadr cell))))

  (define (filter-cells cell)
    (let [(row (car cell))
          (col (cdr cell))]
      (and (>= row 1)
           (and (>= col 1)
                (and (<= row world-size)
                     (<= col world-size))))))
  
  (let* [(neighbours '((-1 -1) (-1 0) (-1 +1) (0 -1) (0 +1) (+1 -1) (+1 0) (+1 +1)))
         (cells (map gen-cells neighbours))
         (filtered-cells (filter filter-cells cells))]
    
    filtered-cells))


(define (eval-cell world row col)

  (define (get-cell cell)
    (get-cell-value world (car cell) (cdr cell)))

  (define (is-live? cell) 
    (equal? 'L cell))

  (let* [(world-size (get-world-size world))
         (neighbours (generate-neighbour-cells row col world-size))
         (neighbours-values (map get-cell neighbours))
         (number-of-live-neighbours (count is-live? neighbours-values))
         (cell-value (get-cell-value world row col))]
    (cond [(and (equal? 'L cell-value) (< number-of-live-neighbours 2) 'D)]
          [(and (equal? 'L cell-value) (or (= number-of-live-neighbours 2)
                                           (= number-of-live-neighbours 3)) 'L)]
          [(and (equal? 'L cell-value) (> number-of-live-neighbours 2) 'D)]
          [(and (equal? 'D cell-value) (= number-of-live-neighbours 3) 'L)]
          [else cell-value])
    ))

(define (goto-next-gen world)

  (define (goto-next-gen-row row acc-row acc-col)
    (if (equal? (cdr row) '())
        (list (eval-cell world acc-row acc-col))
        (append
         (list (eval-cell world acc-row acc-col))
         (goto-next-gen-row (cdr row) acc-row (+ 1 acc-col)))))

  (define (goto-next-gen-rec rest-of-world acc-row acc-col)
    (if (equal? (cdr rest-of-world) '())
        (cons (goto-next-gen-row (car rest-of-world) acc-row acc-col) '())
        (cons
         (goto-next-gen-row (car rest-of-world) acc-row acc-col)
         (goto-next-gen-rec (cdr rest-of-world) (+ 1 acc-row) acc-col))))

  (goto-next-gen-rec world 1 1))
  



(define game-of-life-tests           
  (test-suite "make world"
              (test-case "make empty world"
                         (check-equal? (make-world 0) '() ))
              (test-case "make empty world 2x2"
                         (check-equal? (make-world 2) '( (D D)
                                                         (D D) )  ))
              (test-case "live cell in the middle for 3x3 world"
                         (let [(world (make-world 3))]
                           (set! world (change-cell world 2 2 'L))
                           (check-equal? world    '( (D D D)
                                                     (D L D)
                                                     (D D D))  )))
              (test-case "kill cell in the middle for 3x3 world"
                         (let [(world (make-world 3))]
                           (set! world (change-cell world 2 2 'L))
                           (set! world (change-cell world 2 2 'D))
                           (check-equal? world    '( (D D D)
                                                     (D D D)
                                                     (D D D))  )))
              (test-case "reverse cell in the corner for 3x3 world"
                         (let [(world (make-world 3))]
                           (set! world (reverse-cell world 3 1))
                           (check-equal? world    '( (D D D)
                                                     (D D D)
                                                     (L D D))  )))
              (test-case "blinker - one generation"
                         (let [(world (make-world 3))]
                           (set! world (reverse-cell world 1 2))
                           (set! world (reverse-cell world 2 2))
                           (set! world (reverse-cell world 3 2))
                           (set! world (goto-next-gen world))
                           (check-equal? world    '( (D D D)
                                                     (L L L)
                                                     (D D D) ) )))



              ))
   
(run-tests game-of-life-tests)

