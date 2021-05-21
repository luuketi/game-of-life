#lang racket

(require rackunit)
(require rackunit/text-ui)

(require "game-of-life.rkt")

(define game-of-life-tests           
  (test-suite "make world"
              (test-case "make empty world"
                         (check-equal? (make-world 0) '() ))
              (test-case "make empty world 2x2"
                         (check-equal? (make-world 2) '( (D D)
                                                         (D D) )  ))
              (test-case "live cell in the middle for 3x3 world"
                         (let [(world (make-world 3))]
                           (set! world (change-cell world 2 2 LIVE_CELL))
                           (check-equal? world    '( (D D D)
                                                     (D L D)
                                                     (D D D))  )))
              (test-case "kill cell in the middle for 3x3 world"
                         (let [(world (make-world 3))]
                           (set! world (change-cell world 2 2 LIVE_CELL))
                           (set! world (change-cell world 2 2 DEAD_CELL))
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
              (test-case "blinker - two generations"
                         (let [(world (make-world 3))]
                           (set! world (reverse-cell world 1 2))
                           (set! world (reverse-cell world 2 2))
                           (set! world (reverse-cell world 3 2))
                           (set! world (goto-next-gen world))
                           (set! world (goto-next-gen world))
                           (check-equal? world    '( (D L D)
                                                     (D L D)
                                                     (D L D) ) )))
              (test-case "block - three generations"
                         (let [(world (make-world 4))]
                           (set! world (reverse-cell world 2 2))
                           (set! world (reverse-cell world 2 3))
                           (set! world (reverse-cell world 3 2))
                           (set! world (reverse-cell world 3 3))
                           (set! world (goto-next-gen world))
                           (check-equal? world    '( (D D D D)
                                                     (D L L D)
                                                     (D L L D)
                                                     (D D D D) ) )
                           (set! world (goto-next-gen world))
                           (check-equal? world    '( (D D D D)
                                                     (D L L D)
                                                     (D L L D)
                                                     (D D D D) ) )
                           (set! world (goto-next-gen world))
                           (check-equal? world    '( (D D D D)
                                                     (D L L D)
                                                     (D L L D)
                                                     (D D D D) ) ) ))
              (test-case "add pattern: tub"
                         (let [(world (make-world 5))]
                           (set! world (add-tub world 2 2))
                           (set! world (goto-next-gen world))
                           (check-equal? world    '( (D D D D D)
                                                     (D D L D D)
                                                     (D L D L D)
                                                     (D D L D D)
                                                     (D D D D D)) ) ))
              (test-case "add pattern: glider"
                         (let [(world (make-world 5))]
                           (set! world (add-glider world 2 2))
                           (check-equal? world    '( (D D D D D)
                                                     (D D L D D)
                                                     (D D D L D)
                                                     (D L L L D)
                                                     (D D D D D)) ) ))
              (test-case "add pattern: pentadecathlon"
                         (let [(world (make-world 14))]
                           (set! world (add-pentadecathlon world 2 2))
                           (check-equal? world    '( (D D D D D D D D D D D D D D)
                                                     (D L L L D D D D D D D D D D)
                                                     (D D L D D D D D D D D D D D)
                                                     (D D L D D D D D D D D D D D)
                                                     (D L L L D D D D D D D D D D)
                                                     (D D D D D D D D D D D D D D)
                                                     (D L L L D D D D D D D D D D)
                                                     (D L L L D D D D D D D D D D)
                                                     (D D D D D D D D D D D D D D)
                                                     (D L L L D D D D D D D D D D)
                                                     (D D L D D D D D D D D D D D)
                                                     (D D L D D D D D D D D D D D)
                                                     (D L L L D D D D D D D D D D)
                                                     (D D D D D D D D D D D D D D)) ) ))
              (test-case "add pattern: pulsar"
                         (let [(world (make-world 14))]
                           (set! world (add-pulsar world 1 1))
                           (check-equal? world    '( (D D L L L D D D L L L D D D)
                                                     (D D D D D D D D D D D D D D)
                                                     (L D D D D L D L D D D D L D)
                                                     (L D D D D L D L D D D D L D)
                                                     (L D D D D L D L D D D D L D)
                                                     (D D L L L D D D L L L D D D)
                                                     (D D D D D D D D D D D D D D)
                                                     (D D L L L D D D L L L D D D)
                                                     (L D D D D L D L D D D D L D)
                                                     (L D D D D L D L D D D D L D)
                                                     (L D D D D L D L D D D D L D)
                                                     (D D D D D D D D D D D D D D)
                                                     (D D L L L D D D L L L D D D)
                                                     (D D D D D D D D D D D D D D)) ) ))
              ))
   
(run-tests game-of-life-tests)