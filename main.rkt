#lang racket/gui

(require "game-of-life.rkt")


(require 2htdp/image)
(require 2htdp/universe)


(define dead-cell (square 15 "solid" "white"))
(define live-cell (square 15 "solid" "black"))


(define (make-cube cell)
  (if (is-live? cell)
      live-cell
      dead-cell))

(define (convert-row-to-cubes row)
  (if (equal? (cdr row) '())
      (make-cube (car row))
      (beside (make-cube (car row))
              (convert-row-to-cubes (cdr row)))))


(define (convert-world-to-grid rest-of-world)
  (if (equal? (cddr rest-of-world) '())
      (convert-row-to-cubes (cadr rest-of-world))
      (above (convert-row-to-cubes (car rest-of-world))
             (convert-world-to-grid (cdr rest-of-world)))))


(define world (make-world 50))
(set! world (add-pulsar world 30 30))
(set! world (add-glider world 3 3))
(set! world (add-glider world 30 20))
(set! world (add-glider world 15 25))
(set! world (add-tub world 30 10))
(set! world (add-pentadecathlon world 5 35))


(define (play-game time)
  (set! world (goto-next-gen world))
  (underlay/xy (rectangle 770 750 "solid" "blue")
               10 10
               (convert-world-to-grid world)))

(animate play-game)