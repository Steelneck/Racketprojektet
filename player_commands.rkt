#lang racket
(require "cmd_store.rkt")
(require "place.rkt")
(require "character.rkt")
(require "item.rkt")
(require "gameboard.rkt")
(require "graphic_objects.rkt")
(require "canvasclasses.rkt")

(provide (all-defined-out))



#|Connects the level to the viewbox, n parameter for how many elements and rows
while k parameter will drop one column at a time.|#

(define (help-connect n k)
  (cond
    ((= n 10)
     (send viewbox get-board))
    (else
     (vector-copy!
      (vector-ref (send viewbox get-board) n) 0
      (vector-take
       (vector-drop
        (vector-ref (send level1 get-board) n) k) 10))
     (help-connect (+ n 1) k))))

#|Simple function to make sure Andreas stay in the same position
in the viewbox.|#

(define (andreas-pos row col)
  (let ([viewbox (send viewbox get-board)])
  (vector-set! (vector-ref viewbox row) col '#(0))
  viewbox))

#|Lets the figure Andreas to stay in the same column on the viewbox but
moves around in the world. Lvl-col is needed to change the level when power-ups
are taken or enemies destroyed.|#

(define move
  (let ([k 0]
        [row 7]
        [col 4]
        [lvl-col 4])
    (lambda arg
      (let ([arg (car arg)])
      (cond
        [(null? arg)
         display "What direction?"]
        [(and (eqv? arg 'jump)
              (collision? (- row 1) col lvl-col arg))
         (set! row (- row 1))
         (help-connect 0 k)
         (andreas-pos row col)]
        [(and (eqv? arg 'fall)
              (collision? (+ row 1) col lvl-col arg))
         (set! row (+ row 1))
         (help-connect 0 k)
         (andreas-pos row col)]
        [(and (eqv? arg 'right)
              (collision? row (+ col 1) (+ lvl-col 1) arg))
         (set! lvl-col (+ lvl-col 1))
         (set! k (+ k 1))
         (help-connect 0 k)
         (andreas-pos row col)]
        [else
         (help-connect 0 k)
         (andreas-pos row col)])))))

#|A function that checks for collisions and handles them.|#

(define (collision? row col lvl-col arg)
  (let ([col-val (vector-ref
        (vector-ref (send viewbox get-board) row)
        col)])
  (cond
    [(false?
      (equal?
       col-val
       '#(1)))
     (handle-collision col-val row col lvl-col arg)])))

#|The function that handles all possible collisions in the desired way.|#

(define (handle-collision col-val row col lvl-col arg)
  (cond
    [(or (equal? col-val '#(10))
         (equal? col-val '#(20)))
     #f]
    [(equal? col-val '#(30))
     (displayln "Power-up gained!")
     (send andreas inc-score-power)
     (vector-set! (vector-ref (send level1 get-board) row) lvl-col '#(1))]
    [(and (equal? col-val '#(30))
          (equal? arg 'fall))
     (displayln "ENEMY SLAIN")
     (send andreas inc-score-enemy)
     (vector-set! (vector-ref (send level1 get-board) row) lvl-col '#(1))]
    [(equal? col-val '#(30))
     (displayln "I LOSE LIFE!")
     (send andreas lose-life)]
    (else #t)))





