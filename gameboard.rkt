#lang racket/gui
(require "place.rkt")
(require "item.rkt")
(require "character.rkt")
(require "cmd_store.rkt")
(require "help_functions.rkt")
(require "canvasclasses.rkt")
(require "timer.rkt")



(provide (all-defined-out))

#|Initiate Andreas and some enemies to their own values in the matrix.|#

(define andreas
  (new character%
       [name '#(0)]
       [talk-line
        "I've quit with women and alcohol"]
       [description
        "Looks like a man that's going to save his second wife."]))

(define foot-soldier
  (new character%
       [name '#(90)]
       [talk-line
        "I'm evil?"]
       [description
        "It looks rather evil."]))

#|Initiate some items and power-ups to the world.|#

(define mushroom
  (new item%
       [name '#(20)]
       [description
        "The briefcase that can authorize a nuclear attack!"]
       [take 'Y]
       [error-description 'no-need]))


#|Sets up the worldview for Andreas and levels, each "matrix" slot will be defined as air, obstacles enemies etc
that can roll in and change depending on it's pixel coordinates.|#

#|The defined areas will have predetermined values on their borders. 10 squares for heigt and width
for the viewbox (i are rows, j are columns) and 10 squares height with 250 squares width for levels.|#

(define viewbox
  (new place%
       [name "Viewbox"]
       [description "The frame for what the player can see."]
       [board (build-vector
               10
               (lambda (i)
                 (build-vector
                  12
                  (lambda (j)
                    (vector
                     (modulo 1 10))))))]))

(define level1
  (new place%
       [name "Level 1"]
       [description "First level."]
       [board (build-vector
               10
               (lambda (i)
                 (build-vector
                  20
                  (lambda (j)
                    (vector
                     (cond
                       [(and (= j 0) (member i (enumerate 0 7 1)))    ;10 = Boundries for the map
                        10]
                       [(and (>= j 14) (member i (enumerate 0 7 1)))
                        10]
                       [(and (= i 0) (member j (enumerate 0 100 1)))
                        10]
                       [(and (>= i 8) (member j (enumerate 0 100 1)))  ;20 = obstacles, ground, platues
                        20]
                       [(and (= i 4) (member j '(7 8)))   
                        20]
                       [(and (= i 5) (member j '(5)))                 ;30 = enemies
                        30]
                       [(and (= i 7) (member j '(13)))
                        30]
                       [(and (= i 3) (member j '(8)))                 ;40 = power-ups
                        40]
                       [else
                        (modulo 1 10)]))))))]))


#|Temporary placeholder for testing some stuff|#

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

(define (andreas-pos row col)
  (let ([viewbox (send viewbox get-board)])
  (vector-set! (vector-ref viewbox row) col '#(666))
  viewbox))

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
    [(equal? col-val '#(40))
     (displayln "Power-up gained!")
     (send andreas inc-score-power)
     (vector-set! (vector-ref (send level1 get-board) row) lvl-col '#(1))]
    [(and (equal? col-val '#(30))
          (equal? arg 'fall))
     (displayln "ENEMY SLAIN")
     (send andreas inc-score-enemy)
     (vector-set! (vector-ref (send level1 get-board) row) lvl-col '#(1))]
    [(equal? col-val '#(30))
     (displayln "I LOSE LIFE!")]
    (else #t)))

(help-connect 0 0)

(andreas-pos 7 4)




(define game-window                              
  (new frame%
       [label "Andreas window"]
       [width 1080]
       [height 800]))

(send game-window show #t)





#|Graphic viewbox and background|#

(define level
  (new design-canvas%
       [parent game-window]
       [name "The entire level!"]
       [description "The blue background with red floor"]
       [paint-callback
        (lambda (canvas dc)
          (send level list-drawings dc 0 0 (send level list-elements viewbox))
          (send level draw-player dc 480 560))]
       [keyboard-handler
        (lambda (key-event)
          (when (eq? (send key-event get-key-code) 'up)
            (send physics-timer-jump start 32))
;          (when (eq? (send key-event get-key-code) 'down)
;            (send level move-ypos + 10))
;          (when (eq? (send key-event get-key-code) 'left)
;            (send level move-xpos - 10))
          (when (eq? (send key-event get-key-code) 'right)
            (send physics-timer-right start 16)))]))





#|Timers|#

(define (refresh-graphics)
  (send game-window refresh))

(define graphics-timer
  (new design-timer%
       [notify-callback refresh-graphics]))

(send graphics-timer start 32 #f)


#|Time function to control the jump in game.
Need 3 different parameters to make sure the
animation is performed correctly (time-up,
time-down and time-gate.|#

(define physics-timer-right
  (new design-timer%
       [notify-callback
        (lambda ()
          (let ([time-up (send physics-timer-right get-timer-up)])
            (send physics-timer-right inc-time)
            (send level move-xpos - 10)
            (when (= time-up 352)
              (send physics-timer-right stop)
              (move 'right)
              (send level move-xpos + 120)
              (send physics-timer-right reset-time))))]))


(define physics-timer-jump
  (new design-timer%
       [notify-callback
        (lambda ()
          (let ([time-up (send physics-timer-jump get-timer-up)]
                [time-down (send physics-timer-jump get-timer-down)]
                [time-gate (send physics-timer-jump get-timer-gate)])
            (when (= time-gate 2)                                      ;Ends the animation and resets the gate + time values
              (send physics-timer-jump close-gate)
              (send physics-timer-jump stop)
              (send physics-timer-jump reset-time))
            (when (= time-gate 1)                                      ;Sends the character down, change time-down for how long
              (send level move-ypos - 10)
              (send physics-timer-jump decr-time)
              (when (= time-down 320)
                (send physics-timer-jump open-gate-2)))
            (when (= time-gate 0)                                      ;Sends the character up in the air, change parameter time-up for how long.
              (send level move-ypos + 10)
              (send physics-timer-jump inc-time)
              (when (= time-up 320)
                (send physics-timer-jump open-gate-1)))))]))


#|Testing some stuff|#



     


















