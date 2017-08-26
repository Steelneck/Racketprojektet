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
                  10
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
                       [(and (= j 0) (member i (enumerate 0 9 1)))   ;100 = Boundries for the map
                        100]
                       [(and (>= j 14) (member i (enumerate 0 9 1)))
                        100]
                       [(and (= i 0) (member j (enumerate 0 20 1)))
                        100]
                       [(and (>= i 8) (member j (enumerate 0 20 1)))  ;50 = obstacles, ground, platues
                        50]
                       [(and (= i 4) (member j '(7 8)))   
                        50]
                       [(and (= i 5) (member j '(5)))                ;90 = enemies
                        90]
                       [(and (= i 7) (member j '(13)))
                        90]
                       [(and (= i 3) (member j '(8)))                ;20 = power-ups
                        20]
                       [else
                        (modulo 1 10)]))))))]))



(define game-window                              
  (new frame%
       [label "Andreas window"]
       [width 1200]
       [height 800]))

(define nothing-window
  (new frame%
       [label "Nothing"]))

(send game-window show #t)



#|Test drawing board|#

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



#|Graphic viewbox and background|#

(define level
  (new design-canvas%
       [parent game-window]
       [name "The entire level!"]
       [description "The blue background with red floor"]
       [paint-callback
        (lambda (canvas dc)
          (send level canvasobjects dc))]
       [keyboard-handler
        (lambda (key-event)
          (when (eq? (send key-event get-key-code) 'up)
            (send physics-timer-jump start 32))
          (when (eq? (send key-event get-key-code) 'down)
            (send level move-ypos + 10))
          (when (eq? (send key-event get-key-code) 'left)
            (send level move-xpos - 10))
          (when (eq? (send key-event get-key-code) 'right)
            (send level move-xpos + 10)))]))


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
              (send level move-ypos + 10)
              (send physics-timer-jump decr-time)
              (when (= time-down 640)
                (send physics-timer-jump open-gate-2)))
            (when (= time-gate 0)                                      ;Sends the character up in the air, change parameter time-up for how long.
              (send level move-ypos - 10)
              (send physics-timer-jump inc-time)
              (when (= time-up 640)
                (send physics-timer-jump open-gate-1)))))]))


  


    