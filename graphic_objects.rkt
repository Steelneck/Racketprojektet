#lang racket/gui
#|


(require "place.rkt")
(require "character.rkt")
(require "canvasclasses.rkt")
(require "timer.rkt")
(require "gameboard.rkt")
(provide (all-defined-out))

(define game-window                              
  (new frame%
       [label "Andreas window"]
       [width 1200]
       [height 800]))

(define input-window
  (new frame%
       [label "Nothing"]))

(send game-window show #t)



#|Test drawing board|#

(define (draw-map canvas dc)
  (let ([xtile (send level get-xtile)]
        [ytile (send level get-ytile)])
    (send dc set-brush "Red" 'solid)
    (send dc draw-rectangle 0 0 100 100)))


#|Original drawing|#

(define (draw-map1 canvas dc)
  (let ([xpos (send level get-xpos)]
        [ypos (send level get-ypos)])
  (send dc set-brush "DodgerBlue" 'solid)
  (send dc draw-rectangle 0 0 1200 800)      
  (send dc set-brush "DarkRed" 'solid)
  (send dc draw-rectangle 0 640 1200 640)
  (send dc translate xpos ypos)
  (send dc set-brush "red" 'solid)
  (send dc draw-ellipse 400 600 40 40)
  (send dc translate (- xpos) (- ypos))))


#|Graphic viewbox and background|#

(define level
  (new design-canvas%
       [parent game-window]
       [name "The entire level!"]
       [description "The blue background with red floor"]
       [paint-callback draw-map]
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
                (send physics-timer-jump open-gate-1)))))]))|#























