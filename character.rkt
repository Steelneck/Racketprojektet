#lang racket
(provide character%)
(provide (all-defined-out))


#|Template for characters.|#

(define character%
  (class object%
    (init-field name
                talk-line
                description
                [character-value 0]
                [character-life 3]
                [player-xpos 480]
                [player-ypos 560]
                [enemy-xpos 0]
                [enemy-ypos 0]
                [x-size 120]
                [y-size 80]
                [character-location null])

    (define/public (get-name)                                ;Characters name, description, location, talk-line.
      name)
    
    (define/public (get-description)
      description)
    
    (define/public (get-place)
      character-location)
    
    (define/public (get-talk-line)
      talk-line)

    (define/public (get-value)
      character-value)

    (define/public (dead?)
      (= character-life 0))

    (define/public (get-life)
      character-life)

    (define/public (gain-life)
      (set! character-life (+ character-life 1)))

    (define/public (lose-life)
      (set! character-life (- character-life 1)))

    (define/public (inc-score-enemy)
      (set! character-value (+ character-value 1)))

    (define/public (inc-score-power)
      (set! character-value (+ character-value 5)))

    
    
    #|Attempts of handling graphic collision|#
    

    (define/public (get-player-xpos)
      player-xpos)

    (define/public (get-player-ypos)
      player-ypos)

    (define/public (get-player-coord)
      (list player-xpos player-ypos))

    (define/public (move-player-right)
      (set! player-xpos (+ player-xpos x-size)))

    (define/public (move-player-fall)
      (set! player-ypos (+ player-ypos y-size)))

    (define/public (move-player-jump)
      (set! player-ypos (- player-ypos y-size)))

    (define/public (move-player-left)
      (set! player-xpos (- player-xpos x-size)))
    
    (define/public (get-char-coord char)
      (cond
        ((eq? char 'enemy)
         (list enemy-xpos enemy-ypos))
        ((eq? char 'andreas)
         (list player-xpos player-ypos))))

    (define/public (set-char-coord char xpos ypos)
      (cond
        ((eq? char 'enemy)
         (set! enemy-xpos xpos)
         (set! enemy-ypos ypos))
        ((eq? char 'andreas)
         (set! player-xpos xpos)
         (set! player-ypos ypos))))

    (define/public (set-coord xpos ypos list-board)
      (cond
        ((null? list-board)
         (displayln "Did it work"))
        ((= xpos 2400)
         (set-coord 0 (+ ypos y-size) list-board))
        ((equal? (car list-board) '#(30))
         (set-char-coord 'enemy xpos ypos)
         (set-coord (+ xpos x-size) ypos (cdr list-board)))         
        (else
         (set-coord (+ xpos x-size) ypos (cdr list-board)))))
    
    
    (super-new)))
