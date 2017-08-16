#lang racket/gui
(require "place.rkt")
(require "item.rkt")
(require "character.rkt")
(require "cmd_store.rkt")

(provide (all-defined-out))

(define enumerate                
  (lambda (from to steps)        
    (if (> from to)
        '()
        (cons from (enumerate (+ from steps) to steps)))))

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






  


    