#lang racket/gui

(provide canvas%)
(provide (all-defined-out))

#|Canvas classes|#

(define design-canvas%
  (class canvas%
    (init-field name
                description
                keyboard-handler
                [xpos-tile 0]
                [ypos-tile 0]
                [x-size 120]
                [y-size 80]
                [level-objects '()])


    #|Pixel values with functions|#

    (define/public (get-tile-x)
      xpos-tile)

    (define/public (get-tile-y)
      ypos-tile)

    (define/public (next-tile-x)
      (set! xpos-tile (+ xpos-tile x-size)))

    (define/public (next-tile-y)
      (set! ypos-tile (+ ypos-tile y-size)))

    (define/public (reset-tile-x)
      (set! xpos-tile 0))

    (define/public (reset-tile-y)
      (set! ypos-tile 0))

    (define/public (draw-sky dc xpos-tile ypos-tile)
      (send dc set-brush "DodgerBlue" 'solid)
      (send dc draw-rectangle xpos-tile ypos-tile 120 80))

    (define/public (draw-ground dc)
      (send dc set-brush "Firebrick" 'solid)
      (send dc draw-rectangle 0 640 3600 160))

    (define/public (draw-player dc)
      (send dc translate xpos-tile ypos-tile)
      (send dc set-brush "red" 'solid)
      (send dc draw-ellipse 400 600 40 40)
      (send dc translate (- xpos-tile) (- ypos-tile)))

  ;  (define/public (list-drawings dc element xpos-tile n)
  ;    (cond
  ;      ((null? element)
  ;       level-objects)
  ;      ((= n 0)
  ;       level-objects)
  ;      ((= element 1)
  ;       (cons (draw-sky dc xpos-tile ypos-tile) level-objects)
  ;       (list-drawings dc (+ xpos-tile x-size) (- n 1)
  ;      (else
  ;       (cons (draw-sky dc xpos-tile ypos-tile) level-objects)
  ;       (cons (list-drawings dc (+ xpos-tile x-size) (- n 1)) level-objects))))

    (define/public (move-xpos operator pixels)
      (set! xpos-tile (operator xpos-tile pixels)))

    (define/public (move-ypos operator pixels)
      (set! ypos-tile (operator ypos-tile pixels)))
      



    #|Hash objects with functions|#

    (define/public (canvasobjects dc)
      (draw-sky dc 0 0)
      (draw-sky dc 120 0)
      (draw-sky dc 240 80))



    
    (define/public (add-canvas drawing proc)
      (hash-set! level-objects drawing proc))

    (define/public (get-objects)                                  
      (hash-keys level-objects))

    (define/public (get-object name)
      (hash-ref level-objects name))

    (define/public (move-object-to new-object)                      
      (cond
        ((hash-has-key? level-objects new-object)
         (display (error "Object already there.")))     
        (else
         (when (not (null? level-objects))              
           (add-canvas new-object)))))


    [define/override (on-char key-event)
      (keyboard-handler key-event)]
    
    (super-new)))



       



