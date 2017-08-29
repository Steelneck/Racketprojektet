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
                [translate-xpos 0]
                [translate-ypos 0]
                [x-size 120]
                [y-size 80]
                [level-objects '()]
                [canvas-objects '()])


    #|Pixel values with functions|#

    (define/public (draw-sky dc xpos ypos)
      (send dc translate translate-xpos translate-ypos)
      (send dc set-brush "DodgerBlue" 'solid)
      (send dc draw-rectangle xpos ypos x-size y-size)
      (send dc translate (- translate-xpos) (- translate-ypos)))

    (define/public (draw-ground dc xpos ypos)
      (send dc translate translate-xpos translate-ypos)
      (send dc set-brush "Firebrick" 'solid)
      (send dc draw-rectangle xpos ypos x-size y-size)
      (send dc translate (- translate-xpos) (- translate-ypos)))

    (define/public (draw-power-up dc xpos ypos)
      (send dc translate translate-xpos translate-ypos)
      (send dc set-brush "Lime" 'solid)
      (send dc draw-rectangle xpos ypos x-size y-size)
      (send dc translate (- translate-xpos) (- translate-ypos)))

    (define/public (draw-enemy1 dc xpos ypos)
      (send dc translate translate-xpos translate-ypos)
      (send dc set-brush "DarkMagenta" 'solid)
      (send dc draw-rectangle xpos ypos x-size y-size)
      (send dc translate (- translate-xpos)  (- translate-ypos)))

    (define/public (draw-player dc xpos ypos)
      (send dc set-brush "red" 'solid)
      (send dc draw-ellipse xpos ypos x-size y-size))


    #|List-drawings will gather all the information from the matrix and "create" canvas objects with
      x and y coordinates for every element. The result is a list that paint-callback will paint from.
      When xpos is 1000, reset it and increase ypos. Elements like 1 and 100 will be painted sky while
      50 will be painted as ground etc.|#

    (define/public (list-merging list-board)                                          
      (if (null? list-board)
          '()
          (append (vector->list (car list-board))
                  (list-merging (cdr list-board)))))

    (define/public (list-elements board)
      (list-merging (vector->list (send board get-board))))

    
    (define/public (list-drawings dc xpos ypos list-board)
      (cond
        ((null? list-board)
         '())
        ((= xpos 1440)
         (list-drawings dc 0 (+ ypos y-size) list-board))
        ((or (equal? (car list-board) '#(1))
             (equal? (car list-board) '#(10))
             (equal? (car list-board) '#(666)))
         (cons (draw-sky dc xpos ypos) canvas-objects)
         (cons (list-drawings dc (+ xpos x-size) ypos (cdr list-board)) canvas-objects))
        ((equal? (car list-board) '#(20))
         (cons (draw-ground dc xpos ypos) canvas-objects)
         (cons (list-drawings dc (+ xpos x-size) ypos (cdr list-board)) canvas-objects))
        ((equal? (car list-board) '#(40))
         (cons (draw-power-up dc xpos ypos) canvas-objects)
         (cons (list-drawings dc (+ xpos x-size) ypos (cdr list-board)) canvas-objects))
        ((equal? (car list-board) '#(30))
         (cons (draw-enemy1 dc xpos ypos) canvas-objects)
         (cons (list-drawings dc (+ xpos x-size) ypos (cdr list-board)) canvas-objects))))
    

    (define/public (move-xpos operator pixels)
      (set! translate-xpos (operator translate-xpos pixels)))

    (define/public (move-ypos operator pixels)
      (set! translate-ypos (operator translate-ypos pixels)))


    [define/override (on-char key-event)
      (keyboard-handler key-event)]
    
    (super-new)))



       



