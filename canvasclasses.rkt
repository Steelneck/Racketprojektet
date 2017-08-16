#lang racket/gui

(provide canvas%)
(provide (all-defined-out))

#|Canvas classes|#

(define design-canvas%
  (class canvas%
    (init-field name
                description
                keyboard-handler
                [xpos 0]
                [ypos 0])

    (field
     [level-objects (make-hash)])


    #|Pixel values with functions|#
    
    (define/public (get-xpos)
      xpos)

    (define/public (get-ypos)
      ypos)

    (define/public (move-xpos operator pixels)
      (set! xpos (operator xpos pixels)))

    (define/public (move-ypos operator pixels)
      (set! ypos (operator ypos pixels)))
      
    
    (define/public (physic-update-xpos)
      (set! xpos (+ xpos 3)))



    #|Hash objects with functions|#
    
    (define/public (add-canvas object)
      (if (hash-has-key? level-objects object)
          (display ("Object already in place"))
          (hash-set! level-objects (send object get-name) object)))

    (define/public (get-objects)                                  
      (hash-keys level-objects))

    (define/public (get-object name)
      (hash-ref level-objects name))

    (define/public (move-object-to new-object)                       ;Moves the character, checks if already there.
      (cond
        ((hash-has-key? level-objects new-object)
         (display (error "Object already there.")))       ;Handles if the character is already there.
        (else
         (when (not (null? level-objects))              ;Updates character-location.
           (add-canvas new-object)))))

      
    [define/override (on-char key-event)
      (keyboard-handler key-event)]
    
    (super-new)))



       



