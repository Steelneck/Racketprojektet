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
                [character-location null])
    (field
     [inventory-ht (make-hash)])
    
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

    (define/public (inc-score-enemy)
      (set! character-value (+ character-value 1)))

    (define/public (inc-score-power)
      (set! character-value (+ character-value 5)))

    (define/public (get-player canvas dc)
      (send dc set-brush "red" 'solid)
      (send dc draw-ellipse 400 600 40 40))









    
    (define/public (move-to new-place)                       ;Moves the character, checks if already there.
      (cond
        ((equal? character-location new-place)
         (display (error "Character already there.")))       ;Handles if the character is already there.
        (else
         (when (not (null? character-location))              ;Updates character-location.
           (send character-location delete-char name))
         (send new-place add-char this)                
         (set! character-location new-place))))
    
    (define/public (get-inventory)                           ;Inventory.
      (hash-keys inventory-ht))
    
    (define/public (get-inventory-ht)
      inventory-ht)
    
    (define/public (get-item item)                           ;Updates inventory when character takes items.
      (hash-set! inventory-ht (send item get-name) item))
    
    (define/public (receive-item item)                       ;Necessary for a "special-character" to receive something.
      (hash-set! inventory-ht (send item get-name) item))
    
    (define/public (give item npc this-ui)                   ;Give function that updates hashtables with handle-special.
      (if (hash-has-key? inventory-ht item)
          (begin
            (send npc handle-special item inventory-ht this-ui))
          "You don't have that item."))

    (define/public (handle-special item inventory-ht this-ui)
      (hash-set! inventory-ht (send item get-name) item))
    
    (define/public (remove-item item this-ui)                ;Necessary to remove items from rooms when taken or from inventory when given away.
      (hash-remove! inventory-ht item))
    
    (super-new)))


#|Special characters will have the ability to receive something.
There will be two templates even if they
essentially do the same thing to make sure they're separated from
unexpected errors. Trigger can be used for specified features
when something occurs during game such as new level when
boss die etc.|#

(define special-character1%
  (class character%
    (init-field
     trigger)
    (define/override (handle-special item inventory-ht this-ui)
      (cond          
        ((equal? item "Tupe")
         (trigger)
         (hash-remove! inventory-ht item)
         (send this-ui present "Ivanka put on your tupé! She will now let you go into the PressRoom. Look for the new exit."))
        (else
         (send this-ui present "What is this? I don't want that!"))))
    (super-new)))

(define special-character2%
  (class character%
    (init-field)
    (define/override (handle-special item inventory-ht this-ui)
      (cond
        ((equal? item "The-nuclear-football")
         (send this-ui notify
               "Congratulations, you won! The world can finally rest knowing that Dolan Thrump will never be able to launch nuclear missiles!")
         (send this-ui close-ui))
        (else
         (send this-ui present "What is this? I don't want that!"))))
    (super-new)))
