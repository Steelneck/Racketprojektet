#lang racket
(provide item%)
(require "help_procs.rkt")


#|Template for items in the game.|#

(define item%
  (class object%
    (init-field name
                description
                take                                 ;Some items can be taken, which ones are decided in world_init file.
                error-description
                [item-location null])
    
    (define/public (get-name)                        ;Name of the item.
      name)
    
    (define/public (get-description)                 ;Description.
      description)
    
    (define/public (get-error-description)           ;When you try to take an item that can't be taken.
      error-description)
    
    (define/public (move-to new-place)               ;Stores the different items in the world in world_init file.
      (cond
        ((equal? item-location new-place)
         (display (error "Item already in place")))  
        (else
         (send new-place add-item this)              
         (set! item-location new-place))))
    
    (define/public (get-take)                        ;For taking items.
      take)
    
    (super-new)))
