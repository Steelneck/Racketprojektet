#lang racket
(provide item%)


#|Template for items in the game.|#

(define item%
  (class object%
    (init-field name
                description
                take                                 ;Some items can be taken, which ones are decided in world_init file.
                error-description
                [item-location null])
    
    (super-new)))
