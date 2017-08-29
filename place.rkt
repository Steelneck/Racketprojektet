#lang racket/gui
(require "character.rkt")
(provide place%)
(provide (all-defined-out))


#|The template for places.|#

(define place%                                                             
  (class object%                                                           
    (init-field name
                description
                board
                [exits-ht null])
    
    (define/public (get-name)                                           
      name)

    (define/public (get-board)
      board)
    
    (define/public (get-description)
      description)
    
                                 
    (super-new)))
