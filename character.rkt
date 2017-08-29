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
    
    
    (super-new)))
