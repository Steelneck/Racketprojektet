#lang racket

(provide (all-defined-out))



#|Some simple functions to assist with gameboard|#

(define enumerate                
  (lambda (from to steps)        
    (if (> from to)
        '()
        (cons from (enumerate (+ from steps) to steps)))))