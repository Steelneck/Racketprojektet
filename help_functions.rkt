#lang racket

(provide (all-defined-out))



#|Some simple functions to assist with gameboard|#

(define enumerate                
  (lambda (from to steps)        
    (if (> from to)
        '()
        (cons from (enumerate (+ from steps) to steps)))))

#|Function to return elements from a matrix into a list|#

;(define (vector-merging vector-board)
;  (if (null? vector-board)
;      '#()
;      (vector-append (car vector-board)
;                     (vector-merging (cdr vector-board)))))





