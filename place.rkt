#lang racket/gui
(provide place%)
(provide (all-defined-out))


#|The template for places.|#

(define place%                                                             
  (class object%                                                           
    (init-field name
                description
                board
                [exits-ht null]
                [k 0]
                [row 7]
                [col 4]
                [lvl-col 4]
                [plateau-xpos 0]
                [plateau-ypos 0]
                [x-size 120]
                [y-size 80]
                [value-list '()])

    (define/public (get-value-list)
      value-list)
    
    (define/public (get-name)                                           
      name)

    (define/public (get-board)
      board)
    
    (define/public (get-description)
      description)

    (define/public (get-k)
      k)

    (define/public (get-row)
      row)

    (define/public (get-col)
      col)

    (define/public (get-lvl-col)
      lvl-col)


    #|More attempts on graphic collision|#

    (define/public (set-coord xpos ypos list-board)
      (cond
        ((null? list-board)
         value-list)
        ((= xpos 2400)
         (set-coord 0 (+ ypos y-size) list-board))
        ((equal? (car list-board) '#(50))
         (set! value-list (cons (list xpos ypos) value-list))
         (set-coord (+ xpos x-size) ypos (cdr list-board)))
        ((equal? (car list-board) '#(20))
         (set! value-list (cons (list xpos ypos) value-list))
         (set-coord (+ xpos x-size) ypos (cdr list-board)))
        (else
         (set-coord (+ xpos x-size) ypos (cdr list-board)))))
    
                                 
    (super-new)))
