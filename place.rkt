#lang racket/gui
(require "character.rkt")
(provide place%)


#|The template for places.|#

(define place%                                                             
  (class object%                                                           
    (init-field name
                description
                board
                [exits-ht null])
    (field
     [chars-in-place (make-hash)]
     [items-in-place (make-hash)])
    
    (define/public (get-name)                                           
      name)

    (define/public (get-board)
      board)
    
    (define/public (get-description)
      description)
    

         





    (define/public (char-there? char-name)
          (hash-has-key? chars-in-place char-name))
    
    (define/public (get-char char-name)                                    ;A function to return characters, hash-ref will return character if true.
      (if (hash-has-key? chars-in-place char-name)
          (hash-ref chars-in-place char-name)
          "That character isn't here"))
    
    (define/public (delete-char char-name) 
      (hash-remove! chars-in-place char-name))
    
    (define/public (characters)                                            ;Returns a list with the characters including Dolan so it's possible to look and interact at yourself.
      (hash-keys chars-in-place))
    
    (define/public (add-item item)                                         ;Adds items and handles if they are already in place.
      (if (hash-has-key? items-in-place item)
          (display (error "Item already there")) 
          (hash-set! items-in-place (send item get-name) item)))
    
    (define/public (get-item item-name)                                    ;Returns a certain item for functions like "take".
      (hash-ref items-in-place item-name))
    
    (define/public (remove-item item-name)                                 ;Removes items from rooms.
      (hash-remove! items-in-place item-name))
    
    (define/public (items-ht)                                                 
      items-in-place)

    (define/public (items)
      (hash-keys items-in-place))
    
    (define/public (get-neighbour! exit-name)                              ;Get-neightbour! checks well, neighbours to the rooms and returns them.
      (hash-ref exits-ht exit-name))
    
    (define/public (add-neighbour! exit-name place)                        ;Add-neighbour will be used for connecting the rooms. 
      (hash-set! exits-ht exit-name place))
                                                                   
        (define/public (exits)                                             ;Exits in a list.
      (hash-keys exits-ht))
                                                                     
    (super-new)))
