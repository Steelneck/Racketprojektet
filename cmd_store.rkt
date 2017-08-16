#lang racket

(provide (all-defined-out))


#|Functions to handle commands.|#

(define *commands* (make-hash))

(define (add-cmd! cmd proc)          ;Saves the procedure as cmd.
  (hash-set! *commands* cmd proc))

(define (remove-cmd! name)           ;Removes.
  (hash-remove! *commands* name))

(define (valid-cmd? name)            ;Checks for existing command.
  (hash-has-key? *commands* name))

(define (get-proc name)              ;Gets the procedure.
  (hash-ref *commands* name))

(define (get-valid-cmd)              ;List of commands.
  (hash-keys *commands*))