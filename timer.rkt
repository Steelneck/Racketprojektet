#lang racket/gui

(provide (all-defined-out))


#|Timer classes|#


(define design-timer%
  (class timer%
    (init-field
                [timer-up 0]
                [timer-down 0]
                [timer-gate-val 0])

    (define/public (get-timer-up)
      timer-up)

    (define/public (get-timer-down)
      timer-down)

    (define/public (get-timer-gate)
      timer-gate-val)

    (define/public (open-gate-1)
      (set! timer-gate-val 1))

    (define/public (open-gate-2)
      (set! timer-gate-val 2))

    (define/public (close-gate)
      (set! timer-gate-val 0))

    (define/public (inc-time)
      (set! timer-up (+ timer-up 32)))

    (define/public (decr-time)
      (set! timer-down (+ timer-down 32)))

    (define/public (reset-time)
      (set! timer-up 0)
      (set! timer-down 0))

    (super-new)))
