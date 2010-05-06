(define (invert input output)
  (define (invert-input)
    (let ( (new-value (logical-not (get-signal input))))
      (after-delay invert-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input  invert-input )
  'ok)

(define (logical-not s)
  (cond ( (= s 1) 0)
        ( (= s 0) 1)
        (else "Invalid signal")))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let (( new-value
            (logical-and (get-signal a1) (get-signal a2) )))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  
  (add-action! a1 and-action-procedure)
  (add-cation! a2 and-action-procedure)
  'ok)

(define (logical-and a b)
  (and a b))


(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ( (new-value
            (logical-or (get-signal a1)(get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or a b)
  (or a b))


(define (make-wire)
  (let ( (signal-value 0) (action-procedure '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-vale new-vale))
          (begin (set! signal-value new-vale)
                 (call-each action-procedures))
          'done))

    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'seg-signal) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else "unknow operation")))
    dispatch ))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procdures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal wire)
  (wire 'set-signal))

(define (add-action! wire action-procedure)
  ( (wire 'add-action!) action-procedure))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-itme (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))


(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(define make-agenda (list ()))