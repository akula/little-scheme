(define (card-list)
  (reduce append
	  (map (lambda (suit) (map (lambda (rank) (word suit rank))
				   '(a 2 3 4 5 6 7 8 9 10 j q k)))
          
               '(h s d c))))

(define (make-deck)
  (shuffle! (list->vector (card-list)) 51))

(define (shuffle! deck index)
  (if (< index 0)
      deck
      (begin (vector-swap! deck index (random (+ index 1)))
	     (shuffle! deck (- index 1)))))

(define (vector-swap! vector index1 index2)
  (let ((temp (vector-ref vector index1)))
    (vector-set! vector index1 (vector-ref vector index2))
    (vector-set! vector index2 temp)))
