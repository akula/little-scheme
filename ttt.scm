(define (find-triples position)
  (every (lambda (comb) (substitute-triple position)) '(123 456 789 147 258 369 159 357)))

(define (substitute-triple combination position)
  (accumulate word (every  (lambda (square)                             
                             (substitute-letter square position)) combination)))

(define (substitute-letter square position)
  (if (equal? '_ (item square position))
      square
      (item square position)))


(define (ttt-choose position me)
  (ttt-choose (find-triples position) me))


    
    
    
(define (my-pair? triple me)
  (and (= (appearances me triple) 2)
       (= (appearances (opponent me) triple) 0)))
    
 (define (opponent letter)
   (if (equal? letter 'x) 'o 'x))
    
    