(define *the-functions*                      ;; partial listing
  (list (list '* * 2 (lambda (x y) (and (number? x) (number? y))))
	(list '+ + 2 (lambda (x y) (and (number? x) (number? y))))
	(list 'and (lambda (x y) (and x y)) 2
	      (lambda (x y) (and (boolean? x) (boolean? y))))
	(list 'equal? equal? 2 (lambda (x y) #t))
	(list 'even? even? 1 integer?)
	(list 'word word 2 (lambda (x y) (and (word? x) (word? y))))))

(define (scheme-procedure fn-name)
  (cadr (assoc fn-name *the-functions*)))

(define (arg-count fn-name)
  (caddr (assoc fn-name *the-functions*)))

(define (type-predicate fn-name)
  (cadddr (assoc fn-name *the-functions*)))

