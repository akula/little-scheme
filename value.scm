

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help
     name
     (first entry)
     (second entry)
     entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    ( cond
      ( (null? values) (entry-f name))
      ( (equal? (car names) name) (car values))
      (else
       (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))


(define lookup-in-table
  (lambda (name table table-f)
    (cond
     ( (null? table)(table-f name))
     (else (lookup-in-entry name (car table)
                            (lambda (name)
                              (lookup-in-table
                               name
                               (cdr table)
                               table-f)))))))


(define expression-to-action
  (lambda (e)
    (cond
     ( (atom? e)(atom-to-action e))
     (else
      (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond ( (number? e)(*self-evaluating))
          (else
           *identifier))))

(define list-to-action
  (lambda (e)
    (cond
     ( (atom? (car e))
       (cond
        ( (eq? (car e)(quote quote)) *quote)
        ( (eq? (car e)(quote lambda)) *lambda)
        ( (eq? (car e)(quote cond)) *cond)
        ( else *applation)))
     (else
      *applation))))

(define value
  (lambda (e)
    (meaning e '())))

(define meaning
  (lambda ( e table )
    ((expression-to-action e) e table)))

(define *self-evaluating
  (lambda (e table)
    e))

(define *quote
  (lambda (e table)
    (text-of-quotation e)))


(define text-of-quotation second)
(define second
  (lambda (l)
    (car (cdr l))))

(define first
  (lambda (l)
    (car l)))

(define *identifier
  (lambda ( e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    ( cond
      ((eq? name 't) t)
      ((eq? name 'nil) nil)
      (else
       (build  (quote primitive) name)))))

(define *lambda
  (lambda (e table)
    (build (quote non-primitive)(cons table (cdr e)))))


      
