
(define set?
  (lambda (  set )
    ( cond
      ( (null? set) #t)
      ( (member? (car set) (cdr set)) #f)
      (else (set? (cdr set))))))

(define member?
  (lambda ( a l )
    (cond
     ( (null? l ) #f)
     ( (equal? a (car l)) #t)
     ( else (member? a (cdr l))))))
(define makeset
  (lambda ( lat )
    (cond
     ( (null? lat) '())
     ( (member? (car lat) (cdr lat)) (makeset (cdr lat)))
     ( else  (cons (car lat) (makeset (cdr lat)))))))

(define multrember
  (lambda ( a lat)
    (cond
     ( ( null? lat) '())
     ( (equal? a (car lat))(multrember a (cdr lat)))
     (else
      (cons (car lat) (multrember a (cdr lat)))))))
;; dirty solution .
(define makeset2
  (lambda ( lat)
    (cond
     ( ( null? lat) '())
     ( (member? (car lat) (cdr lat)) (makeset2 (cons (car lat )(multrember (car lat) (cdr lat)))))
     (else
      (cons (car lat ) (makeset2 (cdr lat)))))))

(define makeset3
  (lambda (lat)
    (cond
     ( (null? lat) '())
     ( else
       (cons (car lat) (makeset3 (multrember (car lat) (cdr lat))))))))


(define subset
  (lambda (set1 set2)
    (cond
     ( (null? set1) #t)
     ( else
       (and (member? (car set1) set2)(subset (cdr set1) set2))))))

(define eqset
  (lambda (set1 set2)
    (and (subset set1 set2)(subset set2 set1))))

(define interset?
  (lambda (set1 set2)
    (cond
     ( (null? set1) #f)
     
     (else (or (member? (car set1) set2)(interset? (cdr set1 ) set2))))))


(define interset
  (lambda (set1 set2 )
    (cond
     ( (or(null? set1) (null? set2) ) '())
     ( (member? (car set1) set2) (cons (car set1) (interset (cdr set1) set2)))
     (else (interset (cdr set1 ) set2)))))


;; interset version 2 , use not member? replace member?

(define interset2
  (lambda (set1 set2)
    (cond
     ( (null? set1) '())
     ( (not (member? (car set1) set2)) (interset2 (cdr set1) set2))
     (else
      (cons (car set1) (interset2 (cdr set1)  set2))))))

;;dirty solution
(define union-
  (lambda (set1 set2)
    (cond
     ( (null? set1) set2)
     ( (member? (car set1) set2 ) (cons (car set1)(union- (cdr set1) (multrember (car set1) set2))))
     (else
      (cons (car set1) (union- (cdr set1) set2))))))

(define union+
  (lambda (set1 set2)
    (cond
     ( (null? set1 ) set2)
     ( (member? (car set1) set2) (union+ (cdr set1) set2))
     (else
      (cons (car set1) (union+ (cdr set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond
     ;;( (null? l-set) '())
     ( (null? (cdr l-set)) (car l-set))
     (else (interset (car l-set) (intersectall (cdr l-set)))))))