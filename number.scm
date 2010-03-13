;;

(define vecadd (lambda (vec)
  ( cond
    (  ( null? vec) 0)
    (else( + (car vec ) (vecadd (cdr vec)))))))
;;version 1 vec+ , can not work with tow vector with difficrent length 
(define lvec+(lambda (v1 v2)
               (cond
                ( (null? v1) '())
                (else (cons (+ (car v1) (car v2)) (vec+ (cdr v1 ) (cdr v2)))))))
;; version 2 vec+, work prefectly . 
(define 2vec+
  (lambda (v1 v2)
    (cond
     ( (null? v1) v2)
     ( (null? v2) v1)
     (else (cons (+ (car v1) (car v2)) (vec- (cdr v1)(cdr v2)))))))



(define  1>
  (lambda (m n )
    (cond
     ( (= m 0) #f)
     ( (= n 0) #t)
     (else ( 1> (- m 1) (- n 1))))))


(define pick
  (lambda ( th lat )
    ( cond
      ( (null? lat) nil)
      ( (= th 1) (car lat))
      (else (pick (- th 1 ) (cdr lat))))))

(define rempick
  (lambda (th lat)
    (cond
     ( (null? lat) '())
     ( (= th 1) (cdr lat))
     (else (cons (car lat)(rempick (- th 1) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond 
    ( (null? lat) '())
    ( (number? (car lat)) (no-nums (cdr lat)))
    (else (cons (car lat)(no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
     ( (null? lat) '())
     ( (number? (car lat ) )(cons (car lat)(all-nums (cdr lat))))
     (else (all-nums (cdr lat))))))

(define duplicate
  (lambda (n obj)
    (cond
     ( (= n 0) '())
     (else (cons obj (duplicate (- n 1) obj))))))

(define multvec
  (lambda (lat)
    (cond
     ( (null? lat) 1)
     (else (* (car lat) (multvec (cdr lat)))))))

(define multrember
  (lambda ( p lat)
    (cond
     ( (null? lat) '())
     ( (eq? (car lat) p) (multrember p (cdr lat)))
     (else
      (cons (car lat) (multrember p (cdr lat)))))))


(define multinsertR
  (lambda (new old lat)
    ( cond
      ( (null? lat) '())
      ( (eq? (car lat) old)(cons old (cons new (multinsertR new old (cdr lat)))))
      ( else
        (cons (car lat)(multinsertR new old (cdr lat)))))))


(define leftmost
  (lambda (lat)
    (cond
     ( (atom? (car lat) ) (car lat))
     (else
      (leftmost (car lat))))))

(define atom?
  (lambda (a)
      (and (not (pair? a)) (not(null? a)))))


(define eqlist?
  (lambda (lista listb)
    (cond
     ( (and (null? lista) (null? listb)) #t)
     ( (or (null? lista ) (null? listb)) #f)
     ( (and (atom? (car lista)) (atom? (car listb))
        (eq? (car lista)(car listb))(eqlist? (cdr lista )(cdr listb))))
     ( (or (atom? (car lista) ) (atom? (car listb)) ) #f)
     
     ( (and (eqlist? (car lista) (car listb)) (eqlist? (cdr lista)(cdr listb))))
     
     (else #f))))

(define equal-
  (lambda (a b)
    (cond
     ( (and (atom? a)(atom? b)(eq? a b)))
     ( (or (atom? a) (atom? b) ) #f)
     (else
      (eqlist?  a b)))))