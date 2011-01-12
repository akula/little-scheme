(define nil #f)
(define false #f)
(define true #t)
(define atom?
  (lambda (x)
    (and (not (pair? x))(not (null? x)))))
(define lat?
  (lambda (l)
    (cond ( (null? l) true)
          ( else (and (atom? (car l))(lat? (cdr l)))))))

(define member?
  (lambda ( a lat)
    (cond ( (null? lat) nil)
          ( else (or (eq? a (car lat))(member? a (cdr lat)))))))

(define rember
  (lambda ( a lat)
    (cond ( (null? lat) '())
          ( (eq? a (car lat))(cdr lat))
          ( else (cons (car lat)(rember a (cdr lat)))))))

(define firsts
  (lambda (lat)
    (cond ( (null? lat) '())
          (else (cons (car (car lat))(firsts (cdr lat)))))))

(define insertR
  (lambda (new old lat)
    (cond ( (null? lat) '())
          ( (eq? (car lat) old) (cons (car lat) (cons new (cdr lat))))
          (else (cons (car lat)(insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond ( (null? lat) '())
          ( (eq? (car lat) old)(cons new lat))
          (else (cons (car lat)(insertL new old (cdr lat)))))))

(define subst 
  (lambda (new old lat)
    (cond ( (null? lat) '())
          ( (eq? (car lat) old)(cons new (cdr lat)))
          (else (cons (car lat)(subst new old (cdr lat)))))))

(define subst2 
  (lambda (new o1 o2 lat)
    (cond ( (null? lat) '())
          ( (or (eq? (car lat) o1) (eq? (car lat) o2))(cons new (cdr lat)))
          (else (cons (car lat)(subst2 new o1 o2 (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond ( (null? lat) '())
          ( (eq? (car lat)  a)(multirember a (cdr lat)))
          (else (cons (car lat)(multirember a (cdr lat)))))))

(define multiinsertR 
  (lambda (new old lat)
    (cond ( (null? lat) '())
          ( (eq? old (car lat))(cons (car lat) (cons new ( multiinsertR new old (cdr lat)))))
          ( else (cons (car lat) (multiinsertR  new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond ( (null? lat) '())
          ( (eq? (car lat) old)(cons new (cons (car lat) (multiinsertL new old (cdr lat)))))
          (else (cons (car lat)(multiinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond ( (null? lat) '())
          ( (eq? (car lat) old)(cons new (multisubst new old (cdr lat))))
          (else (cons (car lat)(multisubst new old (cdr lat)))))))

(define add1 (lambda (x) (+ x 1)))
(define sub1 (lambda (x) (- x 1)))

(define O+
  (lambda (m n)
    (cond  ((zero? m) n)
           (else (add1 (O+ n (sub1 m)))))))

(define O-
  (lambda (m n)
    (cond ((zero? m) n)
          (else (sub1 (O- n (sub1 m)))))))

(define addtup
  (lambda (tup)
    (cond ( (null? tup) 0)
          (else (+ (car tup) (addtup (cdr tup)))))))

(define O*
  (lambda (m n)
    (cond ( (zero? m) 0)
          (else (O+ n (O* n (sub1 m)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond 
      ( (null? tup1) tup2)
      ( (null? tup2) tup1)
      (else (cons (O+ (car tup1)(car tup2)) (tup+ (cdr tup1 )(cdr tup2)))))))

(define O>
  (lambda (n m)
    (cond (( zero? n) #f)
          (( zero? m) #t)
          (else (O> (sub1 n)(sub1 m))))))

(define O<
  (lambda (n m)
    (cond (( zero? m) #f)
          (( zero? n) #t)
          (else (O< (sub1 n)(sub1 m))))))

(define O=
  (lambda ( n m )
    (cond ((zero? m)(zero? n))
          ((zero? n) #f)
          (else (O= (sub1 n)(sub1 m))))))

(define O^ 
  (lambda (a b)
    (cond ( (zero? b) 1)
          (else (O* a (O^ a (sub1 b)))))))

(define len
  (lambda (lat)
    (cond ( (null? lat) 0)
          (else (+ 1 (len (cdr lat)))))))

(define pick
  (lambda ( n lat)
    (cond ( (= n 1) (car lat))
          (else (pick (- n 1) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond ( (= n 1)(cdr lat))
          (else (cons (car lat) (rempick (- n 1) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond ( (null? lat) '())
          ( (number? (car lat))(no-nums (cdr lat)))
          (else (cons (car lat)(no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond ( (null? lat) '())
          ( (number? (car lat))(cons (car lat)(all-nums (cdr lat))))
          (else (all-nums (cdr lat))))))

(define eqan?
  (lambda (a1 a2)
    (cond ( (and (number? a1)(number? a2))(= a1 a2))
          ( (or (number? a1)(number? a2)) #f)
          (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond ( (null? lat) 0)
          ( (eqan? a (car lat))(+ 1 (occur a (cdr lat))))
          (else (occur a (cdr lat))))))

(define rember*
  (lambda (a lat)
    (cond ( (null? lat) '())
          ( (atom? (car lat)) 
            (cond  ( (eq? (car lat) a ) (rember* a (cdr lat)))
                   (else (cons (car lat)(rember* a (cdr lat))))))
          (else (cons (rember* a (car lat))(rember* a (cdr lat)))))))

(define insertR* 
  (lambda (new old lat)
    (cond ( (null? lat) '())
          ( (atom? (car lat))
            (cond ( (eq? (car lat) old)(cons (car lat) (cons new (insertR* new old (cdr lat)))))
                  ( else (cons (car lat )(insertR* new old (cdr lat))))))
          (else (cons (insertR* new old (car lat))(insertR* new old (cdr lat)))))))

(define occur*
  (lambda (a lat)
    (cond ( (null? lat) 0)
          ( (atom? (car lat)) 
                  (cond ( (eq?  (car lat) a)(+ 1 (occur* a (cdr lat))))
                        (else (occur* a (cdr lat)))))
                  (else (+ (occur* a (car lat)) (occur* a (cdr lat)))))))

(define subst*
  (lambda (new old lat)
    (cond ( (null? lat) '())
          ( (atom? (car lat))
            (cond (( eq? (car lat) old)(cons new (subst* new old (cdr lat))))
                  (else (cons (car lat)(subst* new old (cdr lat))))))
          (else (cons (subst* new old (car lat)) (subst* new old (cdr lat)))))))

(define insertL*
  (lambda (new old lat)
    (cond ( (null? lat) '())
          ( (atom? (car lat))
            ( cond ( (eq? (car lat) old)(cons new (cons (car lat)(insertL* new old (cdr lat)))))
                   (else (cons (car lat)(insertL* new old (cdr lat))))))
          (else (cons (insertL* new old (car lat))(insertL* new old (cdr lat)))))))

(define member*
  (lambda (a lat)
    (cond ( (null? lat) '())
          ( (atom? (car lat))
            (cond ((eq? (car lat) a)(member* a (cdr lat)))
                  (else (cons (car lat)(member* a (cdr lat))))))
          (else (cons (member* a (car lat))(member* a (cdr lat)))))))


          
(define leftmost
  (lambda (lat)
    (cond ( (atom? (car lat))(car lat))
          (else (leftmost (car lat))))))

(define eqlist?
  (lambda (lat1 lat2)
    (cond ( (and (null? lat1)(null? lat2)) true)
          ( (or (null? lat1)(null? lat2)) false)
          ( (and (atom? (car lat1))(atom? (car lat2)))(and (eqan? (car lat1)(car lat2))(eqlist? (cdr lat1)(cdr lat2))))
          ( (or (atom? (car lat1))(atom? (car lat2))) false)
          ( else (and (eqlist? (car lat1)(car lat2))(eqlist? (cdr lat1)(cdr lat2)))))))

(define equal?* 
  (lambda (s1 s2)
    (cond 
      (( and (atom? s1)(atom? s2))(eqan? s1 s2))
      ((atom? s1) #f)
      ((atom? s2) #f)
      (else (eqlist? s1 s2)))))
    
(define numbered?
  (lambda (aexp)
    (cond 
      ( (atom? aexp)(number? aexp))
      (else
       (and (numbered? (car aexp))(numbered? (car (cdr (cdr aexp)))))))))
                    
(define 1st-sub-aexp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-aexp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define value
  (lambda (aexp)
    (cond ((atom? aexp) aexp)
          ((eq? (car aexp) '+)(O+ (value(1st-sub-aexp aexp))(value(2nd-sub-aexp aexp))))
          ((eq? (car aexp) '*)(O* (value(1st-sub-aexp aexp))(value(2nd-sub-aexp aexp))))
          ((eq? (car aexp) '^)(O^ (value(1st-sub-aexp aexp))(value(2nd-sub-aexp aexp)))))))

(define sero?
  (lambda (n)
    (null? n)))
(define edd1
  (lambda (n)
    (cons '() n)))
(define zub1
  (lambda (n)
    (cdr n)))

(define set? 
  (lambda (s)
    (cond ( (null? s) true)
          ( (member? (car s) (cdr s))#f)
          (else (set? (cdr s))))))

(define markset
  (lambda (s)
    (cond ( (null? s) '())
          ( (member? (car s) (cdr s))(markset (cdr s)))
          (else (cons (car s)(markset (cdr s)))))))

(define subset
  (lambda (set1 set2)
    (cond ( (null? set1) #t)
          ( (member? (car set1) set2)(subset (cdr set1) set2))
          (else #f))))
(define eqset?
  (lambda (set1 set2)
    (cond ( (and (null? set1)(null? set2)) #t)
          ( (and (subset set1 set2)(subset set2 set1))))))

(define interset?
  (lambda (set1 set2)
    (cond ( (or ( null? set1)(null? set2)) #f)
          ( (or (member? (car set1) set2)(interset? (cdr set1) set2))))))

(define interset
  (lambda (set1 set2)
    (cond ( ( null? set1) '())
          ( (member? (car set1) set2)(cons (car set1)(interset (cdr set1) set2)))
          (else (interset (cdr set1 ) set2)))))

(define union
  (lambda (set1 set2)
    (cond ( (null? set1) set2)
          ( (member? (car set1) set2)(union (cdr set1) set2))
          ( else (cons (car set1)(union (cdr set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond ( (null? (cdr l-set)) (car l-set))
          ( else (interset (car l-set) (intersectall (cdr l-set)))))))

(define a-pair?
  (lambda (x)
    (cond ( (atom? x) #f)
          ( (null? x) #f)
          ( (null? (cdr x)) #f)
          ( (null? (cdr (cdr x))) #t)
          (else #f))))

(define first 
  (lambda (s)
    (car s)))
(define second 
  (lambda (s)
    (car (cdr s))))
(define third
  (lambda (s)
    (car (cdr (cdr s)))))

(define fun
  (lambda (rel)
    (set? (firsts rel))))


(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define revrel
  (lambda (rel)
    (cond ( (null? rel) '())
          (else (cons (build (second (car rel))(first (car rel)))(revrel (cdr rel)))))))
(define seconds
  (lambda (lat)
    (cond ( (null? lat) '())
          (else (cons  (second (car lat)) (seconds (cdr lat)))))))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(define rember-f 
  (lambda (f a lat)
    (cond ( (null? lat) '())
          ( (f (car lat) a)(rember-f f a (cdr lat)))
          (else (cons (car lat)(rember-f f a (cdr lat)))))))

(define eq-c?
  (lambda (x)
    (lambda (a)
      (eq? x a))))