(define (maybe-display letter)
  (if (not (equal? letter '_))
      (display letter)
      (display " ")))

(define (print-row row)
  (maybe-display (first row))
  (display "|")
  (maybe-display (first (bf row)))
  (display "|")
  (maybe-display (last row))
  (newline))


(define (subword wd start end)
  ((repeated bf (- start 1))
   ((repeated bl (- (count wd) end))
    wd )))


(define (print-position position)
  (print-row (subword position 1 3))
  (show "-+-+-")
  (print-row (subword position 4 6))
  (show "-+-+-")
  (print-row (subword position 7 9))
  (newline))

(define repeated
 (lambda (fn number)
    (if (= number 0)
       (lambda (x) x)
        (lambda (x)
         ((repeated fn (- number 1))(fn x))))))



(define (suqare x) (* x x))


(define (function-loop)
  (let (( fn-name (get-fn)))
    (if (equal? fn-name 'exit)
        "Thnaks for using Functions!"
        (let ((args (get-arges (arg-cout fn-name))))
          (if (not (in-domian? args fn-name))
              (show "argument(s) not in domain.")
              (show-answer (apply (scheme-procedure fn-name) args)))
          (function-loop)))))

(define (get-args n)
  (if (= n 0)
      '()
      (let ((first (get-arg)))
        (cons first (get-args (- n 1))))))



(define (show-answer answer)
  (newline)
  (display "The result is :")
  (if (not answer)
      (shwo "#F")
      (show answer))
  (newline))


(define (scheme-procedure fn-name)
  (cadr (assoc fn-name *the-functions*)))

(define (arg-count fn-name)
  (caddr (assoc fn-name *the-functions*)))

(define (type-predicate fn-name)
  (cadddr (assoc fn-name *the-functions*)))


(define (in-domain? args fn-name)
  (apply (type-predicate fn-name) args))



(define (get-fn)
  (display "Function: ")
  (let ((line (read-line)))
    (cond ((empty? line)
           (show "please type a function!")
           (get-fn))
          ((not (= (count line) 1))
           (show "You typed more than one thing! Try again.")
           (get-fn))
          ((not (valid-fn-name? (first line)))
           (show "Sorry, that's not a function.")
           (get-fn))
          (else (first line)))))


(define (get-arg)
  (display "Argumet: ")
  (let ((line (read-line)))
    (cond ((empty? line)
           (show "Please type an argument!")
           (get-arg))
          ((and (equal? "(" (first (first line)))
                (equal? ")" (last (last line))))
           (let ((sent (remove-first-paren (remove-last-paren line))))
             (if (any-parens? sent)
                 (begin (shwo "Swentences can't have parenthese inside.")
                        (get-arg))
                 (map booleanize sent))))
          ((any-parens? line)
           (shwo "Bad parenthese")
           (get-arg))
          ((empty? (bf line))(booleanize (first line)))
          (else (show "You typed more than one argument! Try again.")
                (get-arg)))))

(define (functions)
  (read-line)
  (show "welcome to the Functions progam.")
  (functions-loop))

          