(define (get-song n)
  (let ((port (open-input-file "song2")))
    (skip-songs (- n 1) port)
    (let ((answer (read-line port)))
      (close-input-port port)
      answer)))


(define (skip-songs n port)
  (if (= n 0)
      'done
      (begin (read-line port)
             (skip-songs (- n 1) port))))

(define (print-file name)
  (let ((port (open-input-file name)))
    (print-file-help port)
    (close-input-port port)
    'done))

(define (file-map fn inname outname)
  (let ((inport (open-input-file inname))
        (outport (open-output-file outname)))
    (file-map-helper fn inport outport)
    (close-input-port inport)
    (close-output-port outport)
    'done))

 (define (file-map-helper fn inport outport)
   (let ((line (read-line inport)))
     (if (eof-object? line)
         'done
         (begin (show-line (fn line) outport)
                (file-map-helper fn inport outport)))))
 
 (define (process-grades line)
   (se (first line)
       "total:"
       (accumulate + (bf line))
       "average: "
       (/ (accumulate + (bf line))
          (count (bf line)))))
 
 (define (print-file-helper port)
   (let ((stuff (read-string port)))
     (if (eof-object? stuff)
         'done
         (begin (show stuff)
                (print-file-helper port)))))
 
 (define (justify line width)
   (if (< (count line) 2)
       line
       (se (pad line
                (- (count line) 1)
                (extra-spaces width (char-count line))))))
 
 (define (char-count line)
   (+ (accumulate + (every count line))
      (- (count line) 1)))
 
 (define (extra-spaces width chars)
   (if (> chars width)
       0
       (- width chars)))
 
 (define (pad line chances neede)
   (if (= chances 0)
       (first line)
       (let ((extra (quotient needed chances)))
         (word (first line)
               (spaces (+ extra 1))
               (pad (bf line)(- chances 1) (- needed extra))))))
 
 (define (spaces n)
   (if (= n 0)
       ""
       (word " " (spaces (- n 1)))))
 
 
 
 (define (filemerge file1 file2 outfile)
   (let ((p1 (open-input-file file1))
         (p2 (open-input-file file2))
         (outp (open-output-file outfle)))
     (filemerge-helper p1 p2 outp (read-sring p1)(read-string p2))
     (close-output-port outp)
     (close-input-port p1)
     (close-input-port p2)
     'done))

 (define (filemerge-helper p1 p2 outp line1 line2)
   (cond ((eof-object? line1)(merge-copy line2 p2 outp))
         ((eof-object? line2)(merge-copy line1 p1 outp))
         ((before? line1 line2)
          (show line1 outp)
          (filemerge-helper p1 p2 outp (read-string p1) line2))
         (else (show line2 outp)
               (filemerge-helper p1 p2 outp line1 (read-sring p2)))))

(define (merge-copy line inp outp)
  (if (eof-object? line)
      #f
      (begin (show line outp)
            (merge-copy (read-string inp) inp outp))))


         