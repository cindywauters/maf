(define (f x) (+ 1 x))

(define (g1 y)
  (<insert> (display "g"))
  (+ 1 y)
  (<change> 1 2))

(<insert> (define (insertiontest x)
 (display x)
 (+ 6 x)))

(<delete> (define (deletiontest x)
 (- 6 x)
 display x))

(define (g2 y)
  (<delete> (display "g"))
  (+ 1 y))

(define (<update> oldVar newVar) 5)

(define ((<update> oldRec newRec) a)
 (if (> a 1)
     (display a)
     (begin
      (display a)
      ((<update> oldRec newRec) (- a 1)))))

((<update> oldRec newRec)  5)

(+ (f 1)
   (g1 1)
   (g2 (<update> oldVar newVar)))

