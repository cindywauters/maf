(define (f x) (+ 1 x))

(define (g1 y)
  (<insert> (display "g"))
  (+ 1 y)
  (<update> 1 2))

(<insert> (define (insertiontest x)
 (display x)
 (+ 6 x)))

(<delete> (define (deletiontest x)
 (display x)
 (- 6 x)))

(define (g2 y)
  (<delete> (display "g"))
  (+ 1 y))

(define (<update> oldVar newVar) 5)

(define lambdareplacement
 (<update>
  (lambda (x) (+ x 1))
  (lambda (y) (+ y 1))))


(define ((<update> oldRec newRec) a)
 (if (> a 1)
     (display "end") ;  (<update> "end oldRec" "end newRec"))
     (begin
      (display a)
      ((<update> oldRec newRec) (- a 1)))))

((<update> oldRec newRec)
  (+ (f 1)
     (g1 1)
     (<insert> (insertiontest 6))
     (<delete> (deletiontest 6))
     (lambdareplacement 2)
     (g2 (<update> oldVar newVar))))

