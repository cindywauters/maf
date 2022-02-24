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

(define (lambdareplacement-coarse a)
 (begin
  (display (<update> a (+ a 1)))
  ((<update>
   (lambda (x) (+ x 1))
   (lambda (y) (+ y 1))) a)))

(define (lambdareplacement-fine a)
 (begin
  (display (<update> a (+ a 1)))
  ((lambda ((<update> x y)) (+ (<update> x y) 1)) a)))

(define nested-renaming
 (lambda (a)
  (lambda ((<update> b c))
   (+ a (<update> b c)))))

(define ((<update> oldRec newRec) (<update> a b))
 (if (> (<update> a b) 1)
     (display "end")
     (begin
      (display (<update> a b))
      ((<update> oldRec newRec) (- (<update> a b) 1)))))

(define nested-lambda
  (lambda (a)
    (lambda (b)
     (lambda ((<update> c d))
      (+ a b (<update> c d))))))

(define (nested-define (<update> a x))
  (define (<update> old-nested-define new-nested-define) (+ (<update> a x) 1))
  (display (<update> old-nested-define new-nested-define)))


((<update> oldRec newRec)
  (+ (f 1)
     (g1 1)
     (<insert> (insertiontest 6))
     (<delete> (deletiontest 6))
     (lambdareplacement 2)
     (g2 (<update> oldVar newVar))))

