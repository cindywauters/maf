(define (f x) (+ 1 x))

(define (g2 y)
;  (<insert> (display "g"))
  (+ 1 y))

(define (g4 y)
 ; (<delete> (display "g"))
  (+ 1 y))


;(define (<rename> oldF newF)
;   (lambda (x) (+ x 1)))

;(define (<rename> oldVar newVar)
;   5)

(+ (f 1)
   (g2 1))
 ;  (g4 (<change> oldVar newVar))
  ; (<change> (oldF 1)  (newF 1)))

