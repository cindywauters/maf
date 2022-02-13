(define (f x) (+ 1 x))

;(define (g2 y)
;  (<change> #f (display "g"))
;  (+ 1 y))

(define (g2 y)
  (<insert> (display "g"))
  (+ 1 y))

;(define (g4 y)
;  (<change> (display "g") #f)
;  (+ 1 y))

(define (g4 y)
  (<delete> (display "g"))
  (+ 1 y))

;(define g4
; (lambda (y)
;   (<delete> (display "g"))
;   (+ 1 y)))

(define (<rename> oldF newF) (lambda (x) (+ x 1)))

(+ (f 1)(g2 1)(g4 1)(<change> (oldF 1)  (newF 1)))