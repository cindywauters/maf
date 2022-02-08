(define (f x) (+ 1 x))

(define (g1 y)
  (<insert> (display "g"))
  (+ 1 y))

(define (g2 y)
  (<change> #f (display "g"))
  (+ 1 y))

(define (g3 y)
  (<delete> (display "g"))
  (+ 1 y))

(define (g4 y)
  (<change> (display "g") #f)
  (+ 1 y))

(define (<rename> oldF newF) (lamdba (x) (+ x 1)))

(+ (f 1) (g1 1) (g2 1) (g3 1) (g4 1) (<change> (oldF 1)  (newF 1)))