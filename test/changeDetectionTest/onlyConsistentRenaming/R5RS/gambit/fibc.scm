;; renamed lambdas/lets: 3
 
(define _1+ (lambda (n)
      (+ n 1)))
 
(define _1- (<change>
      (lambda (n)
         (- n 1))
      (lambda (_n0)
         (- _n0 1))))
 
(define addc (<change>
      (lambda (x y k)
         (if (zero? y) (k x) (addc (_1+ x) (_1- y) k)))
      (lambda (_x0 _y0 _k0)
         (if (zero? _y0)
            (_k0 _x0)
            (addc (_1+ _x0) (_1- _y0) _k0)))))
 
(define fibc (<change>
      (lambda (x c)
         (if (zero? x)
            (c 0)
            (if (zero? (_1- x))
               (c 1)
               (addc
                  (call-with-current-continuation (lambda (c) (fibc (_1- x) c)))
                  (call-with-current-continuation (lambda (c) (fibc (_1- (_1- x)) c)))
                  c))))
      (lambda (_x0 _c0)
         (if (zero? _x0)
            (_c0 0)
            (if (zero? (_1- _x0))
               (_c0 1)
               (addc
                  (call-with-current-continuation (lambda (_c1) (fibc (_1- _x0) _c1)))
                  (call-with-current-continuation (lambda (_c2) (fibc (_1- (_1- _x0)) _c2)))
                  _c0))))))
 
(equal? (fibc 18 (lambda (n) n)) 2584)
 
