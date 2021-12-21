;; renamed lambdas/lets: 4
 
(define extended-gcd (lambda (a b)
      (if (= (modulo a b) 0)
         (cons 0 1)
         (<change>
            (let* ((x:y (extended-gcd b (modulo a b)))
                   (x (car x:y))
                   (y (cdr x:y)))
               (cons y (- x (* y (quotient a b)))))
            (let* ((_x:y0 (extended-gcd b (modulo a b)))
                   (_x0 (car _x:y0))
                   (_y0 (cdr _x:y0)))
               (cons _y0 (- _x0 (* _y0 (quotient a b)))))))))
 
(define modulo-inverse (lambda (a n)
      (modulo (car (extended-gcd a n)) n)))
 
(define totient (lambda (p q)
      (* (- p 1) (- q 1))))
 
(define square (lambda (x)
      (* x x)))
 
(define modulo-power (<change>
      (lambda (base exp n)
         (if (= exp 0)
            1
            (if (odd? exp)
               (modulo (* base (modulo-power base (- exp 1) n)) n)
               (modulo (square (modulo-power base (/ exp 2) n)) n))))
      (lambda (_base0 _exp0 _n0)
         (if (= _exp0 0)
            1
            (if (odd? _exp0)
               (modulo (* _base0 (modulo-power _base0 (- _exp0 1) _n0)) _n0)
               (modulo (square (modulo-power _base0 (/ _exp0 2) _n0)) _n0))))))
 
(define is-legal-public-exponent? (<change>
      (lambda (e p q)
         (if (< 1 e)
            (if (< e (totient p q))
               (= 1 (gcd e (totient p q)))
               #f)
            #f))
      (lambda (_e0 _p0 _q0)
         (if (< 1 _e0)
            (if (< _e0 (totient _p0 _q0))
               (= 1 (gcd _e0 (totient _p0 _q0)))
               #f)
            #f))))
 
(define private-exponent (lambda (e p q)
      (if (is-legal-public-exponent? e p q)
         (modulo-inverse e (totient p q))
         (error "Not a legal public exponent for that modulus."))))
 
(define encrypt (<change>
      (lambda (m e n)
         (if (> m n)
            (error "The modulus is too small to encrypt the message.")
            (modulo-power m e n)))
      (lambda (_m0 _e0 _n0)
         (if (> _m0 _n0)
            (error "The modulus is too small to encrypt the message.")
            (modulo-power _m0 _e0 _n0)))))
 
(define decrypt (lambda (c d n)
      (modulo-power c d n)))
 
(define p 41)
 
(define q 47)
 
(define n (* p q))
 
(define e 7)
 
(define d (private-exponent e p q))
 
(define plaintext 42)
 
(define ciphertext (encrypt plaintext e n))
 
(define decrypted-ciphertext (decrypt ciphertext d n))
 
(= plaintext decrypted-ciphertext)
 
