; Changes:
; * removed: 1
; * added: 1
; * swaps: 2
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 1
(letrec ((extended-gcd (lambda (a b)
                         (if (= (modulo a b) 0)
                            (cons 0 1)
                            (let* ((x:y (extended-gcd b (modulo a b)))
                                   (x (car x:y))
                                   (y (cdr x:y)))
                               (cons y (- x (* y (quotient a b))))))))
         (modulo-inverse (lambda (a n)
                           (modulo (car (extended-gcd a n)) n)))
         (totient (lambda (p q)
                    (* (- p 1) (- q 1))))
         (square (lambda (x)
                   (<change>
                      (* x x)
                      ((lambda (x) x) (* x x)))))
         (modulo-power (lambda (base exp n)
                         (if (= exp 0)
                            1
                            (if (odd? exp)
                               (modulo (* base (modulo-power base (- exp 1) n)) n)
                               (modulo (square (modulo-power base (/ exp 2) n)) n)))))
         (is-legal-public-exponent? (lambda (e p q)
                                      (if (< 1 e)
                                         (if (< e (totient p q))
                                            (= 1 (gcd e (totient p q)))
                                            #f)
                                         #f)))
         (private-exponent (lambda (e p q)
                             (if (is-legal-public-exponent? e p q)
                                (modulo-inverse e (totient p q))
                                (error "Not a legal public exponent for that modulus."))))
         (encrypt (lambda (m e n)
                    (if (> m n)
                       (error "The modulus is too small to encrypt the message.")
                       (modulo-power m e n))))
         (decrypt (lambda (c d n)
                    (modulo-power c d n)))
         (p 41)
         (q 47)
         (n (* p q))
         (e 7)
         (d (private-exponent e p q))
         (plaintext 42)
         (ciphertext (encrypt plaintext e n))
         (decrypted-ciphertext (decrypt ciphertext d n)))
   (<change>
      (display "The plaintext is:            ")
      (display plaintext))
   (<change>
      (display plaintext)
      (display "The plaintext is:            "))
   (newline)
   (display "The ciphertext is:           ")
   (<change>
      (display ciphertext)
      (newline))
   (<change>
      (newline)
      (display ciphertext))
   (<change>
      ()
      (display "Success"))
   (display "The decrypted ciphertext is: ")
   (display decrypted-ciphertext)
   (<change>
      (newline)
      ())
   (if (not (= plaintext decrypted-ciphertext))
      (error "RSA fail!")
      (display "Success")))