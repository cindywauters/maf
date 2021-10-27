;; Simple renaming (should be true)
(define first-test
  (<change> (lambda (n) n)
            (lambda (r) r)))
(first-test 2)

;; Renaming + something else (should be false)
(define second-test
  (<change> (lambda (n) n)
            (lambda (r) (+ 1 r))))
(second-test 8)

;; Renaming but with mistake (should be false)
(define third-test
  (<change> (lambda (n) n)
            (lambda (r) (+ 1 (+ 1 n)))))
(third-test 8)

;; More nested renaming (should be true)
(define fourth-test
 (<change> (lambda (n)
              (/ (+ 5 (- n 2)) 2))
          (lambda (r)
              (/ (+ 5 (- r 2)) 2))))
(fourth-test 4)

;; Same nested renaming but with different change (should be false)
(define fifth-test
 (<change> (lambda (n)
             (/ (+ 5 (- n 2)) 2))
          (lambda (r)
              (/ (+ 5 (- r 3)) 2))))
(fifth-test 5)

;; Lambda with multiple parameters, one renamed
(define sixth-test
  (<change> (lambda (a b) (+ a b))
            (lambda (a c) (+ a c))))

(sixth-test 1 2)

;; Lambda with two renamed parameters (should be true)
(define seventh-test
  (<change> (lambda (a b c) (display c) (+ 5 6) (+ 1 a (- b c)))
            (lambda (a d e) (display e) (+ 5 6) (+ 1 a (- d e)))))
(seventh-test 1 2 3)

;; Lambda with two renamed parameters + other changes (should be false)
(define eighth-test
  (<change> (lambda (a b c) (display c) (+ 5 6) (+ 1 a (- b c)))
            (lambda (a d e) (display d) (+ 5 6) (+ 1 a (- e d)))))
(eighth-test 1 2 3)

;; Changed + to -
(define nineth-test
  (<change> (lambda (a b) (+ a b))
            (lambda (a c) (- a c))))

(nineth-test 1 2)

(define tenth-test
  (<change> (lambda (l m) (lambda (n o) (+ l m n o)))
            (lambda (a c) (- a c))))

((tenth-test 1 2) 2 3)