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