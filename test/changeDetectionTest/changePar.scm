(define second-test
  (<change> (lambda (n) (+ (- 3 2) n))
            (lambda (r) (+ (- 3 2) r))))

(define (something-else z)
  (+ z 1))

(define third-test
  (<change> (lambda (n) n)
            (lambda (r) (+ 1 r))))
(second-test 8)
(third-test 8)

;; change name parameter + something else
;(define second-test-change
;  (<change> (lambda (n) n)
;            (lambda (r) (+ 1 r))))
;(second-test-change 8)