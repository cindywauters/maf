(define first-test
  (<change> (lambda (a b) (+ a b))
            (lambda (a d) (+ a d))))

(first-test 5 10)