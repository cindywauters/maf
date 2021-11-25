(define first-test
  (<change> (lambda (a b) (+ a b))
            (lambda (a d) (+ a d))))

(first-test 5 10)


;(define (second-test c)
;   (<change> (let ((a 5) (b 10)) (+ a b c))
;             (let ((a 5) (d 10)) (+ a d c))))

;(second-test 5)