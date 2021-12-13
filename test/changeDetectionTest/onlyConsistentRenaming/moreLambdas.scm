(define (z a b) (+ a b))

(define eighth-test
  (<change>
   (let ((a (lambda (d e) (- d e)))
         (b 5))
    (z (a 1 2) b))
   (let ((d (lambda (f e) (- f e)))
        (b 5))
    (z (d 1 2) b))))

eighth-test

(define test-many-lambdas
  (<change>
   (lambda (a)
     (lambda (a)
       (let ((b (lambda (a) (+ a 1))))
         ((lambda () (b a))))))
   (lambda (a)
     (lambda (d)
       (let ((b (lambda (a) (+ a 1))))
         ((lambda () (b d))))))))

((test-many-lambdas 5) 10)