(define test-more-lambdas
  (<change>
   (lambda (a)
     (lambda (a)
       (let ((b (lambda (a) (+ a 1 ((lambda () (+ 1 7)))))))
         ((lambda () (b a))))))
   (lambda (a)
     (lambda (d)
       (let ((b (lambda (a) (+ a 1 ((lambda () (+ 1 7)))))))
         ((lambda () (b d))))))))

((test-more-lambdas 5) 10)

(define (change-in-lambda a)
  (lambda (b)
   (<change>
    (let ((c (lambda (a) (+ a 1))))
      (lambda ()(+ b (c 5))))
    (let ((d (lambda (a) (+ a 1))))
      (lambda ()(+ b (d 5)))))))

((change-in-lambda 5) 10)


(define (combine-many)
  ((change-in-lambda 5) 10)
  ((test-more-lambdas 5) 10)
  (<change>
    (let ((b 2))
      (fifth-test b))
    (let ((c 2))
      (fifth-test c))))

(combine-many)