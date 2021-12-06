(define first-test
  (<change> (lambda (a b) (+ a b))
            (lambda (a d) (+ a d))))

(first-test 5 10)

(define second-test
  (<change>
   (lambda (a)
     (let ((b 2)
           (c 3))
       (+ a b c)))
   (lambda (a)
     (let ((d 2)
           (e 3))
       (+ a d e)))))

(second-test 2)

(define (fifth-test a)
  (<change>
   (let ((a 1)
         (b 2))
     (let* ((c (+ a b)))
       ((lambda (x) (+ x c)) 5)))
   (let ((a 1)
         (b 2))
     (let* ((f (+ a b)))
       ((lambda (x) (+ x f)) 5)))))

(fifth-test 10)

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
