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

(define (with-vectors)
  (define vec (make-vector 1))
  (vector-set! vec 0 1)
  (<change>
   (vector-set! vec 0 (lambda (a) a))
   (vector-set! vec 0 (lambda (b) b)))
  (vector-ref vec 0)
  vec)

(with-vectors)

(define (program-that-uses-with-vectors)
  (define a-vec (with-vectors))
  (vector-ref a-vec 0))

(program-that-uses-with-vectors)

(define (with-vectors-finegrained)
  (define vec (make-vector 1))
   (vector-set! vec 0 (<change>
      (lambda (a) a)
      (lambda (b) b)))
  (vector-ref vec 0)
  vec)

(with-vectors-finegrained)

(define test-with-vector-in-lambda-outer
  (lambda (a)
    (let ((vec (make-vector 1)))
      (<change>
       (vector-set! vec 0 (lambda (b) (+ a b)))
       (vector-set! vec 0 (lambda (c) (+ a c)))))))

(test-with-vector-in-lambda-outer 5)

(define test-with-vector-in-lambda-outer2
  (lambda (a)
    (<change>
     (let ((vec (make-vector 1)))
       (vector-set! vec 0 (lambda (b) (+ a b))))
     (let ((vector (make-vector 1)))
       (vector-set! vector 0 (lambda (c) (+ a c)))))))

(test-with-vector-in-lambda-outer2 5)

(define test-with-vector-in-lambda
  (<change>
    (lambda (a)
      (let ((vec (make-vector 1)))
        (vector-set! vec 0 a)))
    (lambda (b)
      (let ((vec (make-vector 1)))
        (vector-set! vec 0 b)))))

(test-with-vector-in-lambda 5)

(define test-with-vector-in-lambda-outer2
  (lambda (a)
    (<change>
     (let ((vec (make-vector 1)))
       (vector-set! vec 0 (lambda (b) (+ a b))))
     (let ((vector (make-vector 1)))
       (vector-set! vector 0 (lambda (b) (+ a b)))))))

(test-with-vector-in-lambda-outer2 5)

(define test-with-cons
  (<change>
   (list 1 5 (lambda (a) a))
   (list 1 5 (lambda (b) b))))

test-with-cons

(define test-cons-in-vec
  (lambda (a)
    (<change>
     (let ((vec (make-vector 1))
           (b 5))
       (vector-set! vec 0 (cons a b)))
     (let ((vec (make-vector 1))
           (c 5))
       (vector-set! vec 0 (cons a c))))))

(test-cons-in-vec 5)

(define test-vec-in-cons
  (lambda (a)
    (<change>
     (let* ((b 5)
            (vec (make-vector b)))
       (cons 1 vec))
     (let* ((c 5)
            (vec (make-vector c)))
       (cons 1 vec)))))

(test-vec-in-cons 5)

(define (z arg1 arg2) arg1)

(define test-vec-in-cons2
  (lambda (a)
    (<change>
     (let ((b 5))
       (z 1 (make-vector b)))
     (let ((c 5))
       (z 1 (make-vector c))))))

(test-vec-in-cons2 5)