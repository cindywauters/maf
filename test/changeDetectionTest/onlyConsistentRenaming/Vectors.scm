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
