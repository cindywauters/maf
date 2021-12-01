(define test-with-vector-in-lambda
  (<change>
    (lambda (a)
      (let ((vec (make-vector 1)))
        (vector-set! vec 0 a)))
    (lambda (b)
      (let ((vec (make-vector 1)))
        (vector-set! vec 0 b)))))

(test-with-vector-in-lambda 5)