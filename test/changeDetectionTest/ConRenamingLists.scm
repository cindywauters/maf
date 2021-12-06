(define test-with-cons
  (<change>
   (cons 5 (lambda (a) a))
   (cons 5 (lambda (b) b))))

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

(define (test-with-list)
  (<change>
   (list 1 (lambda (a) a))
   (list 1 (lambda (b) b))))

(test-with-list)

(define (nested-lists)
  (<change>
    (let ((a-vec (make-vector 1))
          (a-lambda (lambda (a) (+ a 1))))
      (list a-vec 7 (list a-lambda (cons 1 2))))
    (let ((vec (make-vector 1))
          (lam (lambda (a) (+ a 1))))
        (list vec 7 (list lam (cons 1 2))))))

(nested-lists)

(define (if-test a)
  (if a
    (<change>
      (let ((b (cons a 1)))
        (list b a))
      (let ((c (cons a 1)))
        (list c a)))
    (list a)))

(if-test 7)

(define test-with-vector-in-lambda-outer2
  (lambda (a)
    (<change>
     (let ((vec (make-vector 2)))
       (vector-set! vec 0 (lambda (b) (+ a b)))
       (vector-set! vec 1 (list 1 2 3)))
     (let ((vector (make-vector 2)))
       (vector-set! vector 0 (lambda (b) (+ a b)))
       (vector-set! vector 1 (list 1 2 3))))))

(test-with-vector-in-lambda-outer2 5)