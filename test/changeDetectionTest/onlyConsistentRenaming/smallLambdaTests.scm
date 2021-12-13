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

(define (third-test a)
  (<change>
   (let ((a 1)
         (b 2))
     (let* ((c (+ a b)))
       ((lambda (x) (+ x c)) 5)))
   (let ((a 1)
         (b 2))
     (let* ((f (+ a b)))
       ((lambda (x) (+ x f)) 5)))))

(third-test 10)