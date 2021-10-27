(define first-test
  (<change> (lambda (l m) (lambda (n o) (+ l m n o)))
            (lambda (a c) (- a c))))

((first-test 1 2) 3 4)

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


(define third-test
  (<change>
   (lambda (a)
     (let* ((k 2)
           (l 3))
       (- a k l)))
   (lambda (a)
     (let* ((m 2)
           (o 3))
       (+ a m o)))))

(third-test 1)

(define fourth-test
  (<change>
   (lambda (a)
     (letrec ((z 2)
           (w 3))
       (- a z w)))
   (lambda (a)
     (letrec ((y 2)
           (x 3))
       (+ a y x)))))

(something 1)