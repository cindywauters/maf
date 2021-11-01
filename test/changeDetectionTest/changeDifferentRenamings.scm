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

(fourth-test 1)

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

(define (sixth-test a)
  (<change>
   (let ((a 1)
         (b 2))
     (let* ((c (+ a b)))
       ((lambda (x) (z x c)) 5)))
   (let ((a 1)
         (b 2))
     (let* ((f (+ a b)))
       ((lambda (x) (z x f)) 5)))))

(sixth-test 10)

(define seventh-test
  (<change>
   (let ((a (lambda (d e) (- d e)))
        (b 5))
    (z (a 1 2) b))
   (let ((z (lambda (d e) (- d e)))
        (b 5))
    (z (z 1 2) b))))

seventh-test

(define eighth-test
  (<change>
   (let ((a (lambda (d e) (- d e)))
         (b 5))
    (z (a 1 2) b))
   (let ((d (lambda (d e) (- d e)))
        (b 5))
    (d (z 1 2) b))))

eighth-test