;; Easy test with one lambda, should be false
(define first-test
  (<change> (lambda (l m) (lambda (n o) (+ l m n o)))
            (lambda (a c) (lambda (n o) (- a c)))))

((first-test 1 2) 3 4)

;; Test with lambda + let, should be true
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

;; lambda + let*, should be false
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

;; Lambda + letrec, should be false
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

;; Let + let*, should be true
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

;; Let + let* + lambda, should be false
(define (sixth-test a)
  (<change>
   (let ((a 1)
         (b 2))
     (let* ((c (+ a b)))
       ((lambda (x) (z x c)) 5)))
   (let ((a 1)
         (b 3))
     (let* ((f (+ a b)))
       ((lambda (x) (z x f)) 5)))))

(sixth-test 10)

;; let with lambda in a binding, should be false due to the z being a seperate function in the first lambda but a let binding in the second
(define seventh-test
  (<change>
   (let ((a (lambda (d e) (- d e)))
        (b 5))
    (z (a 1 2) b))
   (let ((z (lambda (d e) (- d e)))
        (b 5))
    (z (z 1 2) b))))

seventh-test

;; Let with lambda in a binding, should be true (not the same problem as above because the z doesn't change for old vs new)
(define eighth-test
  (<change>
   (let ((a (lambda (d e) (- d e)))
         (b 5))
    (z (a 1 2) b))
   (let ((d (lambda (f e) (- f e)))
        (b 5))
    (z (d 1 2) b))))

eighth-test

;; nested let in binding of a let, should be true
;(define nineth-test
;  (<change>
;   (let ((b (let ((c 5))
;              (+ c 6))))
;     (+ a b))
;   (let ((b (let ((d 5))
;              (+ d 6))))
;     (+ a b))))

;nineth-test

;; nested let in binding of a let, should be false
;(define tenth-test
;  (<change>
;   (let ((b (let ((c 5))
;             (+ c 6))))
;     (+ a b))
;  (let ((b (let ((d 6))
;              (+ d 6))))
;     (+ a b))))

;tenth-test

