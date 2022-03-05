(not 5)
(<= 5 6)
(< 5 6)
(> 5 6)
(>= 5 6)

(define test
 (lambda (x)
  (<update>
   (if (< x 5)
    (begin
     (+ x x)
     (display x)
     (+ x 1)
     (display x))
     x)
   (if (>= x 5)
       x
      (begin
          (+ x x)
          (display x)
          (+ x 1)
          (display x))))))
(test 5)
(define (test2 x)
(<update>
(if (> x 6)
    (display "c")
    (display "d"))
(if (not (> x 6))
    (display "d")
    (display "c"))))

(test2 5)

(define (test3 x)
(let ((smthing 5))
(- x 3)
((lambda (a)
(+ x 3)
(<change>
(if  (< 5 x)
    (display "c")
    (display "d"))
(if (>= 5 x)
    (display "d")
    (display "c")))) 5)))

(test3 5)


