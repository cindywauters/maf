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

(define (test2 x)
(<update>
(if (not (> x 6))
    (display "c")
    (display "d"))
(if (> x 6)
    (display "d")
    (display "c"))))

(test2 5)

(define (test3 x)
(<update>
(if (< 5 x)
    (display "c")
    (display "d"))
(if (>= 5 x)
    (display "d")
    (display "c"))))

(test3 5)


(test 5)
