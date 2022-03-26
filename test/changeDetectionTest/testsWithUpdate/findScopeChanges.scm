(define (test n)
  (<update>
    (if  (<= n 2)
        (display n)
        n)
    (if (> n 2)
         n
         (display n))))

(test 5)
(test #f)
(display 6)