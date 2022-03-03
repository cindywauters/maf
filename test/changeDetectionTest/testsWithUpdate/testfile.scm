(not 5)
(<= 5 6)
(< 5 6)
(> 5 6)
(>= 5 6)

(define (test3 x)
(<update>
(if  (< 5 x)
    (display "c")
    (display "d"))
(if (not (< 5 x))
    (display "d")
    (display "c"))))

(test3 5)


