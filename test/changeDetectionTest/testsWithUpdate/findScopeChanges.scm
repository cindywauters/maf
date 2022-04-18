(define a 'a)
(define b 10)
(if (not a)
    b
    a)

(<update>
(if (not (number? a))
    b
    a)
(if (number? a)
        a
        b))
