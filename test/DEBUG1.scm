; Does higher-order influence or create implicit paths?
(define (phi x) (not x))
(define (chi x) x)
(or ((<change> phi chi) #t)
    ((<change> phi chi) #f))