;; change name function
(<change>
 (define (fac n)
   (if (= n 0)
       1
       (* (fac (- n 1)) n)))
 (define (fac-new n) ;; different
   (if (= n 0)
       1
       (* (fac-new (- n 1)) n)))) ;; different
(<change>
 (fac 5)
 (fac-new 5)) ;; different

;; change name function + other change
(<change>
 (define (fac n)
   (if (= n 0)
       1
       (* (fac (- n 1)) n)))
 (define (fac-new n)
   (if (= n 1) ;; also different
       1
       (* (fac-new (- n 1)) n))))
(<change>
 (fac 5)
 (fac-new 5))