;; renamed lambdas/lets: 1
 
(<change>
   (letrec ((count (lambda (n)
                     (if (= n 0) "done" (count (- n 1))))))
      (count 10))
   (letrec ((_count0 (lambda (_n0)
                       (if (= _n0 0) "done" (_count0 (- _n0 1))))))
      (_count0 10)))
 
