;; renamed lambdas/lets: 6
 
(define plus (<change>
      (lambda (n1 n2)
         (lambda (f)
            (lambda (x)
               ((n1 f) ((n2 f) x)))))
      (lambda (_n10 _n20)
         (lambda (_f0)
            (lambda (_x0)
               ((_n10 _f0) ((_n20 _f0) _x0)))))))
 
(define mult (lambda (n1 n2)
      (lambda (f)
         (n2 (n1 f)))))
 
(define pred (lambda (n)
      (<change>
         (lambda (f)
            (lambda (x)
               (((n (lambda (g) (lambda (h) (h (g f))))) (lambda (ignored) x)) (lambda (id) id))))
         (lambda (_f0)
            (lambda (_x0)
               (((n (lambda (_g0) (lambda (_h0) (_h0 (_g0 _f0))))) (lambda (_ignored0) _x0)) (lambda (_id0) _id0)))))))
 
(define sub (<change>
      (lambda (n1 n2)
         ((n2 pred) n1))
      (lambda (_n10 _n20)
         ((_n20 pred) _n10))))
 
(define church0? (<change>
      (lambda (n)
         ((n (lambda (x) #f)) #t))
      (lambda (_n0)
         ((_n0 (lambda (_x0) #f)) #t))))
 
(define church=? (lambda (n1 n2)
      (if (church0? n1)
         (church0? n2)
         (if (church0? n2)
            #f
            (church=? (sub n1 church1) (sub n2 church1))))))
 
(define church0 (<change>
      (lambda (f)
         (lambda (x)
            x))
      (lambda (_f0)
         (lambda (_x0)
            _x0))))
 
(define church1 (lambda (f)
      (lambda (x)
         (f x))))
 
(define church2 (<change>
      (lambda (f)
         (lambda (x)
            (f (f x))))
      (lambda (_f0)
         (lambda (_x0)
            (_f0 (_f0 _x0))))))
 
(define church3 (lambda (f)
      (lambda (x)
         (f (f (f x))))))
 
(church=?
   (mult church2 (plus church1 church3))
   (plus (mult church2 church1) (mult church2 church3)))
 
