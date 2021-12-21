;; renamed lambdas/lets: 3
 
(define phi (lambda (x1 x2 x3 x4)
      (if (<change> (let ((__or_res x1)) (if __or_res __or_res (let ((__or_res (not x2))) (if __or_res __or_res (not x3))))) (let ((___or_res0 x1)) (if ___or_res0 ___or_res0 (let ((___or_res1 (not x2))) (if ___or_res1 ___or_res1 (not x3))))))
         (if (let ((__or_res (not x2))) (if __or_res __or_res (not x3)))
            (<change>
               (let ((__or_res x4))
                  (if __or_res __or_res x2))
               (let ((___or_res0 x4))
                  (if ___or_res0 ___or_res0 x2)))
            #f)
         #f)))
 
(define try (lambda (f)
      (let ((__or_res (f #t)))
         (if __or_res __or_res (f #f)))))
 
(define sat-solve-4 (lambda (p)
      (try
         (lambda (n1)
            (try
               (lambda (n2)
                  (try
                     (<change>
                        (lambda (n3)
                           (try (lambda (n4) (p n1 n2 n3 n4))))
                        (lambda (_n30)
                           (try (lambda (_n40) (p n1 n2 _n30 _n40))))))))))))
 
(sat-solve-4 phi)
 
