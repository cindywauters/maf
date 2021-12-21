;; renamed lambdas/lets: 1
 
(letrec ((f (<change>
              (lambda ()
                 (f))
              (lambda ()
                 (f)))))
   (f))
 
