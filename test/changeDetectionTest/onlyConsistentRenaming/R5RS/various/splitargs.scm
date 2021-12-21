;; renamed lambdas/lets: 2
 
(define sanity-check (<change>
      (lambda (x)
         (eq? x x))
      (lambda (_x0)
         (eq? _x0 _x0))))
 
(define random-bool (<change>
      (lambda ()
         (= (random 2) 0))
      (lambda ()
         (= (random 2) 0))))
 
(sanity-check (random-bool))
 
