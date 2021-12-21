;; renamed lambdas/lets: 3
 
(define 1- (<change>
      (lambda (x)
         (- x 1))
      (lambda (_x0)
         (- _x0 1))))
 
(define 1+ (<change>
      (lambda (x)
         (+ 1 x))
      (lambda (_x0)
         (+ 1 _x0))))
 
(define rec-add (lambda (a b)
      (if (= b 0) a (1+ (rec-add a (1- b))))))
 
(define iter-add (<change>
      (lambda (a b)
         (if (= a 0)
            b
            (if (< a 0)
               (iter-add (1+ a) (1- b))
               (if (> a 0) (iter-add (1- a) (1+ b)) #f))))
      (lambda (_a0 _b0)
         (if (= _a0 0)
            _b0
            (if (< _a0 0)
               (iter-add (1+ _a0) (1- _b0))
               (if (> _a0 0) (iter-add (1- _a0) (1+ _b0)) #f))))))
 
(= 9 (rec-add 4 5) (iter-add 4 5))
 
