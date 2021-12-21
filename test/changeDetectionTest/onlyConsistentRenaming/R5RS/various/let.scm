;; renamed lambdas/lets: 1
 
(<change>
   (let ((x 1))
      (letrec ((x 2))
         x)
      (let* ((x 3))
         x)
      (letrec ((x 4))
         x)
      x)
   (let ((_x0 1))
      (letrec ((_x1 2))
         _x1)
      (let* ((_x2 3))
         _x2)
      (letrec ((_x3 4))
         _x3)
      _x0))
 
