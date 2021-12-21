;; renamed lambdas/lets: 1
 
(<change>
   (letrec ((h (lambda ()
                 ()))
            (i 1)
            (res (begin
                   (h)
                   i)))
      res)
   (letrec ((_h0 (lambda ()
                   ()))
            (_i0 1)
            (_res0 (begin
                     (_h0)
                     _i0)))
      _res0))
 
