; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 0
(letrec ((random-bool (lambda ()
                        (= (random 2) 0)))
         (f (lambda (x)
              (if (random-bool) x (g (cons 'f x)))))
         (g (lambda (x)
              (if (random-bool) x (f (cons 'g x))))))
   (<change>
      ()
      ())
   (<change>
      ()
      (display f))
   (f ()))