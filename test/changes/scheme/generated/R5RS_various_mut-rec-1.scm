; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
(letrec ((even? (lambda (x)
                  (if (= x 0) #t (odd? (- x 1)))))
         (odd? (lambda (x)
                 (if (= x 0) #f (even? (- x 1))))))
   (<change>
      ()
      (display even?))
   (even? 4))