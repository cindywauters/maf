; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
(letrec ((even? (lambda (x)
                  (if (<change> (= x 0) (not (= x 0)))
                     #t
                     (odd? (- x 1)))))
         (odd? (lambda (x)
                 (if (= x 0) #f (even? (- x 1))))))
   (even? 4))