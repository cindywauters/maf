; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
(letrec ((*max* 20001)
         (test (lambda (x y)
                 (if (= x *max*)
                    x
                    (test (- x (+ (* y 2) (/ x (abs y)))) (- y (+ (* x 2) (/ y (abs x)))))))))
   (<change>
      ()
      1)
   (test 1 1))