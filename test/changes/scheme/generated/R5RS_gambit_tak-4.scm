; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
(letrec ((tak (lambda (x y z)
                (if (not (< y x))
                   z
                   (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y)))))
         (res (= 7 (tak 18 12 6))))
   (<change>
      ()
      res)
   res)