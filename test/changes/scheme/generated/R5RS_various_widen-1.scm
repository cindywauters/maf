; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((g (lambda ()
              1))
         (f (lambda (n)
              (if (= n 0) 0 (+ (f (- n 1)) (g))))))
   (<change>
      ()
      10)
   (f 10))