; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
(letrec ((count (lambda (n)
                  (if (= n 0) "done" (count (- n 1))))))
   (<change>
      ()
      count)
   (count 10))