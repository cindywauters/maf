; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
(letrec ((h (lambda ()
              (<change>
                 ()
                 ())
              ()))
         (i 1)
         (res (begin
                (h)
                i)))
   res)