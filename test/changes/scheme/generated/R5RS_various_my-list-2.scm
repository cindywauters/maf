; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 0
(letrec ((my-cons (lambda (el lst)
                    (cons el lst)))
         (my-list (my-cons 1 (my-cons 2 (my-cons 3 ())))))
   my-list)