; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
(if (equal? (append (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 ()))) (__toplevel_cons 4 (__toplevel_cons 5 ()))) (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 ()))))))
   (if (<change> (equal? (append (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 ()))) ()) (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 ())))) (not (equal? (append (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 ()))) ()) (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 ()))))))
      (if (equal? (append () (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 ())))) (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 ()))))
         (null? (append () ()))
         #f)
      #f)
   #f)