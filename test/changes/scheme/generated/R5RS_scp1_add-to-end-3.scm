; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
(letrec ((add-to-end (lambda (e l)
                       (if (<change> (null? l) (not (null? l)))
                          (cons e ())
                          (cons (car l) (add-to-end e (cdr l)))))))
   (if (equal? (add-to-end 999 (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 ())))))) (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 (__toplevel_cons 999 ())))))))
      (if (equal? (add-to-end 999 ()) (__toplevel_cons 999 ()))
         (equal? (add-to-end 999 (__toplevel_cons 1 ())) (__toplevel_cons 1 (__toplevel_cons 999 ())))
         #f)
      #f))