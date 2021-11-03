; Changes:
; * removed: 0
; * added: 0
; * swaps: 1
; * negated predicates: 1
(letrec ((schuif-in! (lambda (l1 l2)
                       (if (<change> (null? (cdr l1)) (not (null? (cdr l1))))
                          (begin
                             (set-cdr! l1 l2)
                             'ok)
                          (if (null? l2)
                             'ok
                             (let ((rest1 (cdr l1))
                                   (rest2 (cdr l2)))
                                (<change>
                                   (set-cdr! l1 l2)
                                   (set-cdr! l2 rest1))
                                (<change>
                                   (set-cdr! l2 rest1)
                                   (set-cdr! l1 l2))
                                (schuif-in! rest1 rest2))))))
         (lijst1 (__toplevel_cons 1 (__toplevel_cons 3 (__toplevel_cons 5 ()))))
         (lijst2 (__toplevel_cons 2 (__toplevel_cons 4 (__toplevel_cons 6 (__toplevel_cons 8 ()))))))
   (schuif-in! lijst1 lijst2)
   (equal?
      lijst1
      (__toplevel_cons
         1
         (__toplevel_cons
            2
            (__toplevel_cons
               3
               (__toplevel_cons 4 (__toplevel_cons 5 (__toplevel_cons 6 (__toplevel_cons 8 ())))))))))