; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 0
(letrec ((map (lambda (f lst)
                (if (null? lst)
                   ()
                   (cons (f (car lst)) (map f (cdr lst))))))
         (inc (lambda (n)
                (+ n 1))))
   (<change>
      ()
      (display __toplevel_cons))
   (map inc (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 ())))))