; Changes:
; * removed: 1
; * added: 3
; * swaps: 2
; * negated predicates: 0
(letrec ((inport #f)
         (nl #f)
         (nw #f)
         (nc #f)
         (inword #f)
         (wcport (lambda (port)
                   (let ((x (read-char port)))
                      (if (eof-object? x)
                         (begin
                            (list nl nw nc))
                         (begin
                            (set! nc (+ nc 1))
                            (<change>
                               ()
                               (set! nc (+ nc 1)))
                            (<change>
                               (if (char=? x #\
) (set! nl (+ nl 1)) #f)
                               (if (let ((__or_res (char=? x #\ ))) (if __or_res __or_res (char=? x #\
)))
                                  (set! inword #f)
                                  (if (not inword)
                                     (begin
                                        (set! inword #t)
                                        (set! nw (+ nw 1)))
                                     #f)))
                            (<change>
                               (if (let ((__or_res (char=? x #\ ))) (if __or_res __or_res (char=? x #\
)))
                                  (set! inword #f)
                                  (if (not inword)
                                     (begin
                                        (set! nw (+ nw 1))
                                        (set! inword #t))
                                     #f))
                               (if (char=? x #\
) (set! nl (+ nl 1)) #f))
                            (wcport port))))))
         (go (lambda ()
               (<change>
                  ()
                  "input.txt")
               (set! inport (open-input-file "input.txt"))
               (set! nl 0)
               (<change>
                  (set! nw 0)
                  ())
               (set! nc 0)
               (<change>
                  ()
                  inport)
               (set! inword #f)
               (let ((result (wcport inport)))
                  (close-input-port inport)
                  result))))
   (equal? (go) (__toplevel_cons 31102 (__toplevel_cons 851820 (__toplevel_cons 4460056 ())))))