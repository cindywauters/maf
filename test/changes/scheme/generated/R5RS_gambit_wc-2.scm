; Changes:
; * removed: 0
; * added: 1
; * swaps: 3
; * negated predicates: 1
(letrec ((inport #f)
         (nl #f)
         (nw #f)
         (nc #f)
         (inword #f)
         (wcport (lambda (port)
                   (let ((x (read-char port)))
                      (if (eof-object? x)
                         (begin
                            (<change>
                               ()
                               nc)
                            (list nl nw nc))
                         (begin
                            (<change>
                               (set! nc (+ nc 1))
                               (if (char=? x #\
) (set! nl (+ nl 1)) #f))
                            (<change>
                               (if (char=? x #\
) (set! nl (+ nl 1)) #f)
                               (set! nc (+ nc 1)))
                            (if (let ((__or_res (char=? x #\ ))) (if (<change> __or_res (not __or_res)) __or_res (char=? x #\
)))
                               (set! inword #f)
                               (if (not inword)
                                  (begin
                                     (<change>
                                        (set! nw (+ nw 1))
                                        (set! inword #t))
                                     (<change>
                                        (set! inword #t)
                                        (set! nw (+ nw 1))))
                                  #f))
                            (wcport port))))))
         (go (lambda ()
               (set! inport (open-input-file "input.txt"))
               (set! nl 0)
               (<change>
                  (set! nw 0)
                  (set! nc 0))
               (<change>
                  (set! nc 0)
                  (set! nw 0))
               (set! inword #f)
               (let ((result (wcport inport)))
                  (close-input-port inport)
                  result))))
   (equal? (go) (__toplevel_cons 31102 (__toplevel_cons 851820 (__toplevel_cons 4460056 ())))))