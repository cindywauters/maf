;; renamed lambdas/lets: 2
 
(define inport #f)
 
(define nl #f)
 
(define nw #f)
 
(define nc #f)
 
(define inword #f)
 
(define wcport (lambda (port)
      (let ((x (read-char port)))
         (if (eof-object? x)
            (begin
               (list nl nw nc))
            (begin
               (set! nc (+ nc 1))
               (if (char=? x #\
) (set! nl (+ nl 1)) #f)
               (if (let ((__or_res (char=? x #\ ))) (if __or_res __or_res (char=? x #\
)))
                  (set! inword #f)
                  (if (not inword)
                     (begin
                        (set! nw (+ nw 1))
                        (set! inword #t))
                     #f))
               (wcport port))))))
 
(define go (<change>
      (lambda ()
         (set! inport (open-input-file "input.txt"))
         (set! nl 0)
         (set! nw 0)
         (set! nc 0)
         (set! inword #f)
         (let ((result (wcport inport)))
            (close-input-port inport)
            result))
      (lambda ()
         (set! inport (open-input-file "input.txt"))
         (set! nl 0)
         (set! nw 0)
         (set! nc 0)
         (set! inword #f)
         (let ((_result0 (wcport inport)))
            (close-input-port inport)
            _result0))))
 
(equal? (go) (__toplevel_cons 31102 (__toplevel_cons 851820 (__toplevel_cons 4460056 ()))))
 
