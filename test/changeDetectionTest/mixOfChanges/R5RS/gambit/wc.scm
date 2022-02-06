;; renamed lambdas/lets: 2

(define inport #f)

(define nl #f)

(define nw #f)

(define nc #f)

(define inword #f)

(define wcport (lambda (port)
      (<change>
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
                  (wcport port))))
         (let ((_x0 (read-char port)))
            (if (eof-object? _x0)
               (begin
                  (list nl nw nc))
               (begin
                  (set! nc (+ nc 1))
                  (if (char=? _x0 #\
) (set! nl (+ nl 1)) #f)
                  (if (let ((___or_res0 (char=? _x0 #\ ))) (if ___or_res0 ___or_res0 (char=? _x0 #\
)))
                     (set! inword #f)
                     (if (not inword)
                        (begin
                           (set! nw (+ nw 2)) ;; NOT RENAMING 1 -> 2
                           (set! inword #t))
                        #f))
                  (wcport port)))))))

(define go (lambda ()
      (set! inport (open-input-file "input.txt"))
      (set! nl 0)
      (set! nw 0)
      (set! nc 0)
      (set! inword #f)
      (<change>
         (let ((result (wcport inport)))
            (close-input-port inport)
            result)
         (let ((_result0 (wcport inport)))
          ;;  (close-input-port inport)  ;; NO RENAMING, removed a function
            _result0))))

(equal? (go) (cons 31102 (cons 851820 (cons 4460056 ()))))