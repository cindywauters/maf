;; renamed lambdas/lets: 5
 
(define result ())

(define output (lambda (i)
      (set! result (cons i result))))
 
(define linebreak (<change>
      (lambda ()
         (set! result (cons 'linebreak result)))
      (lambda ()
         (set! result (cons 'linebreak result)))))

(define create-counter (lambda ()
      (let ((value 0))
         (define reset
               (lambda ()
                  (set! value 0)
                  'ok))
         (define next
               (lambda ()
                  (set! value (+ 1 value))
                  'ok))
         (define increase (lambda (x)
               (set! value (+ value x))))
         (define dispatch (<change>
               (lambda (msg)
                  (if (eq? msg 'reset)
                     reset
                     (if (eq? msg 'next)
                        next
                        (if (eq? msg 'read)
                           value
                           (if (eq? msg 'increase)
                              increase
                              (error "wrong message: " msg))))))
               (lambda (_msg0)
                  (if (eq? _msg0 'reset)
                     reset
                     (if (eq? _msg0 'next)
                        next
                        (if (eq? _msg0 'read)
                           value
                           (if (eq? _msg0 'increase)
                              increase
                              (error "wrong message: " _msg0))))))))
         dispatch)))
 
(define make-scorebord (<change>
      (lambda ()
         (let ((c-home (create-counter))
               (c-visit (create-counter)))
            (define reset (lambda ()
                  ((c-home 'reset))
                  ((c-visit 'reset))
                  'ok))
            (define read (lambda ()
                  (let ((c1 (c-home 'read))
                        (c2 (c-visit 'read)))
                     (output c1)
                     (output "-")
                     (output c2)
                     (linebreak)
                     'ok)))
            (define score (lambda (team n)
                  (if (not (let ((__or_res (= n 1))) (if __or_res __or_res (let ((__or_res (= n 2))) (if __or_res __or_res (= n 3))))))
                     (begin
                        (linebreak)
                        (output "De score kan slechts 1, 2 of 3 zijn!")
                        (linebreak)
                        'ok)
                     (if (eq? team 'home)
                        (begin
                           ((c-home 'increase) n)
                           'ok)
                        (if (eq? team 'visit)
                           (begin
                              ((c-visit 'increase) n)
                              'ok)
                           (error "wrong team: " team))))))
            (define dispatch (lambda (msg)
                  (if (eq? msg 'reset)
                     reset
                     (if (eq? msg 'read)
                        read
                        (if (eq? msg 'score)
                           score
                           (error "wrong message: " msg))))))
            dispatch))
      (lambda ()
         (let ((_c-home0 (create-counter))
               (_c-visit0 (create-counter)))
            (define reset (lambda ()
                  ((_c-home0 'reset))
                  ((_c-visit0 'reset))
                  'ok))
            (define read (lambda ()
                  (let ((_c10 (_c-home0 'read))
                        (_c20 (_c-visit0 'read)))
                     (output _c10)
                     (output "-")
                     (output _c20)
                     (linebreak)
                     'ok)))
            (define score (lambda (_team0 _n0)
                  (if (not (let ((___or_res0 (= _n0 1))) (if ___or_res0 ___or_res0 (let ((___or_res1 (= _n0 2))) (if ___or_res1 ___or_res1 (= _n0 3))))))
                     (begin
                        (linebreak)
                        (output "De score kan slechts 1, 2 of 3 zijn!")
                        (linebreak)
                        'ok)
                     (if (eq? _team0 'home)
                        (begin
                           ((_c-home0 'increase) _n0)
                           'ok)
                        (if (eq? _team0 'visit)
                           (begin
                              ((_c-visit0 'increase) _n0)
                              'ok)
                           (error "wrong team: " _team0))))))
            (define dispatch (lambda (_msg0)
                  (if (eq? _msg0 'reset)
                     reset
                     (if (eq? _msg0 'read)
                        read
                        (if (eq? _msg0 'score)
                           score
                           (error "wrong message: " _msg0))))))
            dispatch))))
 
(define bord (make-scorebord))
 
((bord 'read))
 
((bord 'score) 'home 2)
 
((bord 'read))
 
((bord 'score) 'visit 5)
 
((bord 'read))
 
((bord 'reset))
 
((bord 'read))
 
(equal?
   result
   (__toplevel_cons
      'linebreak
      (__toplevel_cons
         0
         (__toplevel_cons
            "-"
            (__toplevel_cons
               0
               (__toplevel_cons
                  'linebreak
                  (__toplevel_cons
                     0
                     (__toplevel_cons
                        "-"
                        (__toplevel_cons
                           2
                           (__toplevel_cons
                              'linebreak
                              (__toplevel_cons
                                 "De score kan slechts 1, 2 of 3 zijn!"
                                 (__toplevel_cons
                                    'linebreak
                                    (__toplevel_cons
                                       'linebreak
                                       (__toplevel_cons
                                          0
                                          (__toplevel_cons
                                             "-"
                                             (__toplevel_cons
                                                2
                                                (__toplevel_cons 'linebreak (__toplevel_cons 0 (__toplevel_cons "-" (__toplevel_cons 0 ()))))))))))))))))))))
 
