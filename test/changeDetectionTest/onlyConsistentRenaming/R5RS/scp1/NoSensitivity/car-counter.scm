;; renamed lambdas/lets: 7
 
(define foldr (lambda (f base lst)
      (define foldr-aux (<change>
            (lambda (lst)
               (if (null? lst)
                  base
                  (f (car lst) (foldr-aux (cdr lst)))))
            (lambda (_lst0)
               (if (null? _lst0)
                  base
                  (f (car _lst0) (foldr-aux (cdr _lst0)))))))
      (foldr-aux lst)))
 
(define result ())
 
(define display2 (lambda (i)
      (set! result (cons i result))))
 
(define newline2 (lambda ()
      (set! result (cons 'newline result))))
 
(define error2 (<change>
      (lambda (e)
         (set! result (cons (list 'error e) result)))
      (lambda (_e0)
         (set! result (cons (list 'error _e0) result)))))
 
(define maak-buffer (lambda ()
      (let ((inhoud ()))
         (define newValue (<change>
               (lambda (value)
                  (set! inhoud (append inhoud (list value))))
               (lambda (_value0)
                  (set! inhoud (append inhoud (list _value0))))))
         (define returnSum (lambda ()
               (foldr + 0 inhoud)))
         (define flush (lambda ()
               (set! inhoud ())))
         (define value (<change>
               (lambda (pos)
                  (list-ref inhoud pos))
               (lambda (_pos0)
                  (list-ref inhoud _pos0))))
         (define dispatch (<change>
               (lambda (msg)
                  (if (eq? msg 'newValue)
                     newValue
                     (if (eq? msg 'return)
                        inhoud
                        (if (eq? msg 'returnSum)
                           (returnSum)
                           (if (eq? msg 'flush)
                              (flush)
                              (if (eq? msg 'value)
                                 value
                                 (if (eq? msg 'size)
                                    (length inhoud)
                                    (error "wrong message"))))))))
               (lambda (_msg0)
                  (if (eq? _msg0 'newValue)
                     newValue
                     (if (eq? _msg0 'return)
                        inhoud
                        (if (eq? _msg0 'returnSum)
                           (returnSum)
                           (if (eq? _msg0 'flush)
                              (flush)
                              (if (eq? _msg0 'value)
                                 value
                                 (if (eq? _msg0 'size)
                                    (length inhoud)
                                    (error "wrong message"))))))))))
         dispatch)))
 
(define buffer (maak-buffer))
 
((buffer 'newValue) 3)
 
((buffer 'newValue) 9)
 
(define res1 (if (= (buffer 'returnSum) 12)
      (if (equal? (buffer 'return) (__toplevel_cons 3 (__toplevel_cons 9 ())))
         (if (begin (buffer 'flush))
            (null? (buffer 'return))
            #f)
         #f)
      #f))
 
(define make-counter (lambda ()
      (let ((state 0))
         (define increment (<change>
               (lambda ()
                  (set! state (+ state 1)))
               (lambda ()
                  (set! state (+ state 1)))))
         (define read (lambda ()
               state))
         (define reset (lambda ()
               (set! state 0)))
         (define dispatch (lambda (msg)
               (if (eq? msg 'increment)
                  (increment)
                  (if (eq? msg 'read)
                     (read)
                     (if (eq? msg 'reset)
                        (reset)
                        (error "wrong message"))))))
         dispatch)))
 
(define maak-verkeersteller (<change>
      (lambda ()
         (let ((voorbijgereden (make-counter))
               (buffer (maak-buffer)))
            (define newCar (lambda ()
                  (voorbijgereden 'increment)))
            (define newHour (lambda ()
                  ((buffer 'newValue) (voorbijgereden 'read))
                  (voorbijgereden 'reset)))
            (define newDay (lambda ()
                  (define loop (lambda (start end)
                        (if (= start end)
                           (newline)
                           (begin
                              (display2 "Tussen ")
                              (display2 start)
                              (display2 " en ")
                              (display2 (+ start 1))
                              (display2 " uur : ")
                              (display2 ((buffer 'value) start))
                              (display2 " auto's")
                              (newline2)
                              (loop (+ start 1) end)))))
                  (if (= (buffer 'size) 24)
                     (begin
                        (loop 0 24)
                        (buffer 'flush)
                        (voorbijgereden 'reset))
                     (error2 "no 24 hours have passed"))))
            (define dispatch (lambda (msg)
                  (if (eq? msg 'newCar)
                     (newCar)
                     (if (eq? msg 'newHour)
                        (newHour)
                        (if (eq? msg 'newDay)
                           (newDay)
                           (error2 "wrong message"))))))
            dispatch))
      (lambda ()
         (let ((_voorbijgereden0 (make-counter))
               (_buffer0 (maak-buffer)))
            (define newCar (lambda ()
                  (_voorbijgereden0 'increment)))
            (define newHour (lambda ()
                  ((_buffer0 'newValue) (_voorbijgereden0 'read))
                  (_voorbijgereden0 'reset)))
            (define newDay (lambda ()
                  (define loop (lambda (_start0 _end0)
                        (if (= _start0 _end0)
                           (newline)
                           (begin
                              (display2 "Tussen ")
                              (display2 _start0)
                              (display2 " en ")
                              (display2 (+ _start0 1))
                              (display2 " uur : ")
                              (display2 ((_buffer0 'value) _start0))
                              (display2 " auto's")
                              (newline2)
                              (loop (+ _start0 1) _end0)))))
                  (if (= (_buffer0 'size) 24)
                     (begin
                        (loop 0 24)
                        (_buffer0 'flush)
                        (_voorbijgereden0 'reset))
                     (error2 "no 24 hours have passed"))))
            (define dispatch (lambda (_msg0)
                  (if (eq? _msg0 'newCar)
                     (newCar)
                     (if (eq? _msg0 'newHour)
                        (newHour)
                        (if (eq? _msg0 'newDay)
                           (newDay)
                           (error2 "wrong message"))))))
            dispatch))))
 
(define verkeersteller (maak-verkeersteller))
 
(verkeersteller 'newCar)
 
(verkeersteller 'newCar)
 
(verkeersteller 'newHour)
 
(verkeersteller 'newHour)
 
(verkeersteller 'newCar)
 
(verkeersteller 'newCar)
 
(verkeersteller 'newCar)
 
(verkeersteller 'newHour)
 
(verkeersteller 'newHour)
 
(verkeersteller 'newHour)
 
(verkeersteller 'newCar)
 
(verkeersteller 'newHour)
 
(verkeersteller 'newHour)
 
(verkeersteller 'newHour)
 
(verkeersteller 'newCar)
 
(verkeersteller 'newCar)
 
(verkeersteller 'newHour)
 
(verkeersteller 'newCar)
 
(verkeersteller 'newCar)
 
(verkeersteller 'newHour)
 
(verkeersteller 'newCar)
 
(verkeersteller 'newHour)
 
(verkeersteller 'newHour)
 
(verkeersteller 'newHour)
 
(verkeersteller 'newHour)
 
(verkeersteller 'newCar)
 
(verkeersteller 'newHour)
 
(verkeersteller 'newCar)
 
(verkeersteller 'newHour)
 
(verkeersteller 'newHour)
 
(verkeersteller 'newCar)
 
(verkeersteller 'newHour)
 
(verkeersteller 'newHour)
 
(verkeersteller 'newCar)
 
(verkeersteller 'newHour)
 
(verkeersteller 'newHour)
 
(verkeersteller 'newCar)
 
(verkeersteller 'newCar)
 
(verkeersteller 'newHour)
 
(verkeersteller 'newCar)
 
(verkeersteller 'newHour)
 
(verkeersteller 'newCar)
 
(verkeersteller 'newDay)
 
(verkeersteller 'newHour)
 
(verkeersteller 'newDay)
 
(equal?
   result
   (__toplevel_cons
      'newline
      (__toplevel_cons
         'newline
         (__toplevel_cons
            " auto's"
            (__toplevel_cons
               1
               (__toplevel_cons
                  " uur : "
                  (__toplevel_cons
                     24
                     (__toplevel_cons
                        " en "
                        (__toplevel_cons
                           23
                           (__toplevel_cons
                              "Tussen "
                              (__toplevel_cons
                                 'newline
                                 (__toplevel_cons
                                    " auto's"
                                    (__toplevel_cons
                                       1
                                       (__toplevel_cons
                                          " uur : "
                                          (__toplevel_cons
                                             23
                                             (__toplevel_cons
                                                " en "
                                                (__toplevel_cons
                                                   22
                                                   (__toplevel_cons
                                                      "Tussen "
                                                      (__toplevel_cons
                                                         'newline
                                                         (__toplevel_cons
                                                            " auto's"
                                                            (__toplevel_cons
                                                               2
                                                               (__toplevel_cons
                                                                  " uur : "
                                                                  (__toplevel_cons
                                                                     22
                                                                     (__toplevel_cons
                                                                        " en "
                                                                        (__toplevel_cons
                                                                           21
                                                                           (__toplevel_cons
                                                                              "Tussen "
                                                                              (__toplevel_cons
                                                                                 'newline
                                                                                 (__toplevel_cons
                                                                                    " auto's"
                                                                                    (__toplevel_cons
                                                                                       0
                                                                                       (__toplevel_cons
                                                                                          " uur : "
                                                                                          (__toplevel_cons
                                                                                             21
                                                                                             (__toplevel_cons
                                                                                                " en "
                                                                                                (__toplevel_cons
                                                                                                   20
                                                                                                   (__toplevel_cons
                                                                                                      "Tussen "
                                                                                                      (__toplevel_cons
                                                                                                         'newline
                                                                                                         (__toplevel_cons
                                                                                                            " auto's"
                                                                                                            (__toplevel_cons
                                                                                                               1
                                                                                                               (__toplevel_cons
                                                                                                                  " uur : "
                                                                                                                  (__toplevel_cons
                                                                                                                     20
                                                                                                                     (__toplevel_cons
                                                                                                                        " en "
                                                                                                                        (__toplevel_cons
                                                                                                                           19
                                                                                                                           (__toplevel_cons
                                                                                                                              "Tussen "
                                                                                                                              (__toplevel_cons
                                                                                                                                 'newline
                                                                                                                                 (__toplevel_cons
                                                                                                                                    " auto's"
                                                                                                                                    (__toplevel_cons
                                                                                                                                       0
                                                                                                                                       (__toplevel_cons
                                                                                                                                          " uur : "
                                                                                                                                          (__toplevel_cons
                                                                                                                                             19
                                                                                                                                             (__toplevel_cons
                                                                                                                                                " en "
                                                                                                                                                (__toplevel_cons
                                                                                                                                                   18
                                                                                                                                                   (__toplevel_cons
                                                                                                                                                      "Tussen "
                                                                                                                                                      (__toplevel_cons
                                                                                                                                                         'newline
                                                                                                                                                         (__toplevel_cons
                                                                                                                                                            " auto's"
                                                                                                                                                            (__toplevel_cons
                                                                                                                                                               1
                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                  " uur : "
                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                     18
                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                        " en "
                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                           17
                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                              "Tussen "
                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                 'newline
                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                    " auto's"
                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                       0
                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                          " uur : "
                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                             17
                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                " en "
                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                   16
                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                      "Tussen "
                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                         'newline
                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                            " auto's"
                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                               1
                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                  " uur : "
                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                     16
                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                        " en "
                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                           15
                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                              "Tussen "
                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                 'newline
                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                    " auto's"
                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                       1
                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                          " uur : "
                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                             15
                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                " en "
                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                   14
                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                      "Tussen "
                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                         'newline
                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                            " auto's"
                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                               0
                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                  " uur : "
                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                     14
                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                        " en "
                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                           13
                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                              "Tussen "
                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                 'newline
                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                    " auto's"
                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                       0
                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                          " uur : "
                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                             13
                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                " en "
                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                   12
                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                      "Tussen "
                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                         'newline
                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                            " auto's"
                                                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                                                               0
                                                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                                                  " uur : "
                                                                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                                                                     12
                                                                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                                                                        " en "
                                                                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                                                                           11
                                                                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                                                                              "Tussen "
                                                                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                 'newline
                                                                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                    " auto's"
                                                                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                       1
                                                                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                          " uur : "
                                                                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                             11
                                                                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                " en "
                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                   10
                                                                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                      "Tussen "
                                                                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                         'newline
                                                                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                            " auto's"
                                                                                                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                               2
                                                                                                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                  " uur : "
                                                                                                                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                     10
                                                                                                                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                        " en "
                                                                                                                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                           9
                                                                                                                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                              "Tussen "
                                                                                                                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                 'newline
                                                                                                                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                    " auto's"
                                                                                                                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                       2
                                                                                                                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                          " uur : "
                                                                                                                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                             9
                                                                                                                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                " en "
                                                                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                   8
                                                                                                                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                      "Tussen "
                                                                                                                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                         'newline
                                                                                                                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                            " auto's"
                                                                                                                                                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                               0
                                                                                                                                                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                  " uur : "
                                                                                                                                                                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                     8
                                                                                                                                                                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                        " en "
                                                                                                                                                                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                           7
                                                                                                                                                                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                              "Tussen "
                                                                                                                                                                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                 'newline
                                                                                                                                                                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                    " auto's"
                                                                                                                                                                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                       0
                                                                                                                                                                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                          " uur : "
                                                                                                                                                                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                             7
                                                                                                                                                                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                " en "
                                                                                                                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                   6
                                                                                                                                                                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                      "Tussen "
                                                                                                                                                                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                         'newline
                                                                                                                                                                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                            " auto's"
                                                                                                                                                                                                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                               1
                                                                                                                                                                                                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                  " uur : "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                     6
                                                                                                                                                                                                                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                        " en "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                           5
                                                                                                                                                                                                                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "Tussen "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 'newline
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    " auto's"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       0
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          " uur : "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             5
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                " en "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   4
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "Tussen "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         'newline
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            " auto's"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               0
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  " uur : "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     4
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        " en "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           3
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "Tussen "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 'newline
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    " auto's"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       3
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          " uur : "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             3
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                " en "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   2
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "Tussen "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         'newline
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            " auto's"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               0
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  " uur : "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     2
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        " en "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           1
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "Tussen "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 'newline
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    " auto's"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       2
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          " uur : "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             1
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                " en "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   0
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "Tussen "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      (__toplevel_cons (__toplevel_cons 'error2 (__toplevel_cons "no 24 hours have passed" ())) ())))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 
