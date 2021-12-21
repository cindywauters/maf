;; renamed lambdas/lets: 41
 
(define count-pairs (<change>
      (lambda (x)
         (if (not (pair? x))
            0
            (+ (count-pairs (car x)) (count-pairs (cdr x)) 1)))
      (lambda (_x0)
         (if (not (pair? _x0))
            0
            (+ (count-pairs (car _x0)) (count-pairs (cdr _x0)) 1)))))
 
(define ret3 (cons 'a (cons 'b (cons 'c ()))))
 
(define ret4 (<change>
      (let ((last (cons 'c ())))
         (cons last (cons 'b last)))
      (let ((_last0 (cons 'c ())))
         (cons _last0 (cons 'b _last0)))))
 
(define ret7 (<change>
      (let* ((last (cons 'c ()))
             (middle (cons last last)))
         (cons middle middle))
      (let* ((_last0 (cons 'c ()))
             (_middle0 (cons _last0 _last0)))
         (cons _middle0 _middle0))))
 
(if (= (count-pairs ret3) 3)
   (if (= (count-pairs ret4) 4)
      (= (count-pairs ret7) 7)
      #f)
   #f)
 
(define result2 ())
 
(define display2 (<change>
      (lambda (i)
         (set! result2 (cons i result2)))
      (lambda (_i0)
         (set! result2 (cons _i0 result2)))))
 
(define make-ring2 (<change>
      (lambda (n)
         (let ((last2 (cons 0 ())))
            (define build-list2 (lambda (n)
                  (if (= n 0) last2 (cons n (build-list2 (- n 1))))))
            (let ((ring2 (build-list2 n)))
               (set-cdr! last2 ring2)
               ring2)))
      (lambda (_n0)
         (let ((_last20 (cons 0 ())))
            (define build-list2 (lambda (_n1)
                  (if (= _n1 0)
                     _last20
                     (cons _n1 (build-list2 (- _n1 1))))))
            (let ((_ring20 (build-list2 _n0)))
               (set-cdr! _last20 _ring20)
               _ring20)))))
 
(define print-ring2 (<change>
      (lambda (r)
         (define aux (lambda (l)
               (if (not (null? l))
                  (if (eq? (cdr l) r)
                     (begin
                        (display2 " ")
                        (display2 (car l))
                        (display2 "..."))
                     (begin
                        (display2 " ")
                        (display2 (car l))
                        (aux (cdr l))))
                  #f)))
         (aux r)
         #t)
      (lambda (_r0)
         (define aux (lambda (_l0)
               (if (not (null? _l0))
                  (if (eq? (cdr _l0) _r0)
                     (begin
                        (display2 " ")
                        (display2 (car _l0))
                        (display2 "..."))
                     (begin
                        (display2 " ")
                        (display2 (car _l0))
                        (aux (cdr _l0))))
                  #f)))
         (aux _r0)
         #t)))
 
(define r2 (make-ring2 3))
 
(print-ring2 r2)
 
(print-ring2 (cdr r2))
 
(equal?
   result2
   (__toplevel_cons
      "..."
      (__toplevel_cons
         3
         (__toplevel_cons
            " "
            (__toplevel_cons
               0
               (__toplevel_cons
                  " "
                  (__toplevel_cons
                     1
                     (__toplevel_cons
                        " "
                        (__toplevel_cons
                           2
                           (__toplevel_cons
                              " "
                              (__toplevel_cons
                                 "..."
                                 (__toplevel_cons
                                    0
                                    (__toplevel_cons
                                       " "
                                       (__toplevel_cons
                                          1
                                          (__toplevel_cons
                                             " "
                                             (__toplevel_cons 2 (__toplevel_cons " " (__toplevel_cons 3 (__toplevel_cons " " ())))))))))))))))))))
 
(define result3 ())
 
(define display3 (<change>
      (lambda (i)
         (set! result3 (cons i result3)))
      (lambda (_i0)
         (set! result3 (cons _i0 result3)))))
 
(define make-ring3 (<change>
      (lambda (n)
         (let ((last3 (cons 0 ())))
            (define build-list3 (lambda (n)
                  (if (= n 0) last3 (cons n (build-list3 (- n 1))))))
            (let ((ring3 (build-list3 n)))
               (set-cdr! last3 ring3)
               ring3)))
      (lambda (_n0)
         (let ((_last30 (cons 0 ())))
            (define build-list3 (lambda (_n1)
                  (if (= _n1 0)
                     _last30
                     (cons _n1 (build-list3 (- _n1 1))))))
            (let ((_ring30 (build-list3 _n0)))
               (set-cdr! _last30 _ring30)
               _ring30)))))
 
(define print-ring3 (<change>
      (lambda (r)
         (define aux (lambda (l)
               (if (not (null? l))
                  (if (eq? (cdr l) r)
                     (begin
                        (display3 " ")
                        (display3 (car l))
                        (display3 "..."))
                     (begin
                        (display " ")
                        (display3 (car l))
                        (aux (cdr l))))
                  #f)))
         (aux r)
         #t)
      (lambda (_r0)
         (define aux (lambda (_l0)
               (if (not (null? _l0))
                  (if (eq? (cdr _l0) _r0)
                     (begin
                        (display3 " ")
                        (display3 (car _l0))
                        (display3 "..."))
                     (begin
                        (display " ")
                        (display3 (car _l0))
                        (aux (cdr _l0))))
                  #f)))
         (aux _r0)
         #t)))
 
(define right-rotate (<change>
      (lambda (r)
         (define iter (lambda (l)
               (if (eq? (cdr l) r) l (iter (cdr l)))))
         (iter r))
      (lambda (_r0)
         (define iter (lambda (_l0)
               (if (eq? (cdr _l0) _r0) _l0 (iter (cdr _l0)))))
         (iter _r0))))
 
(define r3 (make-ring3 3))
 
(print-ring3 (right-rotate r3))
 
(equal?
   result3
   (__toplevel_cons
      "..."
      (__toplevel_cons
         1
         (__toplevel_cons
            " "
            (__toplevel_cons
               2
               (__toplevel_cons
                  " "
                  (__toplevel_cons 3 (__toplevel_cons " " (__toplevel_cons 0 (__toplevel_cons " " ()))))))))))
 
(define ret4-2 (let ((last (cons 'c ())))
      (cons last (cons 'b last))))
 
(define ret7-2 (<change>
      (let* ((last (cons 'c ()))
             (middle (cons last last)))
         (cons middle middle))
      (let* ((_last0 (cons 'c ()))
             (_middle0 (cons _last0 _last0)))
         (cons _middle0 _middle0))))
 
(define retno-2 (<change>
      (let* ((last (cons 'c ()))
             (lst (cons 'a (cons 'b last))))
         (set-cdr! last lst)
         lst)
      (let* ((_last0 (cons 'c ()))
             (_lst0 (cons 'a (cons 'b _last0))))
         (set-cdr! _last0 _lst0)
         _lst0)))
 
(define cycles? (lambda (lst)
      (define find-cycles? (<change>
            (lambda (current path)
               (if (null? current)
                  #f
                  (if (memq current path)
                     #t
                     (find-cycles? (cdr current) (cons current path)))))
            (lambda (_current0 _path0)
               (if (null? _current0)
                  #f
                  (if (memq _current0 _path0)
                     #t
                     (find-cycles? (cdr _current0) (cons _current0 _path0)))))))
      (find-cycles? lst ())))
 
(if (not (cycles? ()))
   (if (not (cycles? (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 ())))))
      (if (not (cycles? ret4-2))
         (if (cycles? retno-2)
            (if (not (cycles? ret7-2))
               (cycles? (cons 'a (cons 'b retno-2)))
               #f)
            #f)
         #f)
      #f)
   #f)
 
(define result4 ())
 
(define display4 (<change>
      (lambda (i)
         (set! result4 (cons i result4)))
      (lambda (_i0)
         (set! result4 (cons _i0 result4)))))
 
(define make-ring4 (<change>
      (lambda (n)
         (let ((last4 (cons 0 ())))
            (define build-list4 (lambda (n)
                  (if (= n 0) last4 (cons n (build-list4 (- n 1))))))
            (let ((ring4 (build-list4 n)))
               (set-cdr! last4 ring4)
               ring4)))
      (lambda (_n0)
         (let ((_last40 (cons 0 ())))
            (define build-list4 (lambda (_n1)
                  (if (= _n1 0)
                     _last40
                     (cons _n1 (build-list4 (- _n1 1))))))
            (let ((_ring40 (build-list4 _n0)))
               (set-cdr! _last40 _ring40)
               _ring40)))))
 
(define print-ring4 (<change>
      (lambda (r)
         (define aux (lambda (l)
               (if (not (null? l))
                  (if (eq? (cdr l) r)
                     (begin
                        (display4 " ")
                        (display4 (car l))
                        (display4 "..."))
                     (begin
                        (display4 " ")
                        (display4 (car l))
                        (aux (cdr l))))
                  #f)))
         (aux r)
         #t)
      (lambda (_r0)
         (define aux (lambda (_l0)
               (if (not (null? _l0))
                  (if (eq? (cdr _l0) _r0)
                     (begin
                        (display4 " ")
                        (display4 (car _l0))
                        (display4 "..."))
                     (begin
                        (display4 " ")
                        (display4 (car _l0))
                        (aux (cdr _l0))))
                  #f)))
         (aux _r0)
         #t)))
 
(define copy-ring (<change>
      (lambda (r)
         (define last ())
         (define aux (lambda (l)
               (if (eq? (cdr l) r)
                  (begin
                     (set! last (cons (car l) ()))
                     last)
                  (cons (car l) (aux (cdr l))))))
         (let ((first (aux r)))
            (set-cdr! last first)
            first))
      (lambda (_r0)
         (define last ())
         (define aux (lambda (_l0)
               (if (eq? (cdr _l0) _r0)
                  (begin
                     (set! last (cons (car _l0) ()))
                     last)
                  (cons (car _l0) (aux (cdr _l0))))))
         (let ((_first0 (aux _r0)))
            (set-cdr! last _first0)
            _first0))))
 
(define r4 (make-ring4 3))
 
(define s4 (copy-ring r4))
 
(print-ring4 s4)
 
(set-car! s4 999)
 
(print-ring4 s4)
 
(print-ring4 r4)
 
(equal?
   result4
   (__toplevel_cons
      "..."
      (__toplevel_cons
         0
         (__toplevel_cons
            " "
            (__toplevel_cons
               1
               (__toplevel_cons
                  " "
                  (__toplevel_cons
                     2
                     (__toplevel_cons
                        " "
                        (__toplevel_cons
                           3
                           (__toplevel_cons
                              " "
                              (__toplevel_cons
                                 "..."
                                 (__toplevel_cons
                                    0
                                    (__toplevel_cons
                                       " "
                                       (__toplevel_cons
                                          1
                                          (__toplevel_cons
                                             " "
                                             (__toplevel_cons
                                                2
                                                (__toplevel_cons
                                                   " "
                                                   (__toplevel_cons
                                                      999
                                                      (__toplevel_cons
                                                         " "
                                                         (__toplevel_cons
                                                            "..."
                                                            (__toplevel_cons
                                                               0
                                                               (__toplevel_cons
                                                                  " "
                                                                  (__toplevel_cons
                                                                     1
                                                                     (__toplevel_cons
                                                                        " "
                                                                        (__toplevel_cons 2 (__toplevel_cons " " (__toplevel_cons 3 (__toplevel_cons " " ()))))))))))))))))))))))))))))
 
(define result5 ())
 
(define display5 (<change>
      (lambda (i)
         (set! result5 (cons i result5)))
      (lambda (_i0)
         (set! result5 (cons _i0 result5)))))
 
(define make-ring5 (<change>
      (lambda (n)
         (let ((last5 (cons 0 ())))
            (define build-list5 (lambda (n)
                  (if (= n 0) last5 (cons n (build-list5 (- n 1))))))
            (let ((ring5 (build-list5 n)))
               (set-cdr! last5 ring5)
               ring5)))
      (lambda (_n0)
         (let ((_last50 (cons 0 ())))
            (define build-list5 (lambda (_n1)
                  (if (= _n1 0)
                     _last50
                     (cons _n1 (build-list5 (- _n1 1))))))
            (let ((_ring50 (build-list5 _n0)))
               (set-cdr! _last50 _ring50)
               _ring50)))))
 
(define print-ring5 (lambda (r)
      (define aux (<change>
            (lambda (l)
               (if (not (null? l))
                  (if (eq? (cdr l) r)
                     (begin
                        (display5 " ")
                        (display5 (car l))
                        (display5 "..."))
                     (begin
                        (display5 " ")
                        (display5 (car l))
                        (aux (cdr l))))
                  #f))
            (lambda (_l0)
               (if (not (null? _l0))
                  (if (eq? (cdr _l0) r)
                     (begin
                        (display5 " ")
                        (display5 (car _l0))
                        (display5 "..."))
                     (begin
                        (display5 " ")
                        (display5 (car _l0))
                        (aux (cdr _l0))))
                  #f))))
      (aux r)
      #t))
 
(define copy-ring5 (lambda (r)
      (define last ())
      (define aux (<change>
            (lambda (l)
               (if (eq? (cdr l) r)
                  (begin
                     (set! last (cons (car l) ()))
                     last)
                  (cons (car l) (aux (cdr l)))))
            (lambda (_l0)
               (if (eq? (cdr _l0) r)
                  (begin
                     (set! last (cons (car _l0) ()))
                     last)
                  (cons (car _l0) (aux (cdr _l0)))))))
      (<change>
         (let ((first (aux r)))
            (set-cdr! last first)
            first)
         (let ((_first0 (aux r)))
            (set-cdr! last _first0)
            _first0))))
 
(define Josephus (<change>
      (lambda (r n)
         (define remove-nth! (lambda (l n)
               (if (<= n 2)
                  (begin
                     (set-cdr! l (cddr l))
                     (cdr l))
                  (remove-nth! (cdr l) (- n 1)))))
         (define iter (lambda (l)
               (print-ring5 l)
               (if (eq? l (cdr l))
                  (car l)
                  (iter (remove-nth! l n)))))
         (if (= n 1)
            (car (right-rotate r))
            (iter (copy-ring5 r))))
      (lambda (_r0 _n0)
         (define remove-nth! (lambda (_l0 _n1)
               (if (<= _n1 2)
                  (begin
                     (set-cdr! _l0 (cddr _l0))
                     (cdr _l0))
                  (remove-nth! (cdr _l0) (- _n1 1)))))
         (define iter (lambda (_l1)
               (print-ring5 _l1)
               (if (eq? _l1 (cdr _l1))
                  (car _l1)
                  (iter (remove-nth! _l1 _n0)))))
         (if (= _n0 1)
            (car (right-rotate _r0))
            (iter (copy-ring5 _r0))))))
 
(define ring5 (make-ring5 5))
 
(Josephus ring5 5)
 
(print-ring5 ring5)
 
(equal?
   result5
   (__toplevel_cons
      "..."
      (__toplevel_cons
         0
         (__toplevel_cons
            " "
            (__toplevel_cons
               1
               (__toplevel_cons
                  " "
                  (__toplevel_cons
                     2
                     (__toplevel_cons
                        " "
                        (__toplevel_cons
                           3
                           (__toplevel_cons
                              " "
                              (__toplevel_cons
                                 4
                                 (__toplevel_cons
                                    " "
                                    (__toplevel_cons
                                       5
                                       (__toplevel_cons
                                          " "
                                          (__toplevel_cons
                                             "..."
                                             (__toplevel_cons
                                                5
                                                (__toplevel_cons
                                                   " "
                                                   (__toplevel_cons
                                                      "..."
                                                      (__toplevel_cons
                                                         5
                                                         (__toplevel_cons
                                                            " "
                                                            (__toplevel_cons
                                                               3
                                                               (__toplevel_cons
                                                                  " "
                                                                  (__toplevel_cons
                                                                     "..."
                                                                     (__toplevel_cons
                                                                        3
                                                                        (__toplevel_cons
                                                                           " "
                                                                           (__toplevel_cons
                                                                              4
                                                                              (__toplevel_cons
                                                                                 " "
                                                                                 (__toplevel_cons
                                                                                    5
                                                                                    (__toplevel_cons
                                                                                       " "
                                                                                       (__toplevel_cons
                                                                                          "..."
                                                                                          (__toplevel_cons
                                                                                             3
                                                                                             (__toplevel_cons
                                                                                                " "
                                                                                                (__toplevel_cons
                                                                                                   4
                                                                                                   (__toplevel_cons
                                                                                                      " "
                                                                                                      (__toplevel_cons
                                                                                                         5
                                                                                                         (__toplevel_cons
                                                                                                            " "
                                                                                                            (__toplevel_cons
                                                                                                               0
                                                                                                               (__toplevel_cons
                                                                                                                  " "
                                                                                                                  (__toplevel_cons
                                                                                                                     "..."
                                                                                                                     (__toplevel_cons
                                                                                                                        2
                                                                                                                        (__toplevel_cons
                                                                                                                           " "
                                                                                                                           (__toplevel_cons
                                                                                                                              3
                                                                                                                              (__toplevel_cons
                                                                                                                                 " "
                                                                                                                                 (__toplevel_cons
                                                                                                                                    4
                                                                                                                                    (__toplevel_cons
                                                                                                                                       " "
                                                                                                                                       (__toplevel_cons
                                                                                                                                          5
                                                                                                                                          (__toplevel_cons
                                                                                                                                             " "
                                                                                                                                             (__toplevel_cons
                                                                                                                                                0
                                                                                                                                                (__toplevel_cons
                                                                                                                                                   " "
                                                                                                                                                   (__toplevel_cons
                                                                                                                                                      "..."
                                                                                                                                                      (__toplevel_cons
                                                                                                                                                         0
                                                                                                                                                         (__toplevel_cons
                                                                                                                                                            " "
                                                                                                                                                            (__toplevel_cons
                                                                                                                                                               1
                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                  " "
                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                     2
                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                        " "
                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                           3
                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                              " "
                                                                                                                                                                              (__toplevel_cons 4 (__toplevel_cons " " (__toplevel_cons 5 (__toplevel_cons " " ()))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 
(define count-pairs2 (<change>
      (lambda (lst)
         (let ((path ()))
            (define count (lambda (current)
                  (if (null? current)
                     0
                     (if (not (pair? current))
                        0
                        (if (memq current path)
                           0
                           (begin
                              (set! path (cons current path))
                              (+ 1 (count (car current)) (count (cdr current)))))))))
            (count lst)))
      (lambda (_lst0)
         (let ((_path0 ()))
            (define count (lambda (_current0)
                  (if (null? _current0)
                     0
                     (if (not (pair? _current0))
                        0
                        (if (memq _current0 _path0)
                           0
                           (begin
                              (set! _path0 (cons _current0 _path0))
                              (+ 1 (count (car _current0)) (count (cdr _current0)))))))))
            (count _lst0)))))
 
(define ret3-3 (cons 'a (cons 'b (cons 'c ()))))
 
(define ret4-3 (<change>
      (let ((last (cons 'c ())))
         (cons last (cons 'b last)))
      (let ((_last0 (cons 'c ())))
         (cons _last0 (cons 'b _last0)))))
 
(define ret7-3 (let* ((last (cons 'c ()))
          (middle (cons last last)))
      (cons middle middle)))
 
(define retno-3 (<change>
      (let* ((last (cons 'c ()))
             (lst (cons 'a (cons 'b last))))
         (set-cdr! last lst)
         lst)
      (let* ((_last0 (cons 'c ()))
             (_lst0 (cons 'a (cons 'b _last0))))
         (set-cdr! _last0 _lst0)
         _lst0)))
 
(= 3 (count-pairs2 ret3-3) (count-pairs2 ret4-3) (count-pairs2 ret7-3) (count-pairs2 retno-3))
 
(define find-last (lambda (lijst)
      (if (null? lijst)
         (error "find-last -- lijst heeft geen laatste element")
         (<change>
            (let ((next (cdr lijst)))
               (if (null? next) lijst (find-last next)))
            (let ((_next0 (cdr lijst)))
               (if (null? _next0) lijst (find-last _next0)))))))
 
(define flatten! (lambda (lijst)
      (if (null? lijst)
         ()
         (<change>
            (let* ((sublist (car lijst))
                   (restlist (flatten! (cdr lijst))))
               (if (null? sublist)
                  restlist
                  (let ((last (find-last sublist)))
                     (set-cdr! last restlist)
                     sublist)))
            (let* ((_sublist0 (car lijst))
                   (_restlist0 (flatten! (cdr lijst))))
               (if (null? _sublist0)
                  _restlist0
                  (let ((_last0 (find-last _sublist0)))
                     (set-cdr! _last0 _restlist0)
                     _sublist0)))))))
 
(define atom? (lambda (x)
      (not (pair? x))))
 
(define flatten2! (<change>
      (lambda (lijst)
         (let ((hulpcel (cons 'dummy lijst)))
            (define flatten-aux! (lambda (prev current)
                  (if (null? current)
                     (cdr hulpcel)
                     (if (null? (car current))
                        (begin
                           (set-cdr! prev (cdr current))
                           (flatten-aux! prev (cdr current)))
                        (if (pair? (car current))
                           (begin
                              (set-cdr! prev (flatten2! (car current)))
                              (flatten-aux! (find-last prev) (cdr current)))
                           (if (null? (cdr prev))
                              (begin
                                 (set-cdr! prev current)
                                 (flatten-aux! (cdr prev) (cdr current)))
                              (if (atom? (car current))
                                 (flatten-aux! (cdr prev) (cdr current))
                                 #f)))))))
            (flatten-aux! hulpcel lijst)
            (cdr hulpcel)))
      (lambda (_lijst0)
         (let ((_hulpcel0 (cons 'dummy _lijst0)))
            (define flatten-aux! (lambda (_prev0 _current0)
                  (if (null? _current0)
                     (cdr _hulpcel0)
                     (if (null? (car _current0))
                        (begin
                           (set-cdr! _prev0 (cdr _current0))
                           (flatten-aux! _prev0 (cdr _current0)))
                        (if (pair? (car _current0))
                           (begin
                              (set-cdr! _prev0 (flatten2! (car _current0)))
                              (flatten-aux! (find-last _prev0) (cdr _current0)))
                           (if (null? (cdr _prev0))
                              (begin
                                 (set-cdr! _prev0 _current0)
                                 (flatten-aux! (cdr _prev0) (cdr _current0)))
                              (if (atom? (car _current0))
                                 (flatten-aux! (cdr _prev0) (cdr _current0))
                                 #f)))))))
            (flatten-aux! _hulpcel0 _lijst0)
            (cdr _hulpcel0)))))
 
(if (equal? (flatten! (__toplevel_cons (__toplevel_cons 1 (__toplevel_cons 2 ())) (__toplevel_cons (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 ()))) (__toplevel_cons (__toplevel_cons 6 ()) (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) ()))))) (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 (__toplevel_cons 6 (__toplevel_cons 7 (__toplevel_cons 8 ())))))))))
   (if (equal? (flatten! (__toplevel_cons () (__toplevel_cons (__toplevel_cons 1 (__toplevel_cons 2 ())) (__toplevel_cons (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 ()))) (__toplevel_cons () (__toplevel_cons (__toplevel_cons 6 ()) (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) ()))))))) (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 (__toplevel_cons 6 (__toplevel_cons 7 (__toplevel_cons 8 ())))))))))
      (if (equal? (flatten2! (__toplevel_cons (__toplevel_cons 1 (__toplevel_cons (__toplevel_cons 2 (__toplevel_cons 3 ())) (__toplevel_cons 4 ()))) (__toplevel_cons 5 (__toplevel_cons 6 (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) ()))))) (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 (__toplevel_cons 6 (__toplevel_cons 7 (__toplevel_cons 8 ())))))))))
         (if (equal? (flatten2! (__toplevel_cons (__toplevel_cons 1 (__toplevel_cons 2 ())) (__toplevel_cons (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 ()))) (__toplevel_cons (__toplevel_cons 6 ()) (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) ()))))) (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 (__toplevel_cons 6 (__toplevel_cons 7 (__toplevel_cons 8 ())))))))))
            (if (equal? (flatten2! (__toplevel_cons () (__toplevel_cons (__toplevel_cons 1 (__toplevel_cons 2 ())) (__toplevel_cons (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 ()))) (__toplevel_cons () (__toplevel_cons (__toplevel_cons 6 ()) (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) ()))))))) (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 (__toplevel_cons 6 (__toplevel_cons 7 (__toplevel_cons 8 ())))))))))
               (equal?
                  (flatten2!
                     (__toplevel_cons
                        1
                        (__toplevel_cons
                           2
                           (__toplevel_cons
                              (__toplevel_cons
                                 3
                                 (__toplevel_cons
                                    (__toplevel_cons 4 (__toplevel_cons 5 ()))
                                    (__toplevel_cons
                                       6
                                       (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) (__toplevel_cons 9 ())))))
                              (__toplevel_cons 10 ())))))
                  (__toplevel_cons
                     1
                     (__toplevel_cons
                        2
                        (__toplevel_cons
                           3
                           (__toplevel_cons
                              4
                              (__toplevel_cons
                                 5
                                 (__toplevel_cons
                                    6
                                    (__toplevel_cons 7 (__toplevel_cons 8 (__toplevel_cons 9 (__toplevel_cons 10 ())))))))))))
               #f)
            #f)
         #f)
      #f)
   #f)
 
(define result6 ())
 
(define display6 (<change>
      (lambda (i)
         (set! result6 (cons i result6)))
      (lambda (_i0)
         (set! result6 (cons _i0 result6)))))
 
(define kw-lijst (lambda (lst)
      (define loop (<change>
            (lambda (l)
               (let ((rest (cdr l))
                     (n (list (* (car l) (car l)))))
                  (set-cdr! l n)
                  (set-cdr! n rest)
                  (if (not (eq? rest lst)) (loop rest) #f)))
            (lambda (_l0)
               (let ((_rest0 (cdr _l0))
                     (_n0 (list (* (car _l0) (car _l0)))))
                  (set-cdr! _l0 _n0)
                  (set-cdr! _n0 _rest0)
                  (if (not (eq? _rest0 lst)) (loop _rest0) #f)))))
      (loop lst)
      lst))
 
(define print-ring6 (<change>
      (lambda (r)
         (define aux (lambda (l)
               (if (not (null? l))
                  (if (eq? (cdr l) r)
                     (begin
                        (display6 " ")
                        (display6 (car l))
                        (display6 "..."))
                     (begin
                        (display6 " ")
                        (display6 (car l))
                        (aux (cdr l))))
                  #f)))
         (aux r)
         #t)
      (lambda (_r0)
         (define aux (lambda (_l0)
               (if (not (null? _l0))
                  (if (eq? (cdr _l0) _r0)
                     (begin
                        (display6 " ")
                        (display6 (car _l0))
                        (display6 "..."))
                     (begin
                        (display6 " ")
                        (display6 (car _l0))
                        (aux (cdr _l0))))
                  #f)))
         (aux _r0)
         #t)))
 
(define last-cons (cons 3 ()))
 
(define test-lst (cons 1 (cons 4 last-cons)))
 
(set-cdr! last-cons test-lst)
 
(print-ring6 (kw-lijst test-lst))
 
(equal?
   result6
   (__toplevel_cons
      "..."
      (__toplevel_cons
         9
         (__toplevel_cons
            " "
            (__toplevel_cons
               3
               (__toplevel_cons
                  " "
                  (__toplevel_cons
                     16
                     (__toplevel_cons
                        " "
                        (__toplevel_cons
                           4
                           (__toplevel_cons
                              " "
                              (__toplevel_cons 1 (__toplevel_cons " " (__toplevel_cons 1 (__toplevel_cons " " ()))))))))))))))
 
(define schuif-in! (<change>
      (lambda (l1 l2)
         (if (null? (cdr l1))
            (begin
               (set-cdr! l1 l2)
               'ok)
            (if (null? l2)
               'ok
               (let ((rest1 (cdr l1))
                     (rest2 (cdr l2)))
                  (set-cdr! l1 l2)
                  (set-cdr! l2 rest1)
                  (schuif-in! rest1 rest2)))))
      (lambda (_l10 _l20)
         (if (null? (cdr _l10))
            (begin
               (set-cdr! _l10 _l20)
               'ok)
            (if (null? _l20)
               'ok
               (let ((_rest10 (cdr _l10))
                     (_rest20 (cdr _l20)))
                  (set-cdr! _l10 _l20)
                  (set-cdr! _l20 _rest10)
                  (schuif-in! _rest10 _rest20)))))))
 
(define lijst1 (__toplevel_cons 1 (__toplevel_cons 3 (__toplevel_cons 5 ()))))
 
(define lijst2 (__toplevel_cons 2 (__toplevel_cons 4 (__toplevel_cons 6 (__toplevel_cons 8 ())))))
 
(schuif-in! lijst1 lijst2)
 
(equal?
   lijst1
   (__toplevel_cons
      1
      (__toplevel_cons
         2
         (__toplevel_cons
            3
            (__toplevel_cons 4 (__toplevel_cons 5 (__toplevel_cons 6 (__toplevel_cons 8 ()))))))))
 
(define ontdubbel! (<change>
      (lambda (lijst)
         (let ((deEven ())
               (deOneven ()))
            (define ontdubbel-iter (lambda (prevE prevO restLijst)
                  (if (null? restLijst)
                     (begin
                        (set-cdr! prevE ())
                        (set-cdr! prevO ())
                        (cons deEven deOneven))
                     (if (even? (car restLijst))
                        (begin
                           (if (null? prevE)
                              (set! deEven restLijst)
                              (set-cdr! prevE restLijst))
                           (ontdubbel-iter restLijst prevO (cdr restLijst)))
                        (begin
                           (if (null? prevO)
                              (set! deOneven restLijst)
                              (set-cdr! prevO restLijst))
                           (ontdubbel-iter prevE restLijst (cdr restLijst)))))))
            (ontdubbel-iter deEven deOneven lijst)))
      (lambda (_lijst0)
         (let ((_deEven0 ())
               (_deOneven0 ()))
            (define ontdubbel-iter (lambda (_prevE0 _prevO0 _restLijst0)
                  (if (null? _restLijst0)
                     (begin
                        (set-cdr! _prevE0 ())
                        (set-cdr! _prevO0 ())
                        (cons _deEven0 _deOneven0))
                     (if (even? (car _restLijst0))
                        (begin
                           (if (null? _prevE0)
                              (set! _deEven0 _restLijst0)
                              (set-cdr! _prevE0 _restLijst0))
                           (ontdubbel-iter _restLijst0 _prevO0 (cdr _restLijst0)))
                        (begin
                           (if (null? _prevO0)
                              (set! _deOneven0 _restLijst0)
                              (set-cdr! _prevO0 _restLijst0))
                           (ontdubbel-iter _prevE0 _restLijst0 (cdr _restLijst0)))))))
            (ontdubbel-iter _deEven0 _deOneven0 _lijst0)))))
 
(equal?
   (ontdubbel!
      (__toplevel_cons
         1
         (__toplevel_cons
            2
            (__toplevel_cons
               3
               (__toplevel_cons
                  4
                  (__toplevel_cons
                     5
                     (__toplevel_cons
                        6
                        (__toplevel_cons 7 (__toplevel_cons 8 (__toplevel_cons 9 (__toplevel_cons 10 ())))))))))))
   (__toplevel_cons
      (__toplevel_cons
         2
         (__toplevel_cons 4 (__toplevel_cons 6 (__toplevel_cons 8 (__toplevel_cons 10 ())))))
      (__toplevel_cons
         1
         (__toplevel_cons 3 (__toplevel_cons 5 (__toplevel_cons 7 (__toplevel_cons 9 ())))))))
 
(define insert-aux! (<change>
      (lambda (lst lst2)
         (set-cdr! lst2 ())
         (if (null? (cdr lst))
            (set-cdr! lst lst2)
            (insert-aux! (cdr lst) lst2))
         lst)
      (lambda (_lst0 _lst20)
         (set-cdr! _lst20 ())
         (if (null? (cdr _lst0))
            (set-cdr! _lst0 _lst20)
            (insert-aux! (cdr _lst0) _lst20))
         _lst0)))
 
(define insert! (<change>
      (lambda (lst1 lst2)
         (if (not (null? lst1))
            (begin
               (insert! (cdr lst1) (cdr lst2))
               (insert-aux! (car lst1) lst2)
               lst1)
            #f))
      (lambda (_lst10 _lst20)
         (if (not (null? _lst10))
            (begin
               (insert! (cdr _lst10) (cdr _lst20))
               (insert-aux! (car _lst10) _lst20)
               _lst10)
            #f))))
 
(if (equal? (insert-aux! (__toplevel_cons 'a (__toplevel_cons 12 (__toplevel_cons 'q ()))) (__toplevel_cons 'v (__toplevel_cons 'w (__toplevel_cons 'x (__toplevel_cons 'y (__toplevel_cons 'z ())))))) (__toplevel_cons 'a (__toplevel_cons 12 (__toplevel_cons 'q (__toplevel_cons 'v ())))))
   (equal?
      (insert!
         (__toplevel_cons
            (__toplevel_cons 'a (__toplevel_cons 12 (__toplevel_cons 'q ())))
            (__toplevel_cons
               (__toplevel_cons 'b (__toplevel_cons 13 ()))
               (__toplevel_cons
                  (__toplevel_cons 'c (__toplevel_cons 14 (__toplevel_cons 'r (__toplevel_cons 's ()))))
                  (__toplevel_cons
                     (__toplevel_cons 'f (__toplevel_cons 18 ()))
                     (__toplevel_cons (__toplevel_cons 'j (__toplevel_cons 22 (__toplevel_cons 't ()))) ())))))
         (__toplevel_cons
            'v
            (__toplevel_cons 'w (__toplevel_cons 'x (__toplevel_cons 'y (__toplevel_cons 'z ()))))))
      (__toplevel_cons
         (__toplevel_cons 'a (__toplevel_cons 12 (__toplevel_cons 'q (__toplevel_cons 'v ()))))
         (__toplevel_cons
            (__toplevel_cons 'b (__toplevel_cons 13 (__toplevel_cons 'w ())))
            (__toplevel_cons
               (__toplevel_cons
                  'c
                  (__toplevel_cons 14 (__toplevel_cons 'r (__toplevel_cons 's (__toplevel_cons 'x ())))))
               (__toplevel_cons
                  (__toplevel_cons 'f (__toplevel_cons 18 (__toplevel_cons 'y ())))
                  (__toplevel_cons
                     (__toplevel_cons 'j (__toplevel_cons 22 (__toplevel_cons 't (__toplevel_cons 'z ()))))
                     ()))))))
   #f)
 
(define all-but-interval (<change>
      (lambda (lst min max)
         (define aux (lambda (last-smaller-cons aux-lst)
               (if (null? aux-lst)
                  (set-cdr! last-smaller-cons ())
                  (if (< (car aux-lst) min)
                     (aux aux-lst (cdr aux-lst))
                     (if (> (car aux-lst) max)
                        (set-cdr! last-smaller-cons aux-lst)
                        (aux last-smaller-cons (cdr aux-lst)))))))
         (aux lst lst)
         lst)
      (lambda (_lst0 _min0 _max0)
         (define aux (lambda (_last-smaller-cons0 _aux-lst0)
               (if (null? _aux-lst0)
                  (set-cdr! _last-smaller-cons0 ())
                  (if (< (car _aux-lst0) _min0)
                     (aux _aux-lst0 (cdr _aux-lst0))
                     (if (> (car _aux-lst0) _max0)
                        (set-cdr! _last-smaller-cons0 _aux-lst0)
                        (aux _last-smaller-cons0 (cdr _aux-lst0)))))))
         (aux _lst0 _lst0)
         _lst0)))
 
(if (equal? (all-but-interval (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 (__toplevel_cons 6 ())))))) 2 4) (__toplevel_cons 1 (__toplevel_cons 5 (__toplevel_cons 6 ()))))
   (if (equal? (all-but-interval (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 ()))))) 2 2) (__toplevel_cons 1 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 ())))))
      (equal?
         (all-but-interval
            (__toplevel_cons
               1
               (__toplevel_cons 2 (__toplevel_cons 5 (__toplevel_cons 6 (__toplevel_cons 7 ())))))
            3
            9)
         (__toplevel_cons 1 (__toplevel_cons 2 ())))
      #f)
   #f)
 
(define first-el (<change>
      (lambda (best)
         (if (not (null? best)) (caar best) #f))
      (lambda (_best0)
         (if (not (null? _best0)) (caar _best0) #f))))
 
(define smaller? (<change>
      (lambda (el1 el2)
         (string<? (symbol->string el1) (symbol->string el2)))
      (lambda (_el10 _el20)
         (string<? (symbol->string _el10) (symbol->string _el20)))))
 
(define same? (<change>
      (lambda (el1 el2)
         (equal? el1 el2))
      (lambda (_el10 _el20)
         (equal? _el10 _el20))))
 
(define merge (<change>
      (lambda (best1 best2)
         (define merge-in (lambda (curr1 curr2 prev)
               (if (null? curr1)
                  (set-cdr! prev curr2)
                  (if (null? curr2)
                     (set-cdr! prev curr1)
                     (if (same? (first-el curr1) (first-el curr2))
                        (begin
                           (set-cdr! prev curr1)
                           (merge-in (cdr curr1) (cdr curr2) curr1))
                        (if (smaller? (first-el curr1) (first-el curr2))
                           (begin
                              (set-cdr! prev curr1)
                              (merge-in (cdr curr1) curr2 curr1))
                           (begin
                              (set-cdr! prev curr2)
                              (merge-in curr1 (cdr curr2) curr2))))))))
         (let* ((result (if (smaller? (first-el best1) (first-el best2))
                          best1
                          best2))
                (curr1 (if (eq? result best1) (cdr best1) best1))
                (curr2 (if (eq? result best2) (cdr best2) best2)))
            (merge-in curr1 curr2 result)
            result))
      (lambda (_best10 _best20)
         (define merge-in (lambda (_curr10 _curr20 _prev0)
               (if (null? _curr10)
                  (set-cdr! _prev0 _curr20)
                  (if (null? _curr20)
                     (set-cdr! _prev0 _curr10)
                     (if (same? (first-el _curr10) (first-el _curr20))
                        (begin
                           (set-cdr! _prev0 _curr10)
                           (merge-in (cdr _curr10) (cdr _curr20) _curr10))
                        (if (smaller? (first-el _curr10) (first-el _curr20))
                           (begin
                              (set-cdr! _prev0 _curr10)
                              (merge-in (cdr _curr10) _curr20 _curr10))
                           (begin
                              (set-cdr! _prev0 _curr20)
                              (merge-in _curr10 (cdr _curr20) _curr20))))))))
         (let* ((_result0 (if (smaller? (first-el _best10) (first-el _best20))
                            _best10
                            _best20))
                (_curr11 (if (eq? _result0 _best10) (cdr _best10) _best10))
                (_curr21 (if (eq? _result0 _best20) (cdr _best20) _best20)))
            (merge-in _curr11 _curr21 _result0)
            _result0))))
 
(define best1 (__toplevel_cons
      (__toplevel_cons
         'ann
         (__toplevel_cons
            (__toplevel_cons
               'meiboomstraat
               (__toplevel_cons 12 (__toplevel_cons 1820 (__toplevel_cons 'Eppegem ()))))
            ()))
      (__toplevel_cons
         (__toplevel_cons
            'bert
            (__toplevel_cons
               (__toplevel_cons
                  'populierendreef
                  (__toplevel_cons 7 (__toplevel_cons 1050 (__toplevel_cons 'Brussel ()))))
               ()))
         (__toplevel_cons
            (__toplevel_cons
               'kurt
               (__toplevel_cons
                  (__toplevel_cons
                     'Mechelsesteenweg
                     (__toplevel_cons 50 (__toplevel_cons 1800 (__toplevel_cons 'Vilvoorde ()))))
                  ()))
            ()))))
 
(define best2 (__toplevel_cons
      (__toplevel_cons
         'bert
         (__toplevel_cons
            (__toplevel_cons
               'populierendreef
               (__toplevel_cons 7 (__toplevel_cons 1050 (__toplevel_cons 'Brussel ()))))
            ()))
      (__toplevel_cons
         (__toplevel_cons
            'jan
            (__toplevel_cons
               (__toplevel_cons 'eikestraat (__toplevel_cons 1 (__toplevel_cons 9000 (__toplevel_cons 'Gent ()))))
               ()))
         (__toplevel_cons
            (__toplevel_cons
               'sofie
               (__toplevel_cons
                  (__toplevel_cons
                     'boerendreef
                     (__toplevel_cons 5 (__toplevel_cons 2800 (__toplevel_cons 'Mechelen ()))))
                  ()))
            ()))))
 
(equal?
   (merge best1 best2)
   (__toplevel_cons
      (__toplevel_cons
         'ann
         (__toplevel_cons
            (__toplevel_cons
               'meiboomstraat
               (__toplevel_cons 12 (__toplevel_cons 1820 (__toplevel_cons 'Eppegem ()))))
            ()))
      (__toplevel_cons
         (__toplevel_cons
            'bert
            (__toplevel_cons
               (__toplevel_cons
                  'populierendreef
                  (__toplevel_cons 7 (__toplevel_cons 1050 (__toplevel_cons 'Brussel ()))))
               ()))
         (__toplevel_cons
            (__toplevel_cons
               'jan
               (__toplevel_cons
                  (__toplevel_cons 'eikestraat (__toplevel_cons 1 (__toplevel_cons 9000 (__toplevel_cons 'Gent ()))))
                  ()))
            (__toplevel_cons
               (__toplevel_cons
                  'kurt
                  (__toplevel_cons
                     (__toplevel_cons
                        'Mechelsesteenweg
                        (__toplevel_cons 50 (__toplevel_cons 1800 (__toplevel_cons 'Vilvoorde ()))))
                     ()))
               (__toplevel_cons
                  (__toplevel_cons
                     'sofie
                     (__toplevel_cons
                        (__toplevel_cons
                           'boerendreef
                           (__toplevel_cons 5 (__toplevel_cons 2800 (__toplevel_cons 'Mechelen ()))))
                        ()))
                  ()))))))
 
