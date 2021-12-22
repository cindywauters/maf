;; renamed lambdas/lets: 4
;; Of which consistent renamings: 3

(define gen (<change>
      (lambda (n)
         (let* ((n/2 (quotient n 2))
                (radicals (make-vector (+ n/2 1) (__toplevel_cons 'H ()))))
            (define rads-of-size (lambda (n)
                  ((letrec ((loop1 (lambda (ps lst)
                                    (if (null? ps)
                                       lst
                                       (let* ((p (car ps))
                                              (nc1 (vector-ref p 0))
                                              (nc2 (vector-ref p 1))
                                              (nc3 (vector-ref p 2)))
                                          ((letrec ((loop2 (lambda (rads1 lst)
                                                            (if (null? rads1)
                                                               lst
                                                               ((letrec ((loop3 (lambda (rads2 lst)
                                                                                 (if (null? rads2)
                                                                                    lst
                                                                                    ((letrec ((loop4 (lambda (rads3 lst)
                                                                                                      (if (null? rads3)
                                                                                                         lst
                                                                                                         (cons (vector 'C (car rads1) (car rads2) (car rads3)) (loop4 (cdr rads3) lst))))))
                                                                                       loop4)
                                                                                       (if (= nc2 nc3) rads2 (vector-ref radicals nc3))
                                                                                       (loop3 (cdr rads2) lst))))))
                                                                  loop3)
                                                                  (if (= nc1 nc2) rads1 (vector-ref radicals nc2))
                                                                  (loop2 (cdr rads1) lst))))))
                                             loop2)
                                             (vector-ref radicals nc1)
                                             (loop1 (cdr ps) lst)))))))
                     loop1)
                     (three-partitions (- n 1))
                     ())))
            (define bcp-generator (lambda (j)
                  (if (odd? j)
                     ()
                     ((letrec ((loop1 (lambda (rads1 lst)
                                       (if (null? rads1)
                                          lst
                                          ((letrec ((loop2 (lambda (rads2 lst)
                                                            (if (null? rads2)
                                                               lst
                                                               (cons (vector 'BCP (car rads1) (car rads2)) (loop2 (cdr rads2) lst))))))
                                             loop2)
                                             rads1
                                             (loop1 (cdr rads1) lst))))))
                        loop1)
                        (vector-ref radicals (quotient j 2))
                        ()))))
            (define ccp-generator (lambda (j)
                  ((letrec ((loop1 (lambda (ps lst)
                                    (if (null? ps)
                                       lst
                                       (let* ((p (car ps))
                                              (nc1 (vector-ref p 0))
                                              (nc2 (vector-ref p 1))
                                              (nc3 (vector-ref p 2))
                                              (nc4 (vector-ref p 3)))
                                          ((letrec ((loop2 (lambda (rads1 lst)
                                                            (if (null? rads1)
                                                               lst
                                                               ((letrec ((loop3 (lambda (rads2 lst)
                                                                                 (if (null? rads2)
                                                                                    lst
                                                                                    ((letrec ((loop4 (lambda (rads3 lst)
                                                                                                      (if (null? rads3)
                                                                                                         lst
                                                                                                         ((letrec ((loop5 (lambda (rads4 lst)
                                                                                                                           (if (null? rads4)
                                                                                                                              lst
                                                                                                                              (cons (vector 'CCP (car rads1) (car rads2) (car rads3) (car rads4)) (loop5 (cdr rads4) lst))))))
                                                                                                            loop5)
                                                                                                            (if (= nc3 nc4) rads3 (vector-ref radicals nc4))
                                                                                                            (loop4 (cdr rads3) lst))))))
                                                                                       loop4)
                                                                                       (if (= nc2 nc3) rads2 (vector-ref radicals nc3))
                                                                                       (loop3 (cdr rads2) lst))))))
                                                                  loop3)
                                                                  (if (= nc1 nc2) rads1 (vector-ref radicals nc2))
                                                                  (loop2 (cdr rads1) lst))))))
                                             loop2)
                                             (vector-ref radicals nc1)
                                             (loop1 (cdr ps) lst)))))))
                     loop1)
                     (four-partitions (- j 1))
                     ())))
            ((letrec ((loop (lambda (i)
                             (if (> i n/2)
                                (vector (bcp-generator n) (ccp-generator n))
                                (begin
                                   (vector-set! radicals i (rads-of-size i))
                                   (loop (+ i 1)))))))
               loop)
               1)))
      (lambda (_n0)
         (let* ((_n/20 (quotient _n0 2))
                (_radicals0 (make-vector (+ _n/20 1) (__toplevel_cons 'H ()))))
            (define rads-of-size (lambda (_n1)
                  ((letrec ((_loop10 (lambda (_ps0 _lst0)
                                      (if (null? _ps0)
                                         _lst0
                                         (let* ((_p0 (car _ps0))
                                                (_nc10 (vector-ref _p0 0))
                                                (_nc20 (vector-ref _p0 1))
                                                (_nc30 (vector-ref _p0 2)))
                                            ((letrec ((_loop20 (lambda (_rads10 _lst1)
                                                                (if (null? _rads10)
                                                                   _lst1
                                                                   ((letrec ((_loop30 (lambda (_rads20 _lst2)
                                                                                       (if (null? _rads20)
                                                                                          _lst2
                                                                                          ((letrec ((_loop40 (lambda (_rads30 _lst3)
                                                                                                              (if (null? _rads30)
                                                                                                                 _lst3
                                                                                                                 (cons (vector 'C (car _rads10) (car _rads20) (car _rads30)) (_loop40 (cdr _rads30) _lst3))))))
                                                                                             _loop40)
                                                                                             (if (= _nc20 _nc30)
                                                                                                _rads20
                                                                                                (vector-ref _radicals0 _nc30))
                                                                                             (_loop30 (cdr _rads20) _lst2))))))
                                                                      _loop30)
                                                                      (if (= _nc10 _nc20)
                                                                         _rads10
                                                                         (vector-ref _radicals0 _nc20))
                                                                      (_loop20 (cdr _rads10) _lst1))))))
                                               _loop20)
                                               (vector-ref _radicals0 _nc10)
                                               (_loop10 (cdr _ps0) _lst0)))))))
                     _loop10)
                     (three-partitions (- _n1 1))
                     ())))
            (define bcp-generator (lambda (_j0)
                  (if (odd? _j0)
                     ()
                     ((letrec ((_loop11 (lambda (_rads11 _lst4)
                                         (if (null? _rads11)
                                            _lst4
                                            ((letrec ((_loop21 (lambda (_rads21 _lst5)
                                                                (if (null? _rads21)
                                                                   _lst5
                                                                   (cons (vector 'BCP (car _rads11) (car _rads21)) (_loop21 (cdr _rads21) _lst5))))))
                                               _loop21)
                                               _rads11
                                               (_loop11 (cdr _rads11) _lst4))))))
                        _loop11)
                        (vector-ref _radicals0 (quotient _j0 2))
                        ()))))
            (define ccp-generator (lambda (_j1)
                  ((letrec ((_loop12 (lambda (_ps1 _lst6)
                                      (if (null? _ps1)
                                         _lst6
                                         (let* ((_p1 (car _ps1))
                                                (_nc11 (vector-ref _p1 0))
                                                (_nc21 (vector-ref _p1 1))
                                                (_nc31 (vector-ref _p1 2))
                                                (_nc40 (vector-ref _p1 3)))
                                            ((letrec ((_loop22 (lambda (_rads12 _lst7)
                                                                (if (null? _rads12)
                                                                   _lst7
                                                                   ((letrec ((_loop31 (lambda (_rads22 _lst8)
                                                                                       (if (null? _rads22)
                                                                                          _lst8
                                                                                          ((letrec ((_loop41 (lambda (_rads31 _lst9)
                                                                                                              (if (null? _rads31)
                                                                                                                 _lst9
                                                                                                                 ((letrec ((_loop50 (lambda (_rads40 _lst10)
                                                                                                                                     (if (null? _rads40)
                                                                                                                                        _lst10
                                                                                                                                        (cons
                                                                                                                                           (vector 'CCP (car _rads12) (car _rads22) (car _rads31) (car _rads40))
                                                                                                                                           (_loop50 (cdr _rads40) _lst10))))))
                                                                                                                    _loop50)
                                                                                                                    (if (= _nc31 _nc40)
                                                                                                                       _rads31
                                                                                                                       (vector-ref _radicals0 _nc40))
                                                                                                                    (_loop41 (cdr _rads31) _lst9))))))
                                                                                             _loop41)
                                                                                             (if (= _nc21 _nc31)
                                                                                                _rads22
                                                                                                (vector-ref _radicals0 _nc31))
                                                                                             (_loop31 (cdr _rads22) _lst8))))))
                                                                      _loop31)
                                                                      (if (= _nc11 _nc21)
                                                                         _rads12
                                                                         (vector-ref _radicals0 _nc21))
                                                                      (_loop22 (cdr _rads12) _lst7))))))
                                               _loop22)
                                               (vector-ref _radicals0 _nc11)
                                               (_loop12 (cdr _ps1) _lst6)))))))
                     _loop12)
                     (four-partitions (- _j1 1))
                     ())))
            ((letrec ((_loop0 (lambda (_i0)
                               (if (> _i0 _n/20)
                                  (vector (bcp-generator _n0) (ccp-generator _n0))
                                  (begin
                                     (vector-set! _radicals0 _i0 (rads-of-size _i0))
                                     (_loop0 (+ _i0 1)))))))
               _loop0)
               1)))))
 
(define three-partitions (<change>
      (lambda (m)
         ((letrec ((loop1 (lambda (lst nc1)
                           (if (< nc1 0)
                              lst
                              ((letrec ((loop2 (lambda (lst nc2)
                                                (if (< nc2 nc1)
                                                   (loop1 lst (- nc1 1))
                                                   (loop2 (cons (vector nc1 nc2 (- m (+ nc1 nc2))) lst) (- nc2 1))))))
                                 loop2)
                                 lst
                                 (quotient (- m nc1) 2))))))
            loop1)
            ()
            (quotient m 3)))
      (lambda (_m0)
         ((letrec ((_loop10 (lambda (_lst0 _nc10)
                             (if (< _nc10 0)
                                _lst0
                                ((letrec ((_loop20 (lambda (_lst1 _nc20)
                                                    (if (< _nc20 _nc10)
                                                       (_loop10 _lst1 (- _nc10 1))
                                                       (_loop20 (cons (vector _nc10 _nc20 (- _m0 (+ _nc10 _nc20))) _lst1) (- _nc20 1))))))
                                   _loop20)
                                   _lst0
                                   (quotient (- _m0 _nc10) 2))))))
            _loop10)
            ()
            (quotient _m0 3)))))
 
(define four-partitions (<change>
      (lambda (m)
         ((letrec ((loop1 (lambda (lst nc1)
                           (if (< nc1 0)
                              lst
                              ((letrec ((loop2 (lambda (lst nc2)
                                                (if (< nc2 nc1)
                                                   (loop1 lst (- nc1 1))
                                                   (let ((start (max nc2 (- (quotient (+ m 1) 2) (+ nc1 nc2)))))
                                                      ((letrec ((loop3 (lambda (lst nc3)
                                                                        (if (< nc3 start)
                                                                           (loop2 lst (- nc2 1))
                                                                           (loop3 (cons (vector nc1 nc2 nc3 (- m (+ nc1 (+ nc2 nc3)))) lst) (- nc3 1))))))
                                                         loop3)
                                                         lst
                                                         (quotient (- m (+ nc1 nc2)) 2)))))))
                                 loop2)
                                 lst
                                 (quotient (- m nc1) 3))))))
            loop1)
            ()
            (quotient m 4)))
      (lambda (_m0)
         ((letrec ((_loop10 (lambda (_lst0 _nc10)
                             (if (< _nc10 0)
                                _lst0
                                ((letrec ((_loop20 (lambda (_lst1 _nc20)
                                                    (if (< _nc20 _nc10)
                                                       (_loop10 _lst1 (- _nc10 1))
                                                       (let ((_start0 (max _nc20 (- (quotient (+ _m0 1) 2) (+ _nc10 _nc20)))))
                                                          ((letrec ((_loop30 (lambda (_lst2 _nc30)
                                                                              (if (< _nc30 _start0)
                                                                                 (_loop20 _lst2 (- _nc20 1))
                                                                                 (_loop30 (cons (vector _nc10 _nc20 _nc30 (- _m0 (+ _nc10 (+ _nc20 _nc30)))) _lst2) (- _nc30 1))))))
                                                             _loop30)
                                                             _lst1
                                                             (quotient (- _m0 (+ _nc10 _nc20)) 2)))))))
                                   _loop20)
                                   _lst0
                                   (quotient (- _m0 _nc10) 3))))))
            _loop10)
            ()
            (quotient _m0 4)))))
 
(define nb (<change>
      (lambda (n)
         (let ((x (gen n)))
            (+ (length (vector-ref x 0)) (length (vector-ref x 1)))))
      (lambda (_n0)
         (let ((_x0 (gen _n0)))
            (+ (length (vector-ref _x0 1)) (length (vector-ref _x0 1))))))) ;; not renaming, changed vector-ref
 
(= (nb 17) 24894)
 
