;; renamed lambdas/lets: 25
 
(define map2 (<change>
      (lambda (f l1 l2)
         (if (let ((__or_res (null? l1))) (if __or_res __or_res (null? l2)))
            ()
            (if (if (pair? l1) (pair? l2) #f)
               (cons (f (car l1) (car l2)) (map2 f (cdr l1) (cdr l2)))
               (error "Cannot map2 over a non-list"))))
      (lambda (_f0 _l10 _l20)
         (if (let ((___or_res0 (null? _l10))) (if ___or_res0 ___or_res0 (null? _l20)))
            ()
            (if (if (pair? _l10) (pair? _l20) #f)
               (cons (_f0 (car _l10) (car _l20)) (map2 _f0 (cdr _l10) (cdr _l20)))
               (error "Cannot map2 over a non-list"))))))
 
(define chez-box (lambda (x)
      (cons x ())))
 
(define chez-unbox (lambda (x)
      (car x)))
 
(define chez-set-box! (<change>
      (lambda (x y)
         (set-car! x y))
      (lambda (_x0 _y0)
         (set-car! _x0 _y0))))
 
(define maximal? (lambda (mat)
      ((letrec ((pick-first-row (lambda (first-row-perm)
                                 (if first-row-perm
                                    (if (zunda first-row-perm mat)
                                       (pick-first-row (first-row-perm 'brother))
                                       #f)
                                    #t))))
         pick-first-row)
         (gen-perms mat))))
 
(define zunda (<change>
      (lambda (first-row-perm mat)
         (let* ((first-row (first-row-perm 'now))
                (number-of-cols (length first-row))
                (make-row->func (lambda (if-equal if-different)
                                  (lambda (row)
                                     (let ((vec (make-vector number-of-cols 0)))
                                        (letrec ((__do_loop (lambda (i first row)
                                                              (if (= i number-of-cols)
                                                                 #f
                                                                 (begin
                                                                    (vector-set! vec i (if (= (car first) (car row)) if-equal if-different))
                                                                    (__do_loop (+ i 1) (cdr first) (cdr row)))))))
                                           (__do_loop 0 first-row row))
                                        (lambda (i)
                                           (vector-ref vec i))))))
                (mat (cdr mat)))
            (zebra (first-row-perm 'child) (make-row->func 1 -1) (make-row->func -1 1) mat number-of-cols)))
      (lambda (_first-row-perm0 _mat0)
         (let* ((_first-row0 (_first-row-perm0 'now))
                (_number-of-cols0 (length _first-row0))
                (_make-row->func0 (lambda (_if-equal0 _if-different0)
                                    (lambda (_row0)
                                       (let ((_vec0 (make-vector _number-of-cols0 0)))
                                          (letrec ((___do_loop0 (lambda (_i0 _first0 _row1)
                                                                  (if (= _i0 _number-of-cols0)
                                                                     #f
                                                                     (begin
                                                                        (vector-set! _vec0 _i0 (if (= (car _first0) (car _row1)) _if-equal0 _if-different0))
                                                                        (___do_loop0 (+ _i0 1) (cdr _first0) (cdr _row1)))))))
                                             (___do_loop0 0 _first-row0 _row0))
                                          (lambda (_i1)
                                             (vector-ref _vec0 _i1))))))
                (_mat1 (cdr _mat0)))
            (zebra
               (_first-row-perm0 'child)
               (_make-row->func0 1 -1)
               (_make-row->func0 -1 1)
               _mat1
               _number-of-cols0)))))
 
(define zebra (lambda (row-perm row->func+ row->func- mat number-of-cols)
      ((letrec ((_-*- (lambda (row-perm mat partitions)
                       (let ((__or_res (not row-perm)))
                          (if __or_res
                             __or_res
                             (if (zulu (car mat) (row->func+ (row-perm 'now)) partitions (lambda (new-partitions) (_-*- (row-perm 'child) (cdr mat) new-partitions)))
                                (if (zulu (car mat) (row->func- (row-perm 'now)) partitions (lambda (new-partitions) (_-*- (row-perm 'child) (cdr mat) new-partitions)))
                                   (let ((new-row-perm (row-perm 'brother)))
                                      (let ((__or_res (not new-row-perm)))
                                         (if __or_res
                                            __or_res
                                            (_-*- new-row-perm mat partitions))))
                                   #f)
                                #f))))))
         _-*-)
         row-perm
         mat
         (list (miota number-of-cols)))))
 
(define zulu (let ((cons-if-not-null (<change>
                             (lambda (lhs rhs)
                                (if (null? lhs) rhs (cons lhs rhs)))
                             (lambda (_lhs0 _rhs0)
                                (if (null? _lhs0) _rhs0 (cons _lhs0 _rhs0))))))
      (lambda (old-row new-row-func partitions equal-cont)
         ((letrec ((_-*- (lambda (p-in old-row rev-p-out)
                          ((letrec ((_-split- (lambda (partition old-row plus minus)
                                               (if (null? partition)
                                                  ((letrec ((_-minus- (lambda (old-row m)
                                                                       (if (null? m)
                                                                          (let ((rev-p-out (cons-if-not-null minus (cons-if-not-null plus rev-p-out)))
                                                                                (p-in (cdr p-in)))
                                                                             (if (null? p-in)
                                                                                (equal-cont (reverse rev-p-out))
                                                                                (_-*- p-in old-row rev-p-out)))
                                                                          (let ((__or_res (= 1 (car old-row))))
                                                                             (if __or_res
                                                                                __or_res
                                                                                (_-minus- (cdr old-row) (cdr m))))))))
                                                     _-minus-)
                                                     old-row
                                                     minus)
                                                  (let ((next (car partition)))
                                                     (let ((__case-atom-key (new-row-func next)))
                                                        (if (eq? __case-atom-key 1)
                                                           (if (= 1 (car old-row))
                                                              (_-split- (cdr partition) (cdr old-row) (cons next plus) minus)
                                                              #f)
                                                           (if (eq? __case-atom-key -1)
                                                              (_-split- (cdr partition) old-row plus (cons next minus))
                                                              #f))))))))
                             _-split-)
                             (car p-in)
                             old-row
                             ()
                             ()))))
            _-*-)
            partitions
            old-row
            ()))))
 
(define all? (<change>
      (lambda (ok? lst)
         ((letrec ((_-*- (lambda (lst)
                          (let ((__or_res (null? lst)))
                             (if __or_res
                                __or_res
                                (if (ok? (car lst)) (_-*- (cdr lst)) #f))))))
            _-*-)
            lst))
      (lambda (_ok?0 _lst0)
         ((letrec ((__-*-0 (lambda (_lst1)
                            (let ((___or_res0 (null? _lst1)))
                               (if ___or_res0
                                  ___or_res0
                                  (if (_ok?0 (car _lst1)) (__-*-0 (cdr _lst1)) #f))))))
            __-*-0)
            _lst0))))
 
(define gen-perms (lambda (objects)
      ((letrec ((_-*- (lambda (zulu-future past)
                       (if (null? zulu-future)
                          #f
                          (lambda (msg)
                             (if (eq? msg 'now)
                                (car zulu-future)
                                (if (eq? msg 'brother)
                                   (_-*- (cdr zulu-future) (cons (car zulu-future) past))
                                   (if (eq? msg 'child)
                                      (gen-perms (fold past cons (cdr zulu-future)))
                                      (if (eq? msg 'puke)
                                         (cons (car zulu-future) (fold past cons (cdr zulu-future)))
                                         (error gen-perms "Bad msg: ~a" msg))))))))))
         _-*-)
         objects
         ())))
 
(define fold (lambda (lst folder state)
      ((letrec ((_-*- (lambda (lst state)
                       (if (null? lst)
                          state
                          (_-*- (cdr lst) (folder (car lst) state))))))
         _-*-)
         lst
         state)))
 
(define miota (<change>
      (lambda (len)
         ((letrec ((_-*- (lambda (i) (if (= i len) () (cons i (_-*- (+ i 1))))))) _-*-) 0))
      (lambda (_len0)
         ((letrec ((__-*-0 (lambda (_i0) (if (= _i0 _len0) () (cons _i0 (__-*-0 (+ _i0 1))))))) __-*-0) 0))))
 
(define proc->vector (<change>
      (lambda (size proc)
         (let ((res (make-vector size 0)))
            (letrec ((__do_loop (lambda (i)
                                  (if (= i size)
                                     #f
                                     (begin
                                        (vector-set! res i (proc i))
                                        (__do_loop (+ i 1)))))))
               (__do_loop 0))
            res))
      (lambda (_size0 _proc0)
         (let ((_res0 (make-vector _size0 0)))
            (letrec ((___do_loop0 (lambda (_i0)
                                    (if (= _i0 _size0)
                                       #f
                                       (begin
                                          (vector-set! _res0 _i0 (_proc0 _i0))
                                          (___do_loop0 (+ _i0 1)))))))
               (___do_loop0 0))
            _res0))))
 
(define make-modular (lambda (modulus)
      (<change>
         (let* ((reduce (lambda (x)
                          (modulo x modulus)))
                (coef-zero? (lambda (x)
                              (zero? (reduce x))))
                (coef-+ (lambda (x y)
                          (reduce (+ x y))))
                (coef-negate (lambda (x)
                               (reduce (- x))))
                (coef-* (lambda (x y)
                          (reduce (* x y))))
                (coef-recip (let ((inverses (proc->vector
                                             (- modulus 1)
                                             (lambda (i)
                                                (extended-gcd (+ i 1) modulus (lambda (gcd inverse ignore) inverse))))))
                              (lambda (x)
                                 (let ((x (reduce x)))
                                    (vector-ref inverses (- x 1)))))))
            (lambda (maker)
               (maker 0 1 coef-zero? coef-+ coef-negate coef-* coef-recip)))
         (let* ((_reduce0 (lambda (_x0)
                            (modulo _x0 modulus)))
                (_coef-zero?0 (lambda (_x1)
                                (zero? (_reduce0 _x1))))
                (_coef-+0 (lambda (_x2 _y0)
                            (_reduce0 (+ _x2 _y0))))
                (_coef-negate0 (lambda (_x3)
                                 (_reduce0 (- _x3))))
                (_coef-*0 (lambda (_x4 _y1)
                            (_reduce0 (* _x4 _y1))))
                (_coef-recip0 (let ((_inverses0 (proc->vector
                                                 (- modulus 1)
                                                 (lambda (_i0)
                                                    (extended-gcd (+ _i0 1) modulus (lambda (_gcd0 _inverse0 _ignore0) _inverse0))))))
                                (lambda (_x5)
                                   (let ((_x6 (_reduce0 _x5)))
                                      (vector-ref _inverses0 (- _x6 1)))))))
            (lambda (_maker0)
               (_maker0 0 1 _coef-zero?0 _coef-+0 _coef-negate0 _coef-*0 _coef-recip0))))))
 
(define extended-gcd (<change>
      (let ((n->sgn/abs (lambda (x cont)
                          (if (>= x 0) (cont 1 x) (cons -1 (- x))))))
         (lambda (a b cont)
            (n->sgn/abs
               a
               (lambda (p-a p)
                  (n->sgn/abs
                     b
                     (lambda (q-b q)
                        ((letrec ((_-*- (lambda (p p-a p-b q q-a q-b)
                                         (if (zero? q)
                                            (cont p p-a p-b)
                                            (let ((mult (quotient p q)))
                                               (_-*- q q-a q-b (- p (* mult q)) (- p-a (* mult q-a)) (- p-b (* mult q-b))))))))
                           _-*-)
                           p
                           p-a
                           0
                           q
                           0
                           q-b)))))))
      (let ((_n->sgn/abs0 (lambda (_x0 _cont0)
                            (if (>= _x0 0) (_cont0 1 _x0) (cons -1 (- _x0))))))
         (lambda (_a0 _b0 _cont1)
            (_n->sgn/abs0
               _a0
               (lambda (_p-a0 _p0)
                  (_n->sgn/abs0
                     _b0
                     (lambda (_q-b0 _q0)
                        ((letrec ((__-*-0 (lambda (_p1 _p-a1 _p-b0 _q1 _q-a0 _q-b1)
                                           (if (zero? _q1)
                                              (_cont1 _p1 _p-a1 _p-b0)
                                              (let ((_mult0 (quotient _p1 _q1)))
                                                 (__-*-0
                                                    _q1
                                                    _q-a0
                                                    _q-b1
                                                    (- _p1 (* _mult0 _q1))
                                                    (- _p-a1 (* _mult0 _q-a0))
                                                    (- _p-b0 (* _mult0 _q-b1))))))))
                           __-*-0)
                           _p0
                           _p-a0
                           0
                           _q0
                           0
                           _q-b0)))))))))
 
(define make-row-reduce (<change>
      (lambda (coef-zero coef-one coef-zero? coef-+ coef-negate coef-* coef-recip)
         (lambda (mat)
            ((letrec ((_-*- (lambda (mat)
                             (if (let ((__or_res (null? mat))) (if __or_res __or_res (null? (car mat))))
                                ()
                                ((letrec ((_-**- (lambda (in out)
                                                  (if (null? in)
                                                     (map (lambda (x) (cons coef-zero x)) (_-*- out))
                                                     (let* ((prow (car in))
                                                            (pivot (car prow))
                                                            (prest (cdr prow))
                                                            (in (cdr in)))
                                                        (if (coef-zero? pivot)
                                                           (_-**- in (cons prest out))
                                                           (let ((zap-row (map (let ((mult (coef-recip pivot))) (lambda (x) (coef-* mult x))) prest)))
                                                              (cons
                                                                 (cons coef-one zap-row)
                                                                 (map
                                                                    (lambda (x)
                                                                       (cons coef-zero x))
                                                                    (_-*-
                                                                       (fold
                                                                          in
                                                                          (lambda (row mat)
                                                                             (cons
                                                                                (let ((first-col (car row))
                                                                                      (rest-row (cdr row)))
                                                                                   (if (coef-zero? first-col)
                                                                                      rest-row
                                                                                      (map2
                                                                                         (let ((mult (coef-negate first-col)))
                                                                                            (lambda (f z)
                                                                                               (coef-+ f (coef-* mult z))))
                                                                                         rest-row
                                                                                         zap-row)))
                                                                                mat))
                                                                          out)))))))))))
                                   _-**-)
                                   mat
                                   ())))))
               _-*-)
               mat)))
      (lambda (_coef-zero0 _coef-one0 _coef-zero?0 _coef-+0 _coef-negate0 _coef-*0 _coef-recip0)
         (lambda (_mat0)
            ((letrec ((__-*-0 (lambda (_mat1)
                               (if (let ((___or_res0 (null? _mat1))) (if ___or_res0 ___or_res0 (null? (car _mat1))))
                                  ()
                                  ((letrec ((__-**-0 (lambda (_in0 _out0)
                                                      (if (null? _in0)
                                                         (map (lambda (_x0) (cons _coef-zero0 _x0)) (__-*-0 _out0))
                                                         (let* ((_prow0 (car _in0))
                                                                (_pivot0 (car _prow0))
                                                                (_prest0 (cdr _prow0))
                                                                (_in1 (cdr _in0)))
                                                            (if (_coef-zero?0 _pivot0)
                                                               (__-**-0 _in1 (cons _prest0 _out0))
                                                               (let ((_zap-row0 (map (let ((_mult0 (_coef-recip0 _pivot0))) (lambda (_x1) (_coef-*0 _mult0 _x1))) _prest0)))
                                                                  (cons
                                                                     (cons _coef-one0 _zap-row0)
                                                                     (map
                                                                        (lambda (_x2)
                                                                           (cons _coef-zero0 _x2))
                                                                        (__-*-0
                                                                           (fold
                                                                              _in1
                                                                              (lambda (_row0 _mat2)
                                                                                 (cons
                                                                                    (let ((_first-col0 (car _row0))
                                                                                          (_rest-row0 (cdr _row0)))
                                                                                       (if (_coef-zero?0 _first-col0)
                                                                                          _rest-row0
                                                                                          (map2
                                                                                             (let ((_mult1 (_coef-negate0 _first-col0)))
                                                                                                (lambda (_f0 _z0)
                                                                                                   (_coef-+0 _f0 (_coef-*0 _mult1 _z0))))
                                                                                             _rest-row0
                                                                                             _zap-row0)))
                                                                                    _mat2))
                                                                              _out0)))))))))))
                                     __-**-0)
                                     _mat1
                                     ())))))
               __-*-0)
               _mat0)))))
 
(define make-in-row-space? (lambda (coef-zero coef-one coef-zero? coef-+ coef-negate coef-* coef-recip)
      (<change>
         (let ((row-reduce (make-row-reduce coef-zero coef-one coef-zero? coef-+ coef-negate coef-* coef-recip)))
            (lambda (mat)
               (let ((mat (row-reduce mat)))
                  (lambda (row)
                     ((letrec ((_-*- (lambda (row mat)
                                      (if (null? row)
                                         #t
                                         (let ((r-first (car row))
                                               (r-rest (cdr row)))
                                            (if (coef-zero? r-first)
                                               (_-*-
                                                  r-rest
                                                  (map
                                                     cdr
                                                     (if (let ((__or_res (null? mat))) (if __or_res __or_res (coef-zero? (caar mat))))
                                                        mat
                                                        (cdr mat))))
                                               (if (null? mat)
                                                  #f
                                                  (let* ((zap-row (car mat))
                                                         (z-first (car zap-row))
                                                         (z-rest (cdr zap-row))
                                                         (mat (cdr mat)))
                                                     (if (coef-zero? z-first)
                                                        #f
                                                        (_-*-
                                                           (map2 (let ((mult (coef-negate r-first))) (lambda (r z) (coef-+ r (coef-* mult z)))) r-rest z-rest)
                                                           (map cdr mat)))))))))))
                        _-*-)
                        row
                        mat)))))
         (let ((_row-reduce0 (make-row-reduce coef-zero coef-one coef-zero? coef-+ coef-negate coef-* coef-recip)))
            (lambda (_mat0)
               (let ((_mat1 (_row-reduce0 _mat0)))
                  (lambda (_row0)
                     ((letrec ((__-*-0 (lambda (_row1 _mat2)
                                        (if (null? _row1)
                                           #t
                                           (let ((_r-first0 (car _row1))
                                                 (_r-rest0 (cdr _row1)))
                                              (if (coef-zero? _r-first0)
                                                 (__-*-0
                                                    _r-rest0
                                                    (map
                                                       cdr
                                                       (if (let ((___or_res0 (null? _mat2))) (if ___or_res0 ___or_res0 (coef-zero? (caar _mat2))))
                                                          _mat2
                                                          (cdr _mat2))))
                                                 (if (null? _mat2)
                                                    #f
                                                    (let* ((_zap-row0 (car _mat2))
                                                           (_z-first0 (car _zap-row0))
                                                           (_z-rest0 (cdr _zap-row0))
                                                           (_mat3 (cdr _mat2)))
                                                       (if (coef-zero? _z-first0)
                                                          #f
                                                          (__-*-0
                                                             (map2
                                                                (let ((_mult0 (coef-negate _r-first0)))
                                                                   (lambda (_r0 _z0)
                                                                      (coef-+ _r0 (coef-* _mult0 _z0))))
                                                                _r-rest0
                                                                _z-rest0)
                                                             (map cdr _mat3)))))))))))
                        __-*-0)
                        _row0
                        _mat1))))))))
 
(define make-modular-row-reduce (lambda (modulus)
      ((make-modular modulus) make-row-reduce)))
 
(define make-modular-in-row-space? (<change>
      (lambda (modulus)
         ((make-modular modulus) make-in-row-space?))
      (lambda (_modulus0)
         ((make-modular _modulus0) make-in-row-space?))))
 
(define find-prime (lambda (bound)
      (let* ((primes (list 2))
             (last (chez-box primes))
             (is-next-prime? (<change>
                               (lambda (trial)
                                  ((letrec ((_-*- (lambda (primes)
                                                   (let ((__or_res (null? primes)))
                                                      (if __or_res
                                                         __or_res
                                                         (let ((p (car primes)))
                                                            (let ((__or_res (< trial (* p p))))
                                                               (if __or_res
                                                                  __or_res
                                                                  (if (not (zero? (modulo trial p)))
                                                                     (_-*- (cdr primes))
                                                                     #f)))))))))
                                     _-*-)
                                     primes))
                               (lambda (_trial0)
                                  ((letrec ((__-*-0 (lambda (_primes0)
                                                     (let ((___or_res0 (null? _primes0)))
                                                        (if ___or_res0
                                                           ___or_res0
                                                           (let ((_p0 (car _primes0)))
                                                              (let ((___or_res1 (< _trial0 (* _p0 _p0))))
                                                                 (if ___or_res1
                                                                    ___or_res1
                                                                    (if (not (zero? (modulo _trial0 _p0)))
                                                                       (__-*-0 (cdr _primes0))
                                                                       #f)))))))))
                                     __-*-0)
                                     primes)))))
         (if (> 2 bound)
            2
            ((letrec ((_-*- (lambda (trial)
                             (if (is-next-prime? trial)
                                (let ((entry (list trial)))
                                   (set-cdr! (chez-unbox last) entry)
                                   (chez-set-box! last entry)
                                   (if (> trial bound) trial (_-*- (+ trial 2))))
                                (_-*- (+ trial 2))))))
               _-*-)
               3)))))
 
(define det-upper-bound (lambda (size)
      (let ((main-part (expt size (quotient size 2))))
         (if (even? size)
            main-part
            (*
               main-part
               (letrec ((__do_loop (lambda (i)
                                     (if (>= (* i i) size) i (__do_loop (+ i 1))))))
                  (__do_loop 0)))))))
 
(define go (lambda (number-of-cols inv-size folder state)
      (let* ((in-row-space? (make-modular-in-row-space? (find-prime (det-upper-bound inv-size))))
             (make-tester (<change>
                            (lambda (mat)
                               (let ((tests (let ((old-mat (cdr mat))
                                                 (new-row (car mat)))
                                              (fold-over-subs-of-size
                                                 old-mat
                                                 (- inv-size 2)
                                                 (lambda (sub tests)
                                                    (cons (in-row-space? (cons new-row sub)) tests))
                                                 ()))))
                                  (lambda (row)
                                     ((letrec ((_-*- (lambda (tests)
                                                      (if (not (null? tests))
                                                         (let ((__or_res ((car tests) row)))
                                                            (if __or_res __or_res (_-*- (cdr tests))))
                                                         #f))))
                                        _-*-)
                                        tests))))
                            (lambda (_mat0)
                               (let ((_tests0 (let ((_old-mat0 (cdr _mat0))
                                                   (_new-row0 (car _mat0)))
                                                (fold-over-subs-of-size
                                                   _old-mat0
                                                   (- inv-size 2)
                                                   (lambda (_sub0 _tests1)
                                                      (cons (in-row-space? (cons _new-row0 _sub0)) _tests1))
                                                   ()))))
                                  (lambda (_row0)
                                     ((letrec ((__-*-0 (lambda (_tests2)
                                                        (if (not (null? _tests2))
                                                           (let ((___or_res0 ((car _tests2) _row0)))
                                                              (if ___or_res0 ___or_res0 (__-*-0 (cdr _tests2))))
                                                           #f))))
                                        __-*-0)
                                        _tests0))))))
             (all-rows (fold
                         (fold-over-rows (- number-of-cols 1) cons ())
                         (lambda (row rows)
                            (cons (cons 1 row) rows))
                         ())))
         ((letrec ((_-*- (lambda (number-of-rows rev-mat possible-future state)
                          (let ((zulu-future (remove-in-order
                                               (if (< number-of-rows inv-size)
                                                  (in-row-space? rev-mat)
                                                  (make-tester rev-mat))
                                               possible-future)))
                             (if (null? zulu-future)
                                (folder (reverse rev-mat) state)
                                ((letrec ((_-**- (lambda (zulu-future state)
                                                  (if (null? zulu-future)
                                                     state
                                                     (let ((rest-of-future (cdr zulu-future)))
                                                        (_-**-
                                                           rest-of-future
                                                           (let* ((first (car zulu-future))
                                                                  (new-rev-mat (cons first rev-mat)))
                                                              (if (maximal? (reverse new-rev-mat))
                                                                 (_-*- (+ number-of-rows 1) new-rev-mat rest-of-future state)
                                                                 state))))))))
                                   _-**-)
                                   zulu-future
                                   state))))))
            _-*-)
            1
            (list (car all-rows))
            (cdr all-rows)
            state))))
 
(define go-folder (lambda (mat bsize.blen.blist)
      (<change>
         (let ((bsize (car bsize.blen.blist))
               (size (length mat)))
            (if (< size bsize)
               bsize.blen.blist
               (let ((blen (cadr bsize.blen.blist))
                     (blist (cddr bsize.blen.blist)))
                  (if (= size bsize)
                     (let ((blen (+ blen 1)))
                        (cons
                           bsize
                           (cons blen (if (< blen 3000) (cons mat blist) (if (= blen 3000) (cons "..." blist) blist)))))
                     (list size 1 mat)))))
         (let ((_bsize0 (car bsize.blen.blist))
               (_size0 (length mat)))
            (if (< _size0 _bsize0)
               bsize.blen.blist
               (let ((_blen0 (cadr bsize.blen.blist))
                     (_blist0 (cddr bsize.blen.blist)))
                  (if (= _size0 _bsize0)
                     (let ((_blen1 (+ _blen0 1)))
                        (cons
                           _bsize0
                           (cons
                              _blen1
                              (if (< _blen1 3000)
                                 (cons mat _blist0)
                                 (if (= _blen1 3000) (cons "..." _blist0) _blist0)))))
                     (list _size0 1 mat))))))))
 
(define really-go (<change>
      (lambda (number-of-cols inv-size)
         (cddr (go number-of-cols inv-size go-folder (list -1 -1))))
      (lambda (_number-of-cols0 _inv-size0)
         (cddr (go _number-of-cols0 _inv-size0 go-folder (list -1 -1))))))
 
(define remove-in-order (lambda (remove? lst)
      (reverse (fold lst (lambda (e lst) (if (remove? e) lst (cons e lst))) ()))))
 
(define fold-over-rows (<change>
      (lambda (number-of-cols folder state)
         (if (zero? number-of-cols)
            (folder () state)
            (fold-over-rows
               (- number-of-cols 1)
               (lambda (tail state)
                  (folder (cons -1 tail) state))
               (fold-over-rows (- number-of-cols 1) (lambda (tail state) (folder (cons 1 tail) state)) state))))
      (lambda (_number-of-cols0 _folder0 _state0)
         (if (zero? _number-of-cols0)
            (_folder0 () _state0)
            (fold-over-rows
               (- _number-of-cols0 1)
               (lambda (_tail0 _state1)
                  (_folder0 (cons -1 _tail0) _state1))
               (fold-over-rows
                  (- _number-of-cols0 1)
                  (lambda (_tail1 _state2)
                     (_folder0 (cons 1 _tail1) _state2))
                  _state0))))))
 
(define fold-over-subs-of-size (lambda (universe size folder state)
      (let ((usize (length universe)))
         (if (< usize size)
            state
            ((letrec ((_-*- (lambda (size universe folder csize state)
                             (if (zero? csize)
                                (folder universe state)
                                (if (zero? size)
                                   (folder () state)
                                   (let ((first-u (car universe))
                                         (rest-u (cdr universe)))
                                      (_-*-
                                         size
                                         rest-u
                                         folder
                                         (- csize 1)
                                         (_-*- (- size 1) rest-u (lambda (tail state) (folder (cons first-u tail) state)) csize state))))))))
               _-*-)
               size
               universe
               folder
               (- usize size)
               state)))))
 
(equal?
   (really-go 5 5)
   (__toplevel_cons
      (__toplevel_cons
         (__toplevel_cons
            1
            (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 ())))))
         (__toplevel_cons
            (__toplevel_cons
               1
               (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons -1 ())))))
            (__toplevel_cons
               (__toplevel_cons
                  1
                  (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons 1 ())))))
               (__toplevel_cons
                  (__toplevel_cons
                     1
                     (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons -1 (__toplevel_cons -1 ())))))
                  (__toplevel_cons
                     (__toplevel_cons
                        1
                        (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons -1 ())))))
                     (__toplevel_cons
                        (__toplevel_cons
                           1
                           (__toplevel_cons -1 (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons 1 ())))))
                        ()))))))
      (__toplevel_cons
         (__toplevel_cons
            (__toplevel_cons
               1
               (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 ())))))
            (__toplevel_cons
               (__toplevel_cons
                  1
                  (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons -1 ())))))
               (__toplevel_cons
                  (__toplevel_cons
                     1
                     (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons 1 ())))))
                  (__toplevel_cons
                     (__toplevel_cons
                        1
                        (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons -1 ())))))
                     (__toplevel_cons
                        (__toplevel_cons
                           1
                           (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons -1 ())))))
                        (__toplevel_cons
                           (__toplevel_cons
                              1
                              (__toplevel_cons -1 (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons 1 ())))))
                           ()))))))
         (__toplevel_cons
            (__toplevel_cons
               (__toplevel_cons
                  1
                  (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 ())))))
               (__toplevel_cons
                  (__toplevel_cons
                     1
                     (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons -1 ())))))
                  (__toplevel_cons
                     (__toplevel_cons
                        1
                        (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons 1 ())))))
                     (__toplevel_cons
                        (__toplevel_cons
                           1
                           (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons -1 ())))))
                        (__toplevel_cons
                           (__toplevel_cons
                              1
                              (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons 1 ())))))
                           (__toplevel_cons
                              (__toplevel_cons
                                 1
                                 (__toplevel_cons -1 (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons 1 ())))))
                              ()))))))
            (__toplevel_cons
               (__toplevel_cons
                  (__toplevel_cons
                     1
                     (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 ())))))
                  (__toplevel_cons
                     (__toplevel_cons
                        1
                        (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons -1 ())))))
                     (__toplevel_cons
                        (__toplevel_cons
                           1
                           (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons 1 ())))))
                        (__toplevel_cons
                           (__toplevel_cons
                              1
                              (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons 1 ())))))
                           (__toplevel_cons
                              (__toplevel_cons
                                 1
                                 (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons -1 ())))))
                              (__toplevel_cons
                                 (__toplevel_cons
                                    1
                                    (__toplevel_cons -1 (__toplevel_cons -1 (__toplevel_cons -1 (__toplevel_cons 1 ())))))
                                 ()))))))
               (__toplevel_cons
                  (__toplevel_cons
                     (__toplevel_cons
                        1
                        (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 ())))))
                     (__toplevel_cons
                        (__toplevel_cons
                           1
                           (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons -1 ())))))
                        (__toplevel_cons
                           (__toplevel_cons
                              1
                              (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons 1 ())))))
                           (__toplevel_cons
                              (__toplevel_cons
                                 1
                                 (__toplevel_cons 1 (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons 1 ())))))
                              (__toplevel_cons
                                 (__toplevel_cons
                                    1
                                    (__toplevel_cons -1 (__toplevel_cons 1 (__toplevel_cons 1 (__toplevel_cons 1 ())))))
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       1
                                       (__toplevel_cons -1 (__toplevel_cons -1 (__toplevel_cons -1 (__toplevel_cons -1 ())))))
                                    ()))))))
                  ()))))))
 
