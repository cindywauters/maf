;; renamed lambdas/lets: 18
;; Of which consistent renamings: 15

(define foldr (<change>
      (lambda (f base lst)
         (define foldr-aux (lambda (lst)
               (if (null? lst)
                  base
                  (f (car lst) (foldr-aux (cdr lst))))))
         (foldr-aux lst))
      (lambda (_f0 _base0 _lst0)
         (define foldr-aux (lambda (_lst1)
               (if (not (null? _lst1)) ;; NOT RENAMING, introduced not
                  _base0
                  (_f0 (car _lst1) (foldr-aux (cdr _lst1))))))
         (foldr-aux _lst0))))
 
(define foldl (lambda (f base lst)
      (define foldl-aux (lambda (base lst)
            (if (null? lst)
               base
               (foldl-aux (f base (car lst)) (cdr lst)))))
      (foldl-aux base lst)))
 
(define for (<change>
      (lambda (lo hi f)
         (define for-aux (lambda (lo)
               (if (< lo hi)
                  (cons (f lo) (for-aux (+ lo 1)))
                  ())))
         (for-aux lo))
      (lambda (_lo0 _hi0 _f0)
         (define for-aux (lambda (_lo1)
               (if (> _lo1 _hi0) ;; NOT RENAMING, changed < to >
                  (cons (_f0 _lo1) (for-aux (+ _lo1 1)))
                  ())))
         (for-aux _lo0))))
 
(define concat (<change>
      (lambda (lists)
         (foldr append () lists))
      (lambda (_lists0)
         (foldr append () _lists0))))
 
(define list-read (lambda (lst i)
      (if (= i 0)
         (car lst)
         (list-read (cdr lst) (- i 1)))))
 
(define list-write (<change>
      (lambda (lst i val)
         (if (= i 0)
            (cons val (cdr lst))
            (cons (car lst) (list-write (cdr lst) (- i 1) val))))
      (lambda (_lst0 _i0 _val0)
         (if (= _i0 0)
            (list _val0 (cdr _lst0)) ;; not renaming, changed cons to list
            (cons (car _lst0) (list-write (cdr _lst0) (- _i0 1) _val0))))))
 
(define list-remove-pos (lambda (lst i)
      (if (= i 0)
         (cdr lst)
         (cons (car lst) (list-remove-pos (cdr lst) (- i 1))))))
 
(define duplicates? (<change>
      (lambda (lst)
         (if (null? lst)
            #f
            (let ((__or_res (member (car lst) (cdr lst))))
               (if __or_res __or_res (duplicates? (cdr lst))))))
      (lambda (_lst0)
         (if (null? _lst0)
            #f
            (let ((___or_res0 (member (car _lst0) (cdr _lst0))))
               (if ___or_res0
                  ___or_res0
                  (duplicates? (cdr _lst0))))))))
 
(define make-matrix (<change>
      (lambda (n m init)
         (for 0 n (lambda (i) (for 0 m (lambda (j) (init i j))))))
      (lambda (_n0 _m0 _init0)
         (for 0 _n0 (lambda (_i0) (for 0 _m0 (lambda (_j0) (_init0 _i0 _j0))))))))
 
(define matrix-read (<change>
      (lambda (mat i j)
         (list-read (list-read mat i) j))
      (lambda (_mat0 _i0 _j0)
         (list-read (list-read _mat0 _i0) _j0))))
 
(define matrix-write (<change>
      (lambda (mat i j val)
         (list-write mat i (list-write (list-read mat i) j val)))
      (lambda (_mat0 _i0 _j0 _val0)
         (list-write _mat0 _i0 (list-write (list-read _mat0 _i0) _j0 _val0)))))
 
(define matrix-size (lambda (mat)
      (cons (length mat) (length (car mat)))))
 
(define matrix-map (<change>
      (lambda (f mat)
         (map (lambda (lst) (map f lst)) mat))
      (lambda (_f0 _mat0)
         (map (lambda (_lst0) (map _f0 _lst0)) _mat0))))
 
(define initial-random 0)
 
(define next-random (lambda (current-random)
      (remainder (+ (* current-random 3581) 12751) 131072)))
 
(define shuffle (<change>
      (lambda (lst)
         (shuffle-aux lst initial-random))
      (lambda (_lst0)
         (shuffle-aux _lst0 initial-random))))
 
(define shuffle-aux (<change>
      (lambda (lst current-random)
         (if (null? lst)
            ()
            (let ((new-random (next-random current-random)))
               (let ((i (modulo new-random (length lst))))
                  (cons (list-read lst i) (shuffle-aux (list-remove-pos lst i) new-random))))))
      (lambda (_lst0 _current-random0)
         (if (null? _lst0)
            ()
            (let ((_new-random0 (next-random _current-random0)))
               (let ((_i0 (modulo _new-random0 (length _lst0))))
                  (cons (list-read _lst0 _i0) (shuffle-aux (list-remove-pos _lst0 _i0) _new-random0))))))))
 
(define make-maze (<change>
      (lambda (n m)
         (if (not (if (odd? n) (odd? m) #f))
            'error
            (let ((cave (make-matrix n m (lambda (i j) (if (if (even? i) (even? j) #f) (cons i j) #f))))
                  (possible-holes (concat
                                    (for
                                       0
                                       n
                                       (lambda (i)
                                          (concat (for 0 m (lambda (j) (if (equal? (even? i) (even? j)) () (list (cons i j)))))))))))
               (cave-to-maze (pierce-randomly (shuffle possible-holes) cave)))))
      (lambda (_n0 _m0)
         (if (not (if (odd? _n0) (odd? _m0) #f))
            'error
            (let ((_cave0 (make-matrix _n0 _m0 (lambda (_i0 _j0) (if (if (even? _i0) (even? _j0) #f) (cons _i0 _j0) #f))))
                  (_possible-holes0 (concat
                                      (for
                                         0
                                         _n0
                                         (lambda (_i1)
                                            (concat (for 0 _m0 (lambda (_j1) (if (equal? (even? _i1) (even? _j1)) () (list (cons _i1 _j1)))))))))))
               (cave-to-maze (pierce-randomly (shuffle _possible-holes0) _cave0)))))))
 
(define cave-to-maze (lambda (cave)
      (matrix-map (<change> (lambda (x) (if x '_ '*)) (lambda (_x0) (if _x0 '_ '*))) cave)))
 
(define pierce (<change>
      (lambda (pos cave)
         (let ((i (car pos))
               (j (cdr pos)))
            (matrix-write cave i j pos)))
      (lambda (_pos0 _cave0)
         (let ((_i0 (car _pos0))
               (_j0 (cdr _pos0)))
            (matrix-write _cave0 _i0 _j0 _pos0)))))
 
(define pierce-randomly (<change>
      (lambda (possible-holes cave)
         (if (null? possible-holes)
            cave
            (let ((hole (car possible-holes)))
               (pierce-randomly (cdr possible-holes) (try-to-pierce hole cave)))))
      (lambda (_possible-holes0 _cave0)
         (if (null? _possible-holes0)
            _cave0
            (let ((_hole0 (car _possible-holes0)))
               (pierce-randomly (cdr _possible-holes0) (try-to-pierce _hole0 _cave0)))))))
 
(define try-to-pierce (<change>
      (lambda (pos cave)
         (let ((i (car pos))
               (j (cdr pos)))
            (let ((ncs (neighboring-cavities pos cave)))
               (if (duplicates? (map (lambda (nc) (matrix-read cave (car nc) (cdr nc))) ncs))
                  cave
                  (pierce pos (foldl (lambda (c nc) (change-cavity c nc pos)) cave ncs))))))
      (lambda (_pos0 _cave0)
         (let ((_i0 (car _pos0))
               (_j0 (cdr _pos0)))
            (let ((_ncs0 (neighboring-cavities _pos0 _cave0)))
               (if (duplicates? (map (lambda (_nc0) (matrix-read _cave0 (car _nc0) (cdr _nc0))) _ncs0))
                  _cave0
                  (pierce _pos0 (foldl (lambda (_c0 _nc1) (change-cavity _c0 _nc1 _pos0)) _cave0 _ncs0))))))))
 
(define change-cavity (lambda (cave pos new-cavity-id)
      (let ((i (car pos))
            (j (cdr pos)))
         (change-cavity-aux cave pos new-cavity-id (matrix-read cave i j)))))
 
(define change-cavity-aux (<change>
      (lambda (cave pos new-cavity-id old-cavity-id)
         (let ((i (car pos))
               (j (cdr pos)))
            (let ((cavity-id (matrix-read cave i j)))
               (if (equal? cavity-id old-cavity-id)
                  (foldl
                     (lambda (c nc)
                        (change-cavity-aux c nc new-cavity-id old-cavity-id))
                     (matrix-write cave i j new-cavity-id)
                     (neighboring-cavities pos cave))
                  cave))))
      (lambda (_cave0 _pos0 _new-cavity-id0 _old-cavity-id0)
         (let ((_i0 (car _pos0))
               (_j0 (cdr _pos0)))
            (let ((_cavity-id0 (matrix-read _cave0 _i0 _j0)))
               (if (equal? _cavity-id0 _old-cavity-id0)
                  (foldl
                     (lambda (_c0 _nc0)
                        (change-cavity-aux _c0 _nc0 _new-cavity-id0 _old-cavity-id0))
                     (matrix-write _cave0 _i0 _j0 _new-cavity-id0)
                     (neighboring-cavities _pos0 _cave0))
                  _cave0))))))
 
(define neighboring-cavities (<change>
      (lambda (pos cave)
         (let ((size (matrix-size cave)))
            (let ((n (car size))
                  (m (cdr size)))
               (let ((i (car pos))
                     (j (cdr pos)))
                  (append
                     (if (if (> i 0) (matrix-read cave (- i 1) j) #f)
                        (list (cons (- i 1) j))
                        ())
                     (append
                        (if (if (< i (- n 1)) (matrix-read cave (+ i 1) j) #f)
                           (list (cons (+ i 1) j))
                           ())
                        (append
                           (if (if (> j 0) (matrix-read cave i (- j 1)) #f)
                              (list (cons i (- j 1)))
                              ())
                           (if (if (< j (- m 1)) (matrix-read cave i (+ j 1)) #f)
                              (list (cons i (+ j 1)))
                              ()))))))))
      (lambda (_pos0 _cave0)
         (let ((_size0 (matrix-size _cave0)))
            (let ((_n0 (car _size0))
                  (_m0 (cdr _size0)))
               (let ((_i0 (car _pos0))
                     (_j0 (cdr _pos0)))
                  (append
                     (if (if (> _i0 0) (matrix-read _cave0 (- _i0 1) _j0) #f)
                        (list (cons (- _i0 1) _j0))
                        ())
                     (append
                        (if (if (< _i0 (- _n0 1)) (matrix-read _cave0 (+ _i0 1) _j0) #f)
                           (list (cons (+ _i0 1) _j0))
                           ())
                        (append
                           (if (if (> _j0 0) (matrix-read _cave0 _i0 (- _j0 1)) #f)
                              (list (cons _i0 (- _j0 1)))
                              ())
                           (if (if (< _j0 (- _m0 1)) (matrix-read _cave0 _i0 (+ _j0 1)) #f)
                              (list (cons _i0 (+ _j0 1)))
                              ()))))))))))
 
(define expected-result (__toplevel_cons
      (__toplevel_cons
         '_
         (__toplevel_cons
            '*
            (__toplevel_cons
               '_
               (__toplevel_cons
                  '_
                  (__toplevel_cons
                     '_
                     (__toplevel_cons
                        '_
                        (__toplevel_cons
                           '_
                           (__toplevel_cons '_ (__toplevel_cons '_ (__toplevel_cons '_ (__toplevel_cons '_ ())))))))))))
      (__toplevel_cons
         (__toplevel_cons
            '_
            (__toplevel_cons
               '*
               (__toplevel_cons
                  '*
                  (__toplevel_cons
                     '*
                     (__toplevel_cons
                        '*
                        (__toplevel_cons
                           '*
                           (__toplevel_cons
                              '*
                              (__toplevel_cons '* (__toplevel_cons '_ (__toplevel_cons '* (__toplevel_cons '* ())))))))))))
         (__toplevel_cons
            (__toplevel_cons
               '_
               (__toplevel_cons
                  '_
                  (__toplevel_cons
                     '_
                     (__toplevel_cons
                        '*
                        (__toplevel_cons
                           '_
                           (__toplevel_cons
                              '_
                              (__toplevel_cons
                                 '_
                                 (__toplevel_cons '* (__toplevel_cons '_ (__toplevel_cons '_ (__toplevel_cons '_ ())))))))))))
            (__toplevel_cons
               (__toplevel_cons
                  '_
                  (__toplevel_cons
                     '*
                     (__toplevel_cons
                        '_
                        (__toplevel_cons
                           '*
                           (__toplevel_cons
                              '_
                              (__toplevel_cons
                                 '*
                                 (__toplevel_cons
                                    '_
                                    (__toplevel_cons '* (__toplevel_cons '_ (__toplevel_cons '* (__toplevel_cons '_ ())))))))))))
               (__toplevel_cons
                  (__toplevel_cons
                     '_
                     (__toplevel_cons
                        '*
                        (__toplevel_cons
                           '_
                           (__toplevel_cons
                              '_
                              (__toplevel_cons
                                 '_
                                 (__toplevel_cons
                                    '*
                                    (__toplevel_cons
                                       '_
                                       (__toplevel_cons '* (__toplevel_cons '_ (__toplevel_cons '* (__toplevel_cons '_ ())))))))))))
                  (__toplevel_cons
                     (__toplevel_cons
                        '*
                        (__toplevel_cons
                           '*
                           (__toplevel_cons
                              '_
                              (__toplevel_cons
                                 '*
                                 (__toplevel_cons
                                    '*
                                    (__toplevel_cons
                                       '*
                                       (__toplevel_cons
                                          '*
                                          (__toplevel_cons '* (__toplevel_cons '_ (__toplevel_cons '* (__toplevel_cons '_ ())))))))))))
                     (__toplevel_cons
                        (__toplevel_cons
                           '_
                           (__toplevel_cons
                              '*
                              (__toplevel_cons
                                 '_
                                 (__toplevel_cons
                                    '_
                                    (__toplevel_cons
                                       '_
                                       (__toplevel_cons
                                          '_
                                          (__toplevel_cons
                                             '_
                                             (__toplevel_cons '_ (__toplevel_cons '_ (__toplevel_cons '* (__toplevel_cons '_ ())))))))))))
                        (__toplevel_cons
                           (__toplevel_cons
                              '_
                              (__toplevel_cons
                                 '*
                                 (__toplevel_cons
                                    '_
                                    (__toplevel_cons
                                       '*
                                       (__toplevel_cons
                                          '_
                                          (__toplevel_cons
                                             '*
                                             (__toplevel_cons
                                                '*
                                                (__toplevel_cons '* (__toplevel_cons '* (__toplevel_cons '* (__toplevel_cons '* ())))))))))))
                           (__toplevel_cons
                              (__toplevel_cons
                                 '_
                                 (__toplevel_cons
                                    '_
                                    (__toplevel_cons
                                       '_
                                       (__toplevel_cons
                                          '*
                                          (__toplevel_cons
                                             '_
                                             (__toplevel_cons
                                                '_
                                                (__toplevel_cons
                                                   '_
                                                   (__toplevel_cons '_ (__toplevel_cons '_ (__toplevel_cons '_ (__toplevel_cons '_ ())))))))))))
                              (__toplevel_cons
                                 (__toplevel_cons
                                    '_
                                    (__toplevel_cons
                                       '*
                                       (__toplevel_cons
                                          '*
                                          (__toplevel_cons
                                             '*
                                             (__toplevel_cons
                                                '*
                                                (__toplevel_cons
                                                   '*
                                                   (__toplevel_cons
                                                      '*
                                                      (__toplevel_cons '* (__toplevel_cons '_ (__toplevel_cons '* (__toplevel_cons '* ())))))))))))
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       '_
                                       (__toplevel_cons
                                          '*
                                          (__toplevel_cons
                                             '_
                                             (__toplevel_cons
                                                '_
                                                (__toplevel_cons
                                                   '_
                                                   (__toplevel_cons
                                                      '_
                                                      (__toplevel_cons
                                                         '_
                                                         (__toplevel_cons '_ (__toplevel_cons '_ (__toplevel_cons '_ (__toplevel_cons '_ ())))))))))))
                                    ()))))))))))))
 
(equal? (make-maze 11 11) expected-result)
 
