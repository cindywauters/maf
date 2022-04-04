;; renamed lambdas/lets: 8
;; Of which consistent renamings: 5

(define lookup (lambda (key table)
      ((<change>
         (letrec ((loop (lambda (x)
                          (if (null? x)
                             #f
                             (let ((pair (car x)))
                                (if (eq? (car pair) key) pair (loop (cdr x))))))))
            loop)
         (letrec ((_loop0 (lambda (_x0)
                            (if (not (null? _x0)) ;; NOT RENAMING: inserted not
                               #f
                               (let ((_pair0 (car _x0)))
                                  (if (eq? (car _pair0) key)
                                     _pair0
                                     (_loop0 (cdr _x0))))))))
            _loop0))
         table)))
 
(define properties ())
 
(define get (lambda (key1 key2)
      (let ((x (lookup key1 properties)))
         (if x
            (let ((y (lookup key2 (cdr x))))
               (if y (cdr y) #f))
            #f))))
 
(define put (lambda (key1 key2 val)
      (let ((x (lookup key1 properties)))
         (if x
            (<change>
               (let ((y (lookup key2 (cdr x))))
                  (if y
                     (set-cdr! y val)
                     (set-cdr! x (cons (cons key2 val) (cdr x)))))
               (let ((_y0 (lookup key2 (cdr x))))
                  (if _y0
                     (set-car! _y0 val) ;; NOT RENAMING: changed set-cdr! to set-car!
                     (set-cdr! x (cons (cons key2 val) (cdr x))))))
            (set! properties (cons (list key1 (cons key2 val)) properties))))))
 
(define *current-gensym* 0)
 
(define generate-symbol
      (lambda ()
         (set! *current-gensym* (+ *current-gensym* 1))
         (string->symbol (number->string *current-gensym*))))
 
(define append-to-tail! (lambda (x y)
      (if (null? x)
         y
         (<change>
            (letrec ((__do_loop (lambda (a b)
                                  (if (null? b)
                                     (begin
                                        (set-cdr! a y)
                                        x)
                                     (__do_loop b (cdr b))))))
               (__do_loop x (cdr x)))
            (letrec ((___do_loop0 (lambda (_a0 _b0)
                                    (if (null? _b0)
                                       (begin
                                          (set-cdr! _a0 y)
                                          x)
                                       (___do_loop0 _b0 (cdr _b0))))))
               (___do_loop0 x (cdr x)))))))
 
(define tree-copy (lambda (x)
      (if (not (pair? x))
         x
         (cons (tree-copy (car x)) (tree-copy (cdr x))))))
 
(define *rand* 21)
 
(define init (lambda (n m npats ipats)
      (let ((ipats (tree-copy ipats)))
         (letrec ((__do_loop (lambda (p)
                               (if (null? (cdr p))
                                  (set-cdr! p ipats)
                                  (__do_loop (cdr p))))))
            (__do_loop ipats))
         (letrec ((__do_loop (<change>
                               (lambda (n i name a)
                                  (if (= n 0)
                                     a
                                     (begin
                                        (set! a (cons name a))
                                        (letrec ((__do_loop (lambda (i)
                                                              (if (zero? i)
                                                                 #f
                                                                 (begin
                                                                    (put name (generate-symbol) #f)
                                                                    (__do_loop (- i 1)))))))
                                           (__do_loop i))
                                        (put
                                           name
                                           'pattern
                                           (letrec ((__do_loop (lambda (i ipats a)
                                                                 (if (zero? i)
                                                                    a
                                                                    (begin
                                                                       (set! a (cons (car ipats) a))
                                                                       (__do_loop (- i 1) (cdr ipats) a))))))
                                              (__do_loop npats ipats ())))
                                        (letrec ((__do_loop (lambda (j)
                                                              (if (zero? j)
                                                                 #f
                                                                 (begin
                                                                    (put name (generate-symbol) #f)
                                                                    (__do_loop (- j 1)))))))
                                           (__do_loop (- m i)))
                                        (__do_loop (- n 1) (if (zero? i) m (- i 1)) (generate-symbol) a))))
                               (lambda (_n0 _i0 _name0 _a0)
                                  (if (= _n0 0)
                                     _a0
                                     (begin
                                        (set! _a0 (cons _name0 _a0))
                                        (letrec ((___do_loop0 (lambda (_i1)
                                                                (if (zero? _i1)
                                                                   #f
                                                                   (begin
                                                                      (put _name0 (generate-symbol) #f)
                                                                      (___do_loop0 (- _i1 1)))))))
                                           (___do_loop0 _i0))
                                        (put
                                           _name0
                                           'pattern
                                           (letrec ((___do_loop1 (lambda (_i2 _ipats0 _a1)
                                                                   (if (zero? _i2)
                                                                      _a1
                                                                      (begin
                                                                         (set! _a1 (cons (car _ipats0) _a1))
                                                                         (___do_loop1 (- _i2 1) (cdr _ipats0) _a1))))))
                                              (___do_loop1 npats ipats ())))
                                        (letrec ((___do_loop2 (lambda (_j0)
                                                                (if (zero? _j0)
                                                                   #f
                                                                   (begin
                                                                      (put _name0 (generate-symbol) #f)
                                                                      (___do_loop2 (- _j0 1)))))))
                                           (___do_loop2 (- m _i0)))
                                        (__do_loop (- _n0 1) (if (zero? _i0) m (- _i0 1)) (generate-symbol) _a0)))))))
            (__do_loop n m (generate-symbol) ())))))
 
(define browse-random (lambda ()
      (set! *rand* (remainder (* *rand* 17) 251))
      *rand*))
 
(define randomize (<change>
      (lambda (l)
         (letrec ((__do_loop (lambda (a)
                               (if (null? l)
                                  a
                                  (begin
                                     (let ((n (remainder (browse-random) (length l))))
                                        (if (zero? n)
                                           (begin
                                              (set! a (cons (car l) a))
                                              (set! l (cdr l))
                                              l)
                                           (letrec ((__do_loop (lambda (n x)
                                                                 (if (= n 1)
                                                                    (begin
                                                                       (set! a (cons (cadr x) a))
                                                                       (set-cdr! x (cddr x))
                                                                       x)
                                                                    (__do_loop (- n 1) (cdr x))))))
                                              (__do_loop n l))))
                                     (__do_loop a))))))
            (__do_loop ())))
      (lambda (_l0)
         (letrec ((___do_loop0 (lambda (_a0)
                                 (if (null? _l0)
                                    _a0
                                    (begin
                                       (let ((_n0 (remainder (browse-random) (length _l0))))
                                          (if (zero? _n0)
                                             (begin
                                                (set! _a0 (cons (car _l0) _a0))
                                                (set! _l0 (cdr _l0))
                                                _l0)
                                             (letrec ((___do_loop1 (lambda (_n1 _x0)
                                                                     (if (= _n1 2) ;; NOT RENAMING, 1 -> 2
                                                                        (begin
                                                                           (set! _a0 (cons (cadr _x0) _a0))
                                                                           (set-cdr! _x0 (cddr _x0))
                                                                           _x0)
                                                                        (___do_loop1 (- _n1 1) (cdr _x0))))))
                                                (___do_loop1 _n0 _l0))))
                                       (___do_loop0 _a0))))))
            (___do_loop0 ())))))
 
(define my-match (<change>
      (lambda (pat dat alist)
         (if (null? pat)
            (null? dat)
            (if (null? dat)
               ()
               (if (let ((__or_res (eq? (car pat) '?))) (if __or_res __or_res (eq? (car pat) (car dat))))
                  (my-match (cdr pat) (cdr dat) alist)
                  (if (eq? (car pat) '*)
                     (let ((__or_res (my-match (cdr pat) dat alist)))
                        (if __or_res
                           __or_res
                           (let ((__or_res (my-match (cdr pat) (cdr dat) alist)))
                              (if __or_res
                                 __or_res
                                 (my-match pat (cdr dat) alist)))))
                     (if (not (pair? (car pat)))
                        (if (eq? (string-ref (symbol->string (car pat)) 0) #\?)
                           (let ((val (assq (car pat) alist)))
                              (if val
                                 (my-match (cons (cdr val) (cdr pat)) dat alist)
                                 (my-match (cdr pat) (cdr dat) (cons (cons (car pat) (car dat)) alist))))
                           (if (eq? (string-ref (symbol->string (car pat)) 0) #\*)
                              (let ((val (assq (car pat) alist)))
                                 (if val
                                    (my-match (append (cdr val) (cdr pat)) dat alist)
                                    (letrec ((__do_loop (lambda (l e d)
                                                          (if (let ((__or_res (null? e))) (if __or_res __or_res (my-match (cdr pat) d (cons (cons (car pat) l) alist))))
                                                             (if (null? e) #f #t)
                                                             (__do_loop
                                                                (append-to-tail! l (cons (if (null? d) () (car d)) ()))
                                                                (cdr e)
                                                                (if (null? d) () (cdr d)))))))
                                       (__do_loop () (cons () dat) dat))))
                              #f))
                        (if (pair? (car dat))
                           (if (my-match (car pat) (car dat) alist)
                              (my-match (cdr pat) (cdr dat) alist)
                              #f)
                           #f)))))))
      (lambda (_pat0 _dat0 _alist0)
         (if (null? _pat0)
            (null? _dat0)
            (if (null? _dat0)
               ()
               (if (let ((___or_res0 (eq? (car _pat0) '?))) (if ___or_res0 ___or_res0 (eq? (car _pat0) (car _dat0))))
                  (my-match (cdr _pat0) (cdr _dat0) _alist0)
                  (if (eq? (car _pat0) '*)
                     (let ((___or_res1 (my-match (cdr _pat0) _dat0 _alist0)))
                        (if ___or_res1
                           ___or_res1
                           (let ((___or_res2 (my-match (cdr _pat0) (cdr _dat0) _alist0)))
                              (if ___or_res2
                                 ___or_res2
                                 (my-match _pat0 (cdr _dat0) _alist0)))))
                     (if (not (pair? (car _pat0)))
                        (if (eq? (string-ref (symbol->string (car _pat0)) 0) #\?)
                           (let ((_val0 (assq (car _pat0) _alist0)))
                              (if _val0
                                 (my-match (cons (cdr _val0) (cdr _pat0)) _dat0 _alist0)
                                 (my-match (cdr _pat0) (cdr _dat0) (cons (cons (car _pat0) (car _dat0)) _alist0))))
                           (if (eq? (string-ref (symbol->string (car _pat0)) 0) #\*)
                              (let ((_val1 (assq (car _pat0) _alist0)))
                                 (if _val1
                                    (my-match (append (cdr _val1) (cdr _pat0)) _dat0 _alist0)
                                    (letrec ((___do_loop0 (lambda (_l0 _e0 _d0)
                                                            (if (let ((___or_res3 (null? _e0))) (if ___or_res3 ___or_res3 (my-match (cdr _pat0) _d0 (cons (cons (car _pat0) _l0) _alist0))))
                                                               (if (null? _e0) #f #t)
                                                               (___do_loop0
                                                                  (append-to-tail! _l0 (cons (if (null? _d0) () (car _d0)) ()))
                                                                  (cdr _e0)
                                                                  (if (null? _d0) () (cdr _d0)))))))
                                       (___do_loop0 () (cons () _dat0) _dat0))))
                              #f))
                        (if (pair? (car _dat0))
                           (if (my-match (car _pat0) (car _dat0) _alist0)
                              (my-match (cdr _pat0) (cdr _dat0) _alist0)
                              #f)
                           #f)))))))))
 
(define database (randomize
      (init
         100
         10
         4
         (__toplevel_cons
            (__toplevel_cons
               'a
               (__toplevel_cons
                  'a
                  (__toplevel_cons
                     'a
                     (__toplevel_cons
                        'b
                        (__toplevel_cons
                           'b
                           (__toplevel_cons
                              'b
                              (__toplevel_cons
                                 'b
                                 (__toplevel_cons
                                    'a
                                    (__toplevel_cons
                                       'a
                                       (__toplevel_cons
                                          'a
                                          (__toplevel_cons
                                             'a
                                             (__toplevel_cons
                                                'a
                                                (__toplevel_cons
                                                   'b
                                                   (__toplevel_cons 'b (__toplevel_cons 'a (__toplevel_cons 'a (__toplevel_cons 'a ())))))))))))))))))
            (__toplevel_cons
               (__toplevel_cons
                  'a
                  (__toplevel_cons
                     'a
                     (__toplevel_cons
                        'b
                        (__toplevel_cons
                           'b
                           (__toplevel_cons
                              'b
                              (__toplevel_cons
                                 'b
                                 (__toplevel_cons
                                    'a
                                    (__toplevel_cons
                                       'a
                                       (__toplevel_cons
                                          (__toplevel_cons 'a (__toplevel_cons 'a ()))
                                          (__toplevel_cons (__toplevel_cons 'b (__toplevel_cons 'b ())) ()))))))))))
               (__toplevel_cons
                  (__toplevel_cons
                     'a
                     (__toplevel_cons
                        'a
                        (__toplevel_cons
                           'a
                           (__toplevel_cons
                              'b
                              (__toplevel_cons
                                 (__toplevel_cons 'b (__toplevel_cons 'a ()))
                                 (__toplevel_cons 'b (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'a ())))))))))
                  ()))))))
 
(define browse (lambda (pats)
      (investigate database pats)))
 
(define investigate (<change>
      (lambda (units pats)
         (letrec ((__do_loop (lambda (units)
                               (if (null? units)
                                  #f
                                  (begin
                                     (letrec ((__do_loop (lambda (pats)
                                                           (if (null? pats)
                                                              #f
                                                              (begin
                                                                 (letrec ((__do_loop (lambda (p)
                                                                                       (if (null? p)
                                                                                          #f
                                                                                          (begin
                                                                                             (my-match (car pats) (car p) ())
                                                                                             (__do_loop (cdr p)))))))
                                                                    (__do_loop (get (car units) 'pattern)))
                                                                 (__do_loop (cdr pats)))))))
                                        (__do_loop pats))
                                     (__do_loop (cdr units)))))))
            (__do_loop units)))
      (lambda (_units0 _pats0)
         (letrec ((___do_loop0 (lambda (_units1)
                                 (if (null? _units1)
                                    #f
                                    (begin
                                       (letrec ((___do_loop1 (lambda (_pats1)
                                                               (if (null? _pats1)
                                                                  #f
                                                                  (begin
                                                                     (letrec ((___do_loop2 (lambda (_p0)
                                                                                             (if (null? _p0)
                                                                                                #f
                                                                                                (begin
                                                                                                   (my-match (car _pats1) (car _p0) ())
                                                                                                   (___do_loop2 (cdr _p0)))))))
                                                                        (___do_loop2 (get (car _units1) 'pattern)))
                                                                     (___do_loop1 (cdr _pats1)))))))
                                          (___do_loop1 _pats0))
                                       (___do_loop0 (cdr _units1)))))))
            (___do_loop0 _units0)))))
 
(browse
   (__toplevel_cons
      (__toplevel_cons
         '*a
         (__toplevel_cons
            '?b
            (__toplevel_cons
               '*b
               (__toplevel_cons
                  '?b
                  (__toplevel_cons
                     'a
                     (__toplevel_cons '*a (__toplevel_cons 'a (__toplevel_cons '*b (__toplevel_cons '*a ())))))))))
      (__toplevel_cons
         (__toplevel_cons
            '*a
            (__toplevel_cons
               '*b
               (__toplevel_cons
                  '*b
                  (__toplevel_cons
                     '*a
                     (__toplevel_cons (__toplevel_cons '*a ()) (__toplevel_cons (__toplevel_cons '*b ()) ()))))))
         (__toplevel_cons
            (__toplevel_cons
               '?
               (__toplevel_cons
                  '?
                  (__toplevel_cons
                     '*
                     (__toplevel_cons
                        (__toplevel_cons 'b (__toplevel_cons 'a ()))
                        (__toplevel_cons '* (__toplevel_cons '? (__toplevel_cons '? ())))))))
            ()))))
 
*current-gensym*
 
