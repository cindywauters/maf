;; renamed lambdas/lets: 17
 
(define fold (<change>
      (lambda (lst folder state)
         (letrec ((__do_loop (lambda (lst state)
                               (if (null? lst)
                                  state
                                  (__do_loop (cdr lst) (folder (car lst) state))))))
            (__do_loop lst state)))
      (lambda (_lst0 _folder0 _state0)
         (letrec ((___do_loop0 (lambda (_lst1 _state1)
                                 (if (null? _lst1)
                                    _state1
                                    (___do_loop0 (cdr _lst1) (_folder0 (car _lst1) _state1))))))
            (___do_loop0 _lst0 _state0)))))
 
(define proc->vector (<change>
      (lambda (size f)
         (if (zero? size)
            (vector)
            (let ((x (make-vector size (f 0))))
               ((letrec ((loop (lambda (i)
                                (if (< i size)
                                   (begin
                                      (vector-set! x i (f i))
                                      (loop (+ i 1)))
                                   #f))))
                  loop)
                  1)
               x)))
      (lambda (_size0 _f0)
         (if (zero? _size0)
            (vector)
            (let ((_x0 (make-vector _size0 (_f0 0))))
               ((letrec ((_loop0 (lambda (_i0)
                                  (if (< _i0 _size0)
                                     (begin
                                        (vector-set! _x0 _i0 (_f0 _i0))
                                        (_loop0 (+ _i0 1)))
                                     #f))))
                  _loop0)
                  1)
               _x0)))))
 
(define vector-fold (<change>
      (lambda (vec folder state)
         (let ((len (vector-length vec)))
            (letrec ((__do_loop (lambda (i state)
                                  (if (= i len)
                                     state
                                     (__do_loop (+ i 1) (folder (vector-ref vec i) state))))))
               (__do_loop 0 state))))
      (lambda (_vec0 _folder0 _state0)
         (let ((_len0 (vector-length _vec0)))
            (letrec ((___do_loop0 (lambda (_i0 _state1)
                                    (if (= _i0 _len0)
                                       _state1
                                       (___do_loop0 (+ _i0 1) (_folder0 (vector-ref _vec0 _i0) _state1))))))
               (___do_loop0 0 _state0))))))
 
(define vector-map (lambda (vec proc)
      (proc->vector (vector-length vec) (lambda (i) (proc (vector-ref vec i))))))
 
(define giota (lambda (limit)
      ((letrec ((_-*- (lambda (limit res)
                       (if (zero? limit)
                          res
                          (let ((limit (- limit 1)))
                             (_-*- limit (cons limit res)))))))
         _-*-)
         limit
         ())))
 
(define gnatural-fold (<change>
      (lambda (limit folder state)
         (letrec ((__do_loop (lambda (i state)
                               (if (= i limit)
                                  state
                                  (__do_loop (+ i 1) (folder i state))))))
            (__do_loop 0 state)))
      (lambda (_limit0 _folder0 _state0)
         (letrec ((___do_loop0 (lambda (_i0 _state1)
                                 (if (= _i0 _limit0)
                                    _state1
                                    (___do_loop0 (+ _i0 1) (_folder0 _i0 _state1))))))
            (___do_loop0 0 _state0)))))
 
(define gnatural-for-each (<change>
      (lambda (limit proc!)
         (letrec ((__do_loop (lambda (i)
                               (if (= i limit)
                                  #f
                                  (begin
                                     (proc! i)
                                     (__do_loop (+ i 1)))))))
            (__do_loop 0)))
      (lambda (_limit0 _proc!0)
         (letrec ((___do_loop0 (lambda (_i0)
                                 (if (= _i0 _limit0)
                                    #f
                                    (begin
                                       (_proc!0 _i0)
                                       (___do_loop0 (+ _i0 1)))))))
            (___do_loop0 0)))))
 
(define natural-for-all? (<change>
      (lambda (limit ok?)
         ((letrec ((_-*- (lambda (i)
                          (let ((__or_res (= i limit)))
                             (if __or_res
                                __or_res
                                (if (ok? i) (_-*- (+ i 1)) #f))))))
            _-*-)
            0))
      (lambda (_limit0 _ok?0)
         ((letrec ((__-*-0 (lambda (_i0)
                            (let ((___or_res0 (= _i0 _limit0)))
                               (if ___or_res0
                                  ___or_res0
                                  (if (_ok?0 _i0) (__-*-0 (+ _i0 1)) #f))))))
            __-*-0)
            0))))
 
(define natural-there-exists? (<change>
      (lambda (limit ok?)
         ((letrec ((_-*- (lambda (i)
                          (if (not (= i limit))
                             (let ((__or_res (ok? i)))
                                (if __or_res __or_res (_-*- (+ i 1))))
                             #f))))
            _-*-)
            0))
      (lambda (_limit0 _ok?0)
         ((letrec ((__-*-0 (lambda (_i0)
                            (if (not (= _i0 _limit0))
                               (let ((___or_res0 (_ok?0 _i0)))
                                  (if ___or_res0 ___or_res0 (__-*-0 (+ _i0 1))))
                               #f))))
            __-*-0)
            0))))
 
(define there-exists? (<change>
      (lambda (lst ok?)
         ((letrec ((_-*- (lambda (lst)
                          (if (not (null? lst))
                             (let ((__or_res (ok? (car lst))))
                                (if __or_res __or_res (_-*- (cdr lst))))
                             #f))))
            _-*-)
            lst))
      (lambda (_lst0 _ok?0)
         ((letrec ((__-*-0 (lambda (_lst1)
                            (if (not (null? _lst1))
                               (let ((___or_res0 (_ok?0 (car _lst1))))
                                  (if ___or_res0 ___or_res0 (__-*-0 (cdr _lst1))))
                               #f))))
            __-*-0)
            _lst0))))
 
(define fold-over-perm-tree (lambda (universe b-folder b-state t-folder t-state)
      ((letrec ((_-*- (lambda (universe b-state t-state accross)
                       (if (null? universe)
                          (t-folder b-state t-state accross)
                          ((letrec ((_-**- (lambda (in out t-state)
                                            (let* ((first (car in))
                                                   (rest (cdr in))
                                                   (accross (if (null? rest)
                                                              accross
                                                              (lambda (new-t-state)
                                                                 (_-**- rest (cons first out) new-t-state)))))
                                               (b-folder
                                                  first
                                                  b-state
                                                  t-state
                                                  (lambda (new-b-state new-t-state)
                                                     (_-*- (fold out cons rest) new-b-state new-t-state accross))
                                                  accross)))))
                             _-**-)
                             universe
                             ()
                             t-state)))))
         _-*-)
         universe
         b-state
         t-state
         (lambda (final-t-state)
            final-t-state))))
 
(define make-minimal? (lambda (max-size)
      (<change>
         (let ((iotas (proc->vector (+ max-size 1) giota))
               (perm (make-vector max-size 0)))
            (lambda (size graph folder state)
               (fold-over-perm-tree
                  (vector-ref iotas size)
                  (lambda (perm-x x state deeper accross)
                     (let ((__case-atom-key (cmp-next-vertex graph perm x perm-x)))
                        (if (eq? __case-atom-key 'less)
                           #f
                           (if (eq? __case-atom-key 'equal)
                              (begin
                                 (vector-set! perm x perm-x)
                                 (deeper (+ x 1) state))
                              (if (eq? __case-atom-key 'more)
                                 (accross state)
                                 (error "???"))))))
                  0
                  (lambda (leaf-depth state accross)
                     (folder perm state accross))
                  state)))
         (let ((_iotas0 (proc->vector (+ max-size 1) giota))
               (_perm0 (make-vector max-size 0)))
            (lambda (_size0 _graph0 _folder0 _state0)
               (fold-over-perm-tree
                  (vector-ref _iotas0 _size0)
                  (lambda (_perm-x0 _x0 _state1 _deeper0 _accross0)
                     (let ((___case-atom-key0 (cmp-next-vertex _graph0 _perm0 _x0 _perm-x0)))
                        (if (eq? ___case-atom-key0 'less)
                           #f
                           (if (eq? ___case-atom-key0 'equal)
                              (begin
                                 (vector-set! _perm0 _x0 _perm-x0)
                                 (_deeper0 (+ _x0 1) _state1))
                              (if (eq? ___case-atom-key0 'more)
                                 (_accross0 _state1)
                                 (error "???"))))))
                  0
                  (lambda (_leaf-depth0 _state2 _accross1)
                     (_folder0 _perm0 _state2 _accross1))
                  _state0))))))
 
(define cmp-next-vertex (<change>
      (lambda (graph perm x perm-x)
         (let ((from-x (vector-ref graph x))
               (from-perm-x (vector-ref graph perm-x)))
            ((letrec ((_-*- (lambda (y)
                             (if (= x y)
                                'equal
                                (let ((x->y? (vector-ref from-x y))
                                      (perm-y (vector-ref perm y)))
                                   (if (eq? x->y? (vector-ref from-perm-x perm-y))
                                      (let ((y->x? (vector-ref (vector-ref graph y) x)))
                                         (if (eq? y->x? (vector-ref (vector-ref graph perm-y) perm-x))
                                            (_-*- (+ y 1))
                                            (if y->x? 'less 'more)))
                                      (if x->y? 'less 'more)))))))
               _-*-)
               0)))
      (lambda (_graph0 _perm0 _x0 _perm-x0)
         (let ((_from-x0 (vector-ref _graph0 _x0))
               (_from-perm-x0 (vector-ref _graph0 _perm-x0)))
            ((letrec ((__-*-0 (lambda (_y0)
                               (if (= _x0 _y0)
                                  'equal
                                  (let ((_x->y?0 (vector-ref _from-x0 _y0))
                                        (_perm-y0 (vector-ref _perm0 _y0)))
                                     (if (eq? _x->y?0 (vector-ref _from-perm-x0 _perm-y0))
                                        (let ((_y->x?0 (vector-ref (vector-ref _graph0 _y0) _x0)))
                                           (if (eq? _y->x?0 (vector-ref (vector-ref _graph0 _perm-y0) _perm-x0))
                                              (__-*-0 (+ _y0 1))
                                              (if _y->x?0 'less 'more)))
                                        (if _x->y?0 'less 'more)))))))
               __-*-0)
               0)))))
 
(define fold-over-rdg (lambda (size max-out folder state)
      (let* ((root (- size 1))
             (edge? (proc->vector size (lambda (from) (make-vector size #f))))
             (edges (make-vector size ()))
             (out-degrees (make-vector size 0))
             (minimal-folder (make-minimal? root))
             (non-root-minimal? (<change>
                                  (let ((cont (lambda (perm state accross)
                                                (accross #t))))
                                     (lambda (size)
                                        (minimal-folder size edge? cont #t)))
                                  (let ((_cont0 (lambda (_perm0 _state0 _accross0)
                                                  (_accross0 #t))))
                                     (lambda (_size0)
                                        (minimal-folder _size0 edge? _cont0 #t)))))
             (root-minimal? (<change>
                              (let ((cont (lambda (perm state accross)
                                            (let ((__case-atom-key (cmp-next-vertex edge? perm root root)))
                                               (if (eq? __case-atom-key 'less)
                                                  #f
                                                  (if (let ((__or_res (eq? __case-atom-key 'equal))) (if __or_res __or_res (eq? __case-atom-key 'more)))
                                                     (accross #t)
                                                     (error "???")))))))
                                 (lambda ()
                                    (minimal-folder root edge? cont #t)))
                              (let ((_cont0 (lambda (_perm0 _state0 _accross0)
                                              (let ((___case-atom-key0 (cmp-next-vertex edge? _perm0 root root)))
                                                 (if (eq? ___case-atom-key0 'less)
                                                    #f
                                                    (if (let ((___or_res0 (eq? ___case-atom-key0 'equal))) (if ___or_res0 ___or_res0 (eq? ___case-atom-key0 'more)))
                                                       (_accross0 #t)
                                                       (error "???")))))))
                                 (lambda ()
                                    (minimal-folder root edge? _cont0 #t))))))
         ((letrec ((_-*- (lambda (vertex state)
                          (if (not (non-root-minimal? vertex))
                             state
                             (if (= vertex root)
                                (let ((reach? (make-reach? root edges))
                                      (from-root (vector-ref edge? root)))
                                   ((letrec ((_-*- (lambda (v outs efr efrr state)
                                                    (if (not (let ((__or_res (= v root))) (if __or_res __or_res (= outs max-out))))
                                                       (begin
                                                          (vector-set! from-root v #t)
                                                          (let ((state (_-*- (+ v 1) (+ outs 1) (cons v efr) (cons (vector-ref reach? v) efrr) state)))
                                                             (vector-set! from-root v #f)
                                                             (_-*- (+ v 1) outs efr efrr state)))
                                                       (if (if (natural-for-all? root (lambda (v) (there-exists? efrr (lambda (r) (vector-ref r v))))) (root-minimal?) #f)
                                                          (begin
                                                             (vector-set! edges root efr)
                                                             (folder (proc->vector size (lambda (i) (vector-ref edges i))) state))
                                                          state)))))
                                      _-*-)
                                      0
                                      0
                                      ()
                                      ()
                                      state))
                                (let ((from-vertex (vector-ref edge? vertex)))
                                   ((letrec ((_-**- (lambda (sv outs state)
                                                     (if (= sv vertex)
                                                        (begin
                                                           (vector-set! out-degrees vertex outs)
                                                           (_-*- (+ vertex 1) state))
                                                        (let* ((state (_-**- (+ sv 1) outs state))
                                                               (from-sv (vector-ref edge? sv))
                                                               (sv-out (vector-ref out-degrees sv))
                                                               (state (if (= sv-out max-out)
                                                                        state
                                                                        (begin
                                                                           (vector-set! edges sv (cons vertex (vector-ref edges sv)))
                                                                           (vector-set! from-sv vertex #t)
                                                                           (vector-set! out-degrees sv (+ sv-out 1))
                                                                           (let* ((state (_-**- (+ sv 1) outs state))
                                                                                  (state (if (= outs max-out)
                                                                                           state
                                                                                           (begin
                                                                                              (vector-set! from-vertex sv #t)
                                                                                              (vector-set! edges vertex (cons sv (vector-ref edges vertex)))
                                                                                              (let ((state (_-**- (+ sv 1) (+ outs 1) state)))
                                                                                                 (vector-set! edges vertex (cdr (vector-ref edges vertex)))
                                                                                                 (vector-set! from-vertex sv #f)
                                                                                                 state)))))
                                                                              (vector-set! out-degrees sv sv-out)
                                                                              (vector-set! from-sv vertex #f)
                                                                              (vector-set! edges sv (cdr (vector-ref edges sv)))
                                                                              state)))))
                                                           (if (= outs max-out)
                                                              state
                                                              (begin
                                                                 (vector-set! edges vertex (cons sv (vector-ref edges vertex)))
                                                                 (vector-set! from-vertex sv #t)
                                                                 (let ((state (_-**- (+ sv 1) (+ outs 1) state)))
                                                                    (vector-set! from-vertex sv #f)
                                                                    (vector-set! edges vertex (cdr (vector-ref edges vertex)))
                                                                    state))))))))
                                      _-**-)
                                      0
                                      0
                                      state)))))))
            _-*-)
            0
            state))))
 
(define make-reach? (lambda (size vertex->out)
      (<change>
         (let ((res (proc->vector
                      size
                      (lambda (v)
                         (let ((from-v (make-vector size #f)))
                            (vector-set! from-v v #t)
                            (for-each (lambda (x) (vector-set! from-v x #t)) (vector-ref vertex->out v))
                            from-v)))))
            (gnatural-for-each
               size
               (lambda (m)
                  (let ((from-m (vector-ref res m)))
                     (gnatural-for-each
                        size
                        (lambda (f)
                           (let ((from-f (vector-ref res f)))
                              (if (vector-ref from-f m)
                                 (gnatural-for-each size (lambda (t) (if (vector-ref from-m t) (vector-set! from-f t #t) #f)))
                                 #f)))))))
            res)
         (let ((_res0 (proc->vector
                        size
                        (lambda (_v0)
                           (let ((_from-v0 (make-vector size #f)))
                              (vector-set! _from-v0 _v0 #t)
                              (for-each (lambda (_x0) (vector-set! _from-v0 _x0 #t)) (vector-ref vertex->out _v0))
                              _from-v0)))))
            (gnatural-for-each
               size
               (lambda (_m0)
                  (let ((_from-m0 (vector-ref _res0 _m0)))
                     (gnatural-for-each
                        size
                        (lambda (_f0)
                           (let ((_from-f0 (vector-ref _res0 _f0)))
                              (if (vector-ref _from-f0 _m0)
                                 (gnatural-for-each
                                    size
                                    (lambda (_t0)
                                       (if (vector-ref _from-m0 _t0)
                                          (vector-set! _from-f0 _t0 #t)
                                          #f)))
                                 #f)))))))
            _res0))))
 
(define run (<change>
      (lambda (n)
         (fold-over-rdg n 2 cons ()))
      (lambda (_n0)
         (fold-over-rdg _n0 2 cons ()))))
 
(= (length (run 5)) 596)
 
