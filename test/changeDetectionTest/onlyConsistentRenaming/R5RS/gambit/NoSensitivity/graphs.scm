;; renamed lambdas/lets: 16
 
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
 
(define vector-fold (lambda (vec folder state)
      (let ((len (vector-length vec)))
         (letrec ((__do_loop (lambda (i state)
                               (if (= i len)
                                  state
                                  (__do_loop (+ i 1) (folder (vector-ref vec i) state))))))
            (__do_loop 0 state)))))
 
(define vector-map (lambda (vec proc)
      (proc->vector
         (vector-length vec)
         (<change>
            (lambda (i)
               (proc (vector-ref vec i)))
            (lambda (_i0)
               (proc (vector-ref vec _i0)))))))
 
(define giota (lambda (limit)
      ((<change>
         (letrec ((_-*- (lambda (limit res)
                          (if (zero? limit)
                             res
                             (let ((limit (- limit 1)))
                                (_-*- limit (cons limit res)))))))
            _-*-)
         (letrec ((__-*-0 (lambda (_limit0 _res0)
                            (if (zero? _limit0)
                               _res0
                               (let ((_limit1 (- _limit0 1)))
                                  (__-*-0 _limit1 (cons _limit1 _res0)))))))
            __-*-0))
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
 
(define gnatural-for-each (lambda (limit proc!)
      (<change>
         (letrec ((__do_loop (lambda (i)
                               (if (= i limit)
                                  #f
                                  (begin
                                     (proc! i)
                                     (__do_loop (+ i 1)))))))
            (__do_loop 0))
         (letrec ((___do_loop0 (lambda (_i0)
                                 (if (= _i0 limit)
                                    #f
                                    (begin
                                       (proc! _i0)
                                       (___do_loop0 (+ _i0 1)))))))
            (___do_loop0 0)))))
 
(define natural-for-all? (lambda (limit ok?)
      ((letrec ((_-*- (<change>
                       (lambda (i)
                          (let ((__or_res (= i limit)))
                             (if __or_res
                                __or_res
                                (if (ok? i) (_-*- (+ i 1)) #f))))
                       (lambda (_i0)
                          (let ((___or_res0 (= _i0 limit)))
                             (if ___or_res0
                                ___or_res0
                                (if (ok? _i0) (_-*- (+ _i0 1)) #f)))))))
         _-*-)
         0)))
 
(define natural-there-exists? (lambda (limit ok?)
      ((letrec ((_-*- (<change>
                       (lambda (i)
                          (if (not (= i limit))
                             (let ((__or_res (ok? i)))
                                (if __or_res __or_res (_-*- (+ i 1))))
                             #f))
                       (lambda (_i0)
                          (if (not (= _i0 limit))
                             (let ((___or_res0 (ok? _i0)))
                                (if ___or_res0 ___or_res0 (_-*- (+ _i0 1))))
                             #f)))))
         _-*-)
         0)))
 
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
 
(define fold-over-perm-tree (<change>
      (lambda (universe b-folder b-state t-folder t-state)
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
               final-t-state)))
      (lambda (_universe0 _b-folder0 _b-state0 _t-folder0 _t-state0)
         ((letrec ((__-*-0 (lambda (_universe1 _b-state1 _t-state1 _accross0)
                            (if (null? _universe1)
                               (_t-folder0 _b-state1 _t-state1 _accross0)
                               ((letrec ((__-**-0 (lambda (_in0 _out0 _t-state2)
                                                   (let* ((_first0 (car _in0))
                                                          (_rest0 (cdr _in0))
                                                          (_accross1 (if (null? _rest0)
                                                                       _accross0
                                                                       (lambda (_new-t-state0)
                                                                          (__-**-0 _rest0 (cons _first0 _out0) _new-t-state0)))))
                                                      (_b-folder0
                                                         _first0
                                                         _b-state1
                                                         _t-state2
                                                         (lambda (_new-b-state0 _new-t-state1)
                                                            (__-*-0 (fold _out0 cons _rest0) _new-b-state0 _new-t-state1 _accross1))
                                                         _accross1)))))
                                  __-**-0)
                                  _universe1
                                  ()
                                  _t-state1)))))
            __-*-0)
            _universe0
            _b-state0
            _t-state0
            (lambda (_final-t-state0)
               _final-t-state0)))))
 
(define make-minimal? (<change>
      (lambda (max-size)
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
                  state))))
      (lambda (_max-size0)
         (let ((_iotas0 (proc->vector (+ _max-size0 1) giota))
               (_perm0 (make-vector _max-size0 0)))
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
 
(define cmp-next-vertex (lambda (graph perm x perm-x)
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
            0))))
 
(define fold-over-rdg (lambda (size max-out folder state)
      (let* ((root (- size 1))
             (edge? (proc->vector
                      size
                      (<change>
                         (lambda (from)
                            (make-vector size #f))
                         (lambda (_from0)
                            (make-vector size #f)))))
             (edges (make-vector size ()))
             (out-degrees (make-vector size 0))
             (minimal-folder (make-minimal? root))
             (non-root-minimal? (let ((cont (lambda (perm state accross)
                                             (accross #t))))
                                  (<change>
                                     (lambda (size)
                                        (minimal-folder size edge? cont #t))
                                     (lambda (_size0)
                                        (minimal-folder _size0 edge? cont #t)))))
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
         ((<change>
            (letrec ((_-*- (lambda (vertex state)
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
            (letrec ((__-*-0 (lambda (_vertex0 _state0)
                               (if (not (non-root-minimal? _vertex0))
                                  _state0
                                  (if (= _vertex0 root)
                                     (let ((_reach?0 (make-reach? root edges))
                                           (_from-root0 (vector-ref edge? root)))
                                        ((letrec ((__-*-1 (lambda (_v0 _outs0 _efr0 _efrr0 _state1)
                                                           (if (not (let ((___or_res0 (= _v0 root))) (if ___or_res0 ___or_res0 (= _outs0 max-out))))
                                                              (begin
                                                                 (vector-set! _from-root0 _v0 #t)
                                                                 (let ((_state2 (__-*-1 (+ _v0 1) (+ _outs0 1) (cons _v0 _efr0) (cons (vector-ref _reach?0 _v0) _efrr0) _state1)))
                                                                    (vector-set! _from-root0 _v0 #f)
                                                                    (__-*-1 (+ _v0 1) _outs0 _efr0 _efrr0 _state2)))
                                                              (if (if (natural-for-all? root (lambda (_v1) (there-exists? _efrr0 (lambda (_r0) (vector-ref _r0 _v1))))) (root-minimal?) #f)
                                                                 (begin
                                                                    (vector-set! edges root _efr0)
                                                                    (folder (proc->vector size (lambda (_i0) (vector-ref edges _i0))) _state1))
                                                                 _state1)))))
                                           __-*-1)
                                           0
                                           0
                                           ()
                                           ()
                                           _state0))
                                     (let ((_from-vertex0 (vector-ref edge? _vertex0)))
                                        ((letrec ((__-**-0 (lambda (_sv0 _outs1 _state3)
                                                            (if (= _sv0 _vertex0)
                                                               (begin
                                                                  (vector-set! out-degrees _vertex0 _outs1)
                                                                  (__-*-0 (+ _vertex0 1) _state3))
                                                               (let* ((_state4 (__-**-0 (+ _sv0 1) _outs1 _state3))
                                                                      (_from-sv0 (vector-ref edge? _sv0))
                                                                      (_sv-out0 (vector-ref out-degrees _sv0))
                                                                      (_state5 (if (= _sv-out0 max-out)
                                                                                 _state4
                                                                                 (begin
                                                                                    (vector-set! edges _sv0 (cons _vertex0 (vector-ref edges _sv0)))
                                                                                    (vector-set! _from-sv0 _vertex0 #t)
                                                                                    (vector-set! out-degrees _sv0 (+ _sv-out0 1))
                                                                                    (let* ((_state6 (__-**-0 (+ _sv0 1) _outs1 _state4))
                                                                                           (_state7 (if (= _outs1 max-out)
                                                                                                      _state6
                                                                                                      (begin
                                                                                                         (vector-set! _from-vertex0 _sv0 #t)
                                                                                                         (vector-set! edges _vertex0 (cons _sv0 (vector-ref edges _vertex0)))
                                                                                                         (let ((_state8 (__-**-0 (+ _sv0 1) (+ _outs1 1) _state6)))
                                                                                                            (vector-set! edges _vertex0 (cdr (vector-ref edges _vertex0)))
                                                                                                            (vector-set! _from-vertex0 _sv0 #f)
                                                                                                            _state8)))))
                                                                                       (vector-set! out-degrees _sv0 _sv-out0)
                                                                                       (vector-set! _from-sv0 _vertex0 #f)
                                                                                       (vector-set! edges _sv0 (cdr (vector-ref edges _sv0)))
                                                                                       _state7)))))
                                                                  (if (= _outs1 max-out)
                                                                     _state5
                                                                     (begin
                                                                        (vector-set! edges _vertex0 (cons _sv0 (vector-ref edges _vertex0)))
                                                                        (vector-set! _from-vertex0 _sv0 #t)
                                                                        (let ((_state9 (__-**-0 (+ _sv0 1) (+ _outs1 1) _state5)))
                                                                           (vector-set! _from-vertex0 _sv0 #f)
                                                                           (vector-set! edges _vertex0 (cdr (vector-ref edges _vertex0)))
                                                                           _state9))))))))
                                           __-**-0)
                                           0
                                           0
                                           _state0)))))))
               __-*-0))
            0
            state))))
 
(define make-reach? (lambda (size vertex->out)
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
               (<change>
                  (let ((from-m (vector-ref res m)))
                     (gnatural-for-each
                        size
                        (lambda (f)
                           (let ((from-f (vector-ref res f)))
                              (if (vector-ref from-f m)
                                 (gnatural-for-each size (lambda (t) (if (vector-ref from-m t) (vector-set! from-f t #t) #f)))
                                 #f)))))
                  (let ((_from-m0 (vector-ref res m)))
                     (gnatural-for-each
                        size
                        (lambda (_f0)
                           (let ((_from-f0 (vector-ref res _f0)))
                              (if (vector-ref _from-f0 m)
                                 (gnatural-for-each
                                    size
                                    (lambda (_t0)
                                       (if (vector-ref _from-m0 _t0)
                                          (vector-set! _from-f0 _t0 #t)
                                          #f)))
                                 #f))))))))
         res)))
 
(define run (lambda (n)
      (fold-over-rdg n 2 cons ())))
 
(= (length (run 5)) 596)
 
