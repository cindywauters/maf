;; renamed lambdas/lets: 7
 
(define my-iota (lambda (n)
      (letrec ((__do_loop (<change>
                            (lambda (n list)
                               (if (zero? n)
                                  list
                                  (__do_loop (- n 1) (cons (- n 1) list))))
                            (lambda (_n0 _list0)
                               (if (zero? _n0)
                                  _list0
                                  (__do_loop (- _n0 1) (cons (- _n0 1) _list0)))))))
         (__do_loop n ()))))
 
(define size 511)
 
(define classmax 3)
 
(define typemax 12)
 
(define *iii* 0)
 
(define *kount* 0)
 
(define *d* 8)
 
(define *piececount* (make-vector (+ classmax 1) 0))
 
(define *class* (make-vector (+ typemax 1) 0))
 
(define *piecemax* (make-vector (+ typemax 1) 0))
 
(define *puzzle* (make-vector (+ size 1)))
 
(define *p* (make-vector (+ typemax 1)))
 
(define fit (lambda (i j)
      (let ((end (vector-ref *piecemax* i)))
         (<change>
            (letrec ((__do_loop (lambda (k)
                                  (if (let ((__or_res (> k end))) (if __or_res __or_res (if (vector-ref (vector-ref *p* i) k) (vector-ref *puzzle* (+ j k)) #f)))
                                     (if (> k end) #t #f)
                                     (__do_loop (+ k 1))))))
               (__do_loop 0))
            (letrec ((___do_loop0 (lambda (_k0)
                                    (if (let ((___or_res0 (> _k0 end))) (if ___or_res0 ___or_res0 (if (vector-ref (vector-ref *p* i) _k0) (vector-ref *puzzle* (+ j _k0)) #f)))
                                       (if (> _k0 end) #t #f)
                                       (___do_loop0 (+ _k0 1))))))
               (___do_loop0 0))))))
 
(define place (<change>
      (lambda (i j)
         (let ((end (vector-ref *piecemax* i)))
            (letrec ((__do_loop (lambda (k)
                                  (if (> k end)
                                     #f
                                     (begin
                                        (if (vector-ref (vector-ref *p* i) k)
                                           (begin
                                              (vector-set! *puzzle* (+ j k) #t)
                                              #t)
                                           #f)
                                        (__do_loop (+ k 1)))))))
               (__do_loop 0))
            (vector-set!
               *piececount*
               (vector-ref *class* i)
               (- (vector-ref *piececount* (vector-ref *class* i)) 1))
            (letrec ((__do_loop (lambda (k)
                                  (if (let ((__or_res (> k size))) (if __or_res __or_res (not (vector-ref *puzzle* k))))
                                     (if (> k size) 0 k)
                                     (__do_loop (+ k 1))))))
               (__do_loop j))))
      (lambda (_i0 _j0)
         (let ((_end0 (vector-ref *piecemax* _i0)))
            (letrec ((___do_loop0 (lambda (_k0)
                                    (if (> _k0 _end0)
                                       #f
                                       (begin
                                          (if (vector-ref (vector-ref *p* _i0) _k0)
                                             (begin
                                                (vector-set! *puzzle* (+ _j0 _k0) #t)
                                                #t)
                                             #f)
                                          (___do_loop0 (+ _k0 1)))))))
               (___do_loop0 0))
            (vector-set!
               *piececount*
               (vector-ref *class* _i0)
               (- (vector-ref *piececount* (vector-ref *class* _i0)) 1))
            (letrec ((___do_loop1 (lambda (_k1)
                                    (if (let ((___or_res0 (> _k1 size))) (if ___or_res0 ___or_res0 (not (vector-ref *puzzle* _k1))))
                                       (if (> _k1 size) 0 _k1)
                                       (___do_loop1 (+ _k1 1))))))
               (___do_loop1 _j0))))))
 
(define puzzle-remove (lambda (i j)
      (let ((end (vector-ref *piecemax* i)))
         (letrec ((__do_loop (lambda (k)
                               (if (> k end)
                                  #f
                                  (begin
                                     (if (vector-ref (vector-ref *p* i) k)
                                        (begin
                                           (vector-set! *puzzle* (+ j k) #f)
                                           #f)
                                        #f)
                                     (__do_loop (+ k 1)))))))
            (__do_loop 0))
         (vector-set!
            *piececount*
            (vector-ref *class* i)
            (+ (vector-ref *piececount* (vector-ref *class* i)) 1)))))
 
(define trial (<change>
      (lambda (j)
         (let ((k 0))
            (call-with-current-continuation
               (lambda (return)
                  (letrec ((__do_loop (lambda (i)
                                        (if (> i typemax)
                                           (begin
                                              (set! *kount* (+ *kount* 1))
                                              #f)
                                           (begin
                                              (if (not (zero? (vector-ref *piececount* (vector-ref *class* i))))
                                                 (if (fit i j)
                                                    (begin
                                                       (set! k (place i j))
                                                       (if (let ((__or_res (trial k))) (if __or_res __or_res (zero? k)))
                                                          (begin
                                                             (set! *kount* (+ *kount* 1))
                                                             (return #t))
                                                          (puzzle-remove i j)))
                                                    #f)
                                                 #f)
                                              (__do_loop (+ i 1)))))))
                     (__do_loop 0))))))
      (lambda (_j0)
         (let ((_k0 0))
            (call-with-current-continuation
               (lambda (_return0)
                  (letrec ((___do_loop0 (lambda (_i0)
                                          (if (> _i0 typemax)
                                             (begin
                                                (set! *kount* (+ *kount* 1))
                                                #f)
                                             (begin
                                                (if (not (zero? (vector-ref *piececount* (vector-ref *class* _i0))))
                                                   (if (fit _i0 _j0)
                                                      (begin
                                                         (set! _k0 (place _i0 _j0))
                                                         (if (let ((___or_res0 (trial _k0))) (if ___or_res0 ___or_res0 (zero? _k0)))
                                                            (begin
                                                               (set! *kount* (+ *kount* 1))
                                                               (_return0 #t))
                                                            (puzzle-remove _i0 _j0)))
                                                      #f)
                                                   #f)
                                                (___do_loop0 (+ _i0 1)))))))
                     (___do_loop0 0))))))))
 
(define definePiece (<change>
      (lambda (iclass ii jj kk)
         (let ((index 0))
            (letrec ((__do_loop (lambda (i)
                                  (if (> i ii)
                                     #f
                                     (begin
                                        (letrec ((__do_loop (lambda (j)
                                                              (if (> j jj)
                                                                 #f
                                                                 (begin
                                                                    (letrec ((__do_loop (lambda (k)
                                                                                          (if (> k kk)
                                                                                             #f
                                                                                             (begin
                                                                                                (set! index (+ i (* *d* (+ j (* *d* k)))))
                                                                                                (vector-set! (vector-ref *p* *iii*) index #t)
                                                                                                (__do_loop (+ k 1)))))))
                                                                       (__do_loop 0))
                                                                    (__do_loop (+ j 1)))))))
                                           (__do_loop 0))
                                        (__do_loop (+ i 1)))))))
               (__do_loop 0))
            (vector-set! *class* *iii* iclass)
            (vector-set! *piecemax* *iii* index)
            (if (not (= *iii* typemax))
               (set! *iii* (+ *iii* 1))
               #f)))
      (lambda (_iclass0 _ii0 _jj0 _kk0)
         (let ((_index0 0))
            (letrec ((___do_loop0 (lambda (_i0)
                                    (if (> _i0 _ii0)
                                       #f
                                       (begin
                                          (letrec ((___do_loop1 (lambda (_j0)
                                                                  (if (> _j0 _jj0)
                                                                     #f
                                                                     (begin
                                                                        (letrec ((___do_loop2 (lambda (_k0)
                                                                                                (if (> _k0 _kk0)
                                                                                                   #f
                                                                                                   (begin
                                                                                                      (set! _index0 (+ _i0 (* *d* (+ _j0 (* *d* _k0)))))
                                                                                                      (vector-set! (vector-ref *p* *iii*) _index0 #t)
                                                                                                      (___do_loop2 (+ _k0 1)))))))
                                                                           (___do_loop2 0))
                                                                        (___do_loop1 (+ _j0 1)))))))
                                             (___do_loop1 0))
                                          (___do_loop0 (+ _i0 1)))))))
               (___do_loop0 0))
            (vector-set! *class* *iii* _iclass0)
            (vector-set! *piecemax* *iii* _index0)
            (if (not (= *iii* typemax))
               (set! *iii* (+ *iii* 1))
               #f)))))
 
(define start (<change>
      (lambda ()
         (set! *kount* 0)
         (letrec ((__do_loop (lambda (m)
                               (if (> m size)
                                  #f
                                  (begin
                                     (vector-set! *puzzle* m #t)
                                     (__do_loop (+ m 1)))))))
            (__do_loop 0))
         (letrec ((__do_loop (lambda (i)
                               (if (> i 5)
                                  #f
                                  (begin
                                     (letrec ((__do_loop (lambda (j)
                                                           (if (> j 5)
                                                              #f
                                                              (begin
                                                                 (letrec ((__do_loop (lambda (k)
                                                                                       (if (> k 5)
                                                                                          #f
                                                                                          (begin
                                                                                             (vector-set! *puzzle* (+ i (* *d* (+ j (* *d* k)))) #f)
                                                                                             (__do_loop (+ k 1)))))))
                                                                    (__do_loop 1))
                                                                 (__do_loop (+ j 1)))))))
                                        (__do_loop 1))
                                     (__do_loop (+ i 1)))))))
            (__do_loop 1))
         (letrec ((__do_loop (lambda (i)
                               (if (> i typemax)
                                  #f
                                  (begin
                                     (letrec ((__do_loop (lambda (m)
                                                           (if (> m size)
                                                              #f
                                                              (begin
                                                                 (vector-set! (vector-ref *p* i) m #f)
                                                                 (__do_loop (+ m 1)))))))
                                        (__do_loop 0))
                                     (__do_loop (+ i 1)))))))
            (__do_loop 0))
         (set! *iii* 0)
         (definePiece 0 3 1 0)
         (definePiece 0 1 0 3)
         (definePiece 0 0 3 1)
         (definePiece 0 1 3 0)
         (definePiece 0 3 0 1)
         (definePiece 0 0 1 3)
         (definePiece 1 2 0 0)
         (definePiece 1 0 2 0)
         (definePiece 1 0 0 2)
         (definePiece 2 1 1 0)
         (definePiece 2 1 0 1)
         (definePiece 2 0 1 1)
         (definePiece 3 1 1 1)
         (vector-set! *piececount* 0 13)
         (vector-set! *piececount* 1 3)
         (vector-set! *piececount* 2 1)
         (vector-set! *piececount* 3 1)
         (let ((m (+ (* *d* (+ *d* 1)) 1))
               (n 0))
            (if (fit 0 m)
               (set! n (place 0 m))
               (begin
                  (newline)
                  (display "Error.")))
            (if (trial n) *kount* #f)))
      (lambda ()
         (set! *kount* 0)
         (letrec ((___do_loop0 (lambda (_m0)
                                 (if (> _m0 size)
                                    #f
                                    (begin
                                       (vector-set! *puzzle* _m0 #t)
                                       (___do_loop0 (+ _m0 1)))))))
            (___do_loop0 0))
         (letrec ((___do_loop1 (lambda (_i0)
                                 (if (> _i0 5)
                                    #f
                                    (begin
                                       (letrec ((___do_loop2 (lambda (_j0)
                                                               (if (> _j0 5)
                                                                  #f
                                                                  (begin
                                                                     (letrec ((___do_loop3 (lambda (_k0)
                                                                                             (if (> _k0 5)
                                                                                                #f
                                                                                                (begin
                                                                                                   (vector-set! *puzzle* (+ _i0 (* *d* (+ _j0 (* *d* _k0)))) #f)
                                                                                                   (___do_loop3 (+ _k0 1)))))))
                                                                        (___do_loop3 1))
                                                                     (___do_loop2 (+ _j0 1)))))))
                                          (___do_loop2 1))
                                       (___do_loop1 (+ _i0 1)))))))
            (___do_loop1 1))
         (letrec ((___do_loop4 (lambda (_i1)
                                 (if (> _i1 typemax)
                                    #f
                                    (begin
                                       (letrec ((___do_loop5 (lambda (_m1)
                                                               (if (> _m1 size)
                                                                  #f
                                                                  (begin
                                                                     (vector-set! (vector-ref *p* _i1) _m1 #f)
                                                                     (___do_loop5 (+ _m1 1)))))))
                                          (___do_loop5 0))
                                       (___do_loop4 (+ _i1 1)))))))
            (___do_loop4 0))
         (set! *iii* 0)
         (definePiece 0 3 1 0)
         (definePiece 0 1 0 3)
         (definePiece 0 0 3 1)
         (definePiece 0 1 3 0)
         (definePiece 0 3 0 1)
         (definePiece 0 0 1 3)
         (definePiece 1 2 0 0)
         (definePiece 1 0 2 0)
         (definePiece 1 0 0 2)
         (definePiece 2 1 1 0)
         (definePiece 2 1 0 1)
         (definePiece 2 0 1 1)
         (definePiece 3 1 1 1)
         (vector-set! *piececount* 0 13)
         (vector-set! *piececount* 1 3)
         (vector-set! *piececount* 2 1)
         (vector-set! *piececount* 3 1)
         (let ((_m2 (+ (* *d* (+ *d* 1)) 1))
               (_n0 0))
            (if (fit 0 _m2)
               (set! _n0 (place 0 _m2))
               (begin
                  (newline)
                  (display "Error.")))
            (if (trial _n0) *kount* #f)))))
 
(for-each (lambda (i) (vector-set! *p* i (make-vector (+ size 1)))) (my-iota (+ typemax 1)))
 
(= (start) 2005)
 
