;; renamed lambdas/lets: 17
 
(define make-node (<change>
      (lambda ()
         (vector 'node () () (snb) #f #f #f #f #f #f #f))
      (lambda ()
         (vector 'node () () (snb) #f #f #f #f #f #f #f))))
 
(define node-parents (<change>
      (lambda (node)
         (vector-ref node 1))
      (lambda (_node0)
         (vector-ref _node0 1))))
 
(define node-sons (lambda (node)
      (vector-ref node 2)))
 
(define node-sn (<change>
      (lambda (node)
         (vector-ref node 3))
      (lambda (_node0)
         (vector-ref _node0 3))))
 
(define node-entry1 (<change>
      (lambda (node)
         (vector-ref node 4))
      (lambda (_node0)
         (vector-ref _node0 4))))
 
(define node-entry2 (lambda (node)
      (vector-ref node 5)))
 
(define node-entry3 (lambda (node)
      (vector-ref node 6)))
 
(define node-entry4 (lambda (node)
      (vector-ref node 7)))
 
(define node-entry5 (lambda (node)
      (vector-ref node 8)))
 
(define node-entry6 (lambda (node)
      (vector-ref node 9)))
 
(define node-mark (lambda (node)
      (vector-ref node 10)))
 
(define node-parents-set! (<change>
      (lambda (node v)
         (vector-set! node 1 v))
      (lambda (_node0 _v0)
         (vector-set! _node0 1 _v0))))
 
(define node-sons-set! (<change>
      (lambda (node v)
         (vector-set! node 2 v))
      (lambda (_node0 _v0)
         (vector-set! _node0 2 _v0))))
 
(define node-sn-set! (lambda (node v)
      (vector-set! node 3 v)))
 
(define node-entry1-set! (<change>
      (lambda (node v)
         (vector-set! node 4 v))
      (lambda (_node0 _v0)
         (vector-set! _node0 4 _v0))))
 
(define node-entry2-set! (lambda (node v)
      (vector-set! node 5 v)))
 
(define node-entry3-set! (lambda (node v)
      (vector-set! node 6 v)))
 
(define node-entry4-set! (<change>
      (lambda (node v)
         (vector-set! node 7 v))
      (lambda (_node0 _v0)
         (vector-set! _node0 7 _v0))))
 
(define node-entry5-set! (<change>
      (lambda (node v)
         (vector-set! node 8 v))
      (lambda (_node0 _v0)
         (vector-set! _node0 8 _v0))))
 
(define node-entry6-set! (lambda (node v)
      (vector-set! node 9 v)))
 
(define node-mark-set! (lambda (node v)
      (vector-set! node 10 v)))
 
(define *sn* 0)
 
(define *rand* 21)
 
(define *count* 0)
 
(define *marker* #f)
 
(define *root* ())
 
(define snb (lambda ()
      (set! *sn* (+ 1 *sn*))
      *sn*))
 
(define seed (lambda ()
      (set! *rand* 21)
      *rand*))
 
(define traverse-random (lambda ()
      (set! *rand* (remainder (* *rand* 17) 251))
      *rand*))
 
(define traverse-remove (<change>
      (lambda (n q)
         (if (eq? (cdr (car q)) (car q))
            (let ((x (caar q)))
               (set-car! q ())
               x)
            (if (= n 0)
               (let ((x (caar q)))
                  (letrec ((__do_loop (lambda (p)
                                        (if (eq? (cdr p) (car q))
                                           (begin
                                              (set-cdr! p (cdr (car q)))
                                              (set-car! q p))
                                           (__do_loop (cdr p))))))
                     (__do_loop (car q)))
                  x)
               (letrec ((__do_loop (lambda (n q p)
                                     (if (= n 0)
                                        (let ((x (car q)))
                                           (set-cdr! q p)
                                           x)
                                        (__do_loop (- n 1) (cdr q) (cdr p))))))
                  (__do_loop n (car q) (cdr (car q)))))))
      (lambda (_n0 _q0)
         (if (eq? (cdr (car _q0)) (car _q0))
            (let ((_x0 (caar _q0)))
               (set-car! _q0 ())
               _x0)
            (if (= _n0 0)
               (let ((_x1 (caar _q0)))
                  (letrec ((___do_loop0 (lambda (_p0)
                                          (if (eq? (cdr _p0) (car _q0))
                                             (begin
                                                (set-cdr! _p0 (cdr (car _q0)))
                                                (set-car! _q0 _p0))
                                             (___do_loop0 (cdr _p0))))))
                     (___do_loop0 (car _q0)))
                  _x1)
               (letrec ((___do_loop1 (lambda (_n1 _q1 _p1)
                                       (if (= _n1 0)
                                          (let ((_x2 (car _q1)))
                                             (set-cdr! _q1 _p1)
                                             _x2)
                                          (___do_loop1 (- _n1 1) (cdr _q1) (cdr _p1))))))
                  (___do_loop1 _n0 (car _q0) (cdr (car _q0)))))))))
 
(define traverse-select (<change>
      (lambda (n q)
         (letrec ((__do_loop (lambda (n q)
                               (if (= n 0) (car q) (__do_loop (- n 1) (cdr q))))))
            (__do_loop n (car q))))
      (lambda (_n0 _q0)
         (letrec ((___do_loop0 (lambda (_n1 _q1)
                                 (if (= _n1 0)
                                    (car _q1)
                                    (___do_loop0 (- _n1 1) (cdr _q1))))))
            (___do_loop0 _n0 (car _q0))))))
 
(define add (lambda (a q)
      (if (null? q)
         (__toplevel_cons (let ((x (__toplevel_cons a ()))) (set-cdr! x x) x) ())
         (if (null? (car q))
            (let ((x (__toplevel_cons a ())))
               (set-cdr! x x)
               (set-car! q x)
               q)
            (begin
               (set-cdr! (car q) (__toplevel_cons a (__toplevel_append (cdr (car q)) ())))
               q)))))
 
(define create-structure (<change>
      (lambda (n)
         (let ((a (__toplevel_cons (make-node) ())))
            (letrec ((__do_loop (lambda (m p)
                                  (if (= m 0)
                                     (begin
                                        (set! a (__toplevel_cons (begin (set-cdr! p a) p) ()))
                                        (letrec ((__do_loop (lambda (unused used x y)
                                                              (if (null? (car unused))
                                                                 (find-root (traverse-select 0 used) n)
                                                                 (begin
                                                                    (set! x (traverse-remove (remainder (traverse-random) n) unused))
                                                                    (set! y (traverse-select (remainder (traverse-random) n) used))
                                                                    (add x used)
                                                                    (node-sons-set! y (__toplevel_cons x (__toplevel_append (node-sons y) ())))
                                                                    (node-parents-set! x (__toplevel_cons y (__toplevel_append (node-parents x) ())))
                                                                    (__do_loop unused used x y))))))
                                           (__do_loop a (add (traverse-remove 0 a) ()) () ())))
                                     (begin
                                        (set! a (cons (make-node) a))
                                        (__do_loop (- m 1) p))))))
               (__do_loop (- n 1) a))))
      (lambda (_n0)
         (let ((_a0 (__toplevel_cons (make-node) ())))
            (letrec ((___do_loop0 (lambda (_m0 _p0)
                                    (if (= _m0 0)
                                       (begin
                                          (set! _a0 (__toplevel_cons (begin (set-cdr! _p0 _a0) _p0) ()))
                                          (letrec ((___do_loop1 (lambda (_unused0 _used0 _x0 _y0)
                                                                  (if (null? (car _unused0))
                                                                     (find-root (traverse-select 0 _used0) _n0)
                                                                     (begin
                                                                        (set! _x0 (traverse-remove (remainder (traverse-random) _n0) _unused0))
                                                                        (set! _y0 (traverse-select (remainder (traverse-random) _n0) _used0))
                                                                        (add _x0 _used0)
                                                                        (node-sons-set! _y0 (__toplevel_cons _x0 (__toplevel_append (node-sons _y0) ())))
                                                                        (node-parents-set! _x0 (__toplevel_cons _y0 (__toplevel_append (node-parents _x0) ())))
                                                                        (___do_loop1 _unused0 _used0 _x0 _y0))))))
                                             (___do_loop1 _a0 (add (traverse-remove 0 _a0) ()) () ())))
                                       (begin
                                          (set! _a0 (cons (make-node) _a0))
                                          (___do_loop0 (- _m0 1) _p0))))))
               (___do_loop0 (- _n0 1) _a0))))))
 
(define find-root (<change>
      (lambda (node n)
         (letrec ((__do_loop (lambda (n)
                               (if (let ((__or_res (= n 0))) (if __or_res __or_res (null? (node-parents node))))
                                  node
                                  (begin
                                     (set! node (car (node-parents node)))
                                     (__do_loop (- n 1)))))))
            (__do_loop n)))
      (lambda (_node0 _n0)
         (letrec ((___do_loop0 (lambda (_n1)
                                 (if (let ((___or_res0 (= _n1 0))) (if ___or_res0 ___or_res0 (null? (node-parents _node0))))
                                    _node0
                                    (begin
                                       (set! _node0 (car (node-parents _node0)))
                                       (___do_loop0 (- _n1 1)))))))
            (___do_loop0 _n0)))))
 
(define travers (lambda (node mark)
      (if (eq? (node-mark node) mark)
         #f
         (begin
            (node-mark-set! node mark)
            (set! *count* (+ 1 *count*))
            (node-entry1-set! node (not (node-entry1 node)))
            (node-entry2-set! node (not (node-entry2 node)))
            (node-entry3-set! node (not (node-entry3 node)))
            (node-entry4-set! node (not (node-entry4 node)))
            (node-entry5-set! node (not (node-entry5 node)))
            (node-entry6-set! node (not (node-entry6 node)))
            (letrec ((__do_loop (lambda (sons)
                                  (if (null? sons)
                                     #f
                                     (begin
                                        (travers (car sons) mark)
                                        (__do_loop (cdr sons)))))))
               (__do_loop (node-sons node)))))))
 
(define traverse (lambda (root)
      (let ((*count* 0))
         (travers root (begin (set! *marker* (not *marker*)) *marker*))
         *count*)))
 
(define init-traverse (lambda ()
      (set! *root* (create-structure 100))
      #f))
 
(define run-traverse (<change>
      (lambda ()
         (letrec ((__do_loop (lambda (i)
                               (if (= i 0)
                                  #f
                                  (begin
                                     (traverse *root*)
                                     (traverse *root*)
                                     (traverse *root*)
                                     (traverse *root*)
                                     (traverse *root*)
                                     (__do_loop (- i 1)))))))
            (__do_loop 50)))
      (lambda ()
         (letrec ((___do_loop0 (lambda (_i0)
                                 (if (= _i0 0)
                                    #f
                                    (begin
                                       (traverse *root*)
                                       (traverse *root*)
                                       (traverse *root*)
                                       (traverse *root*)
                                       (traverse *root*)
                                       (___do_loop0 (- _i0 1)))))))
            (___do_loop0 50)))))
 
(init-traverse)
 
(run-traverse)
 
#t
 
