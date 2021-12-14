;; renamed lambdas/lets: 6
 
(define copy (<change>
      (lambda (from-vector to-vector from-index to-index)
         (vector-set! to-vector to-index (vector-ref from-vector from-index)))
      (lambda (_from-vector0 _to-vector0 _from-index0 _to-index0)
         (vector-set! _to-vector0 _to-index0 (vector-ref _from-vector0 _from-index0)))))
 
(define move (<change>
      (lambda (from-vector to-vector from-low from-high to-index)
         (define move-iter (lambda (n)
               (if (<= (+ from-low n) from-high)
                  (begin
                     (copy from-vector to-vector (+ from-low n) (+ to-index n))
                     (move-iter (+ n 1)))
                  #f)))
         (move-iter 0))
      (lambda (_from-vector0 _to-vector0 _from-low0 _from-high0 _to-index0)
         (define move-iter (lambda (_n0)
               (if (<= (+ _from-low0 _n0) _from-high0)
                  (begin
                     (copy _from-vector0 _to-vector0 (+ _from-low0 _n0) (+ _to-index0 _n0))
                     (move-iter (+ _n0 1)))
                  #f)))
         (move-iter 0))))
 
(define merge (<change>
      (lambda (vector1 vector2 vector low1 high1 low2 high2 to-index)
         (define merge-iter (lambda (index index1 index2)
               (if (> index1 high1)
                  (move vector2 vector index2 high2 index)
                  (if (> index2 high2)
                     (move vector1 vector index1 high1 index)
                     (if (< (vector-ref vector1 index1) (vector-ref vector2 index2))
                        (begin
                           (copy vector1 vector index1 index)
                           (merge-iter (+ index 1) (+ index1 1) index2))
                        (begin
                           (copy vector2 vector index2 index)
                           (merge-iter (+ index 1) index1 (+ index2 1))))))))
         (merge-iter to-index low1 low2))
      (lambda (_vector10 _vector20 _vector0 _low10 _high10 _low20 _high20 _to-index0)
         (define merge-iter (lambda (_index0 _index10 _index20)
               (if (> _index10 _high10)
                  (move _vector20 _vector0 _index20 _high20 _index0)
                  (if (> _index20 _high20)
                     (move _vector10 _vector0 _index10 _high10 _index0)
                     (if (< (vector-ref _vector10 _index10) (vector-ref _vector20 _index20))
                        (begin
                           (copy _vector10 _vector0 _index10 _index0)
                           (merge-iter (+ _index0 1) (+ _index10 1) _index20))
                        (begin
                           (copy _vector20 _vector0 _index20 _index0)
                           (merge-iter (+ _index0 1) _index10 (+ _index20 1))))))))
         (merge-iter _to-index0 _low10 _low20))))
 
(define bottom-up-merge-sort (lambda (vector)
      (define merge-subs (<change>
            (lambda (len)
               (let ((aux-vector (make-vector (vector-length vector) 0)))
                  (define merge-subs-iter (lambda (index)
                        (if (< index (- (vector-length vector) (* 2 len)))
                           (begin
                              (merge vector vector aux-vector index (+ index len -1) (+ index len) (+ index len len -1) index)
                              (move aux-vector vector index (+ index len len -1) index)
                              (merge-subs-iter (+ index len len)))
                           (if (< index (- (vector-length vector) len))
                              (begin
                                 (merge
                                    vector
                                    vector
                                    aux-vector
                                    index
                                    (+ index len -1)
                                    (+ index len)
                                    (- (vector-length vector) 1)
                                    index)
                                 (move aux-vector vector index (- (vector-length vector) 1) index))
                              #f))))
                  (merge-subs-iter 0)))
            (lambda (_len0)
               (let ((_aux-vector0 (make-vector (vector-length vector) 0)))
                  (define merge-subs-iter (lambda (_index0)
                        (if (< _index0 (- (vector-length vector) (* 2 _len0)))
                           (begin
                              (merge
                                 vector
                                 vector
                                 _aux-vector0
                                 _index0
                                 (+ _index0 _len0 -1)
                                 (+ _index0 _len0)
                                 (+ _index0 _len0 _len0 -1)
                                 _index0)
                              (move _aux-vector0 vector _index0 (+ _index0 _len0 _len0 -1) _index0)
                              (merge-subs-iter (+ _index0 _len0 _len0)))
                           (if (< _index0 (- (vector-length vector) _len0))
                              (begin
                                 (merge
                                    vector
                                    vector
                                    _aux-vector0
                                    _index0
                                    (+ _index0 _len0 -1)
                                    (+ _index0 _len0)
                                    (- (vector-length vector) 1)
                                    _index0)
                                 (move _aux-vector0 vector _index0 (- (vector-length vector) 1) _index0))
                              #f))))
                  (merge-subs-iter 0)))))
      (define merge-sort-iter (<change>
            (lambda (len)
               (if (< len (vector-length vector))
                  (begin
                     (merge-subs len)
                     (merge-sort-iter (* 2 len)))
                  #f))
            (lambda (_len0)
               (if (< _len0 (vector-length vector))
                  (begin
                     (merge-subs _len0)
                     (merge-sort-iter (* 2 _len0)))
                  #f))))
      (merge-sort-iter 1)))
 
(<change>
   (let ((aVector (vector 8 3 6 6 0 5 4 2 9 6)))
      (bottom-up-merge-sort aVector)
      (equal? aVector (vector 0 2 3 4 5 6 6 6 8 9)))
   (let ((_aVector0 (vector 8 3 6 6 0 5 4 2 9 6)))
      (bottom-up-merge-sort _aVector0)
      (equal? _aVector0 (vector 0 2 3 4 5 6 6 6 8 9))))
 
