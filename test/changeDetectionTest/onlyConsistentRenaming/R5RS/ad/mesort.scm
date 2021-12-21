;; renamed lambdas/lets: 2
 
(define copy (lambda (from-vector to-vector from-index to-index)
      (vector-set! to-vector to-index (vector-ref from-vector from-index))))
 
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
 
(define merge (lambda (vector1 vector2 vector low1 high1 low2 high2 to-index)
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
      (merge-iter to-index low1 low2)))
 
(define bottom-up-merge-sort (lambda (vector)
      (define merge-subs (lambda (len)
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
               (merge-subs-iter 0))))
      (define merge-sort-iter (lambda (len)
            (if (< len (vector-length vector))
               (begin
                  (merge-subs len)
                  (merge-sort-iter (* 2 len)))
               #f)))
      (merge-sort-iter 1)))
 
(<change>
   (let ((aVector (vector 8 3 6 6 0 5 4 2 9 6)))
      (bottom-up-merge-sort aVector)
      (equal? aVector (vector 0 2 3 4 5 6 6 6 8 9)))
   (let ((_aVector0 (vector 8 3 6 6 0 5 4 2 9 6)))
      (bottom-up-merge-sort _aVector0)
      (equal? _aVector0 (vector 0 2 3 4 5 6 6 6 8 9))))
 
