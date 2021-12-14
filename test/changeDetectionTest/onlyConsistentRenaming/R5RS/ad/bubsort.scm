;; renamed lambdas/lets: 1
 
(define bubble-sort (<change>
      (lambda (vector)
         (define swap (lambda (vector index1 index2)
               (let ((temp (vector-ref vector index1)))
                  (vector-set! vector index1 (vector-ref vector index2))
                  (vector-set! vector index2 temp))))
         (define bubble (lambda (index)
               (define bubble-iter (lambda (index1 changed)
                     (if (<= index1 index)
                        (begin
                           (if (> (vector-ref vector index1) (vector-ref vector (+ index1 1)))
                              (begin
                                 (swap vector index1 (+ index1 1))
                                 (set! changed #t))
                              #f)
                           (bubble-iter (+ index1 1) changed))
                        changed)))
               (bubble-iter 0 #f)))
         (define bubble-sort-iter (lambda (index)
               (if (>= index 0)
                  (if (bubble index)
                     (bubble-sort-iter (- index 1))
                     #f)
                  #f)))
         (bubble-sort-iter (- (vector-length vector) 2)))
      (lambda (_vector0)
         (define swap (lambda (_vector1 _index10 _index20)
               (let ((_temp0 (vector-ref _vector1 _index10)))
                  (vector-set! _vector1 _index10 (vector-ref _vector1 _index20))
                  (vector-set! _vector1 _index20 _temp0))))
         (define bubble (lambda (_index0)
               (define bubble-iter (lambda (_index11 _changed0)
                     (if (<= _index11 _index0)
                        (begin
                           (if (> (vector-ref _vector0 _index11) (vector-ref _vector0 (+ _index11 1)))
                              (begin
                                 (swap _vector0 _index11 (+ _index11 1))
                                 (set! _changed0 #t))
                              #f)
                           (bubble-iter (+ _index11 1) _changed0))
                        _changed0)))
               (bubble-iter 0 #f)))
         (define bubble-sort-iter (lambda (_index1)
               (if (>= _index1 0)
                  (if (bubble _index1)
                     (bubble-sort-iter (- _index1 1))
                     #f)
                  #f)))
         (bubble-sort-iter (- (vector-length _vector0) 2)))))
 
(define vect (vector 9 5 1 7 8 9 4 6 2 3))
 
(bubble-sort vect)
 
(equal? vect (vector 1 2 3 4 5 6 7 8 9 9))
 
