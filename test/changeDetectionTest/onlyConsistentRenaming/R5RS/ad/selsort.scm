;; renamed lambdas/lets: 1
 
(define selection-sort (<change>
      (lambda (vector)
         (define swap (lambda (vector index1 index2)
               (let ((temp (vector-ref vector index1)))
                  (vector-set! vector index1 (vector-ref vector index2))
                  (vector-set! vector index2 temp))))
         (define pos-of-min (lambda (vector low high)
               (define min-iter (lambda (index pos-of-min-so-far)
                     (if (<= index high)
                        (if (< (vector-ref vector index) (vector-ref vector pos-of-min-so-far))
                           (min-iter (+ index 1) index)
                           (min-iter (+ index 1) pos-of-min-so-far))
                        pos-of-min-so-far)))
               (min-iter (+ low 1) low)))
         (let ((high (- (vector-length vector) 1)))
            (define selection-sort-iter (lambda (index)
                  (if (< index high)
                     (begin
                        (swap vector index (pos-of-min vector index high))
                        (selection-sort-iter (+ index 1)))
                     #f)))
            (selection-sort-iter 0)))
      (lambda (_vector0)
         (define swap (lambda (_vector1 _index10 _index20)
               (let ((_temp0 (vector-ref _vector1 _index10)))
                  (vector-set! _vector1 _index10 (vector-ref _vector1 _index20))
                  (vector-set! _vector1 _index20 _temp0))))
         (define pos-of-min (lambda (_vector2 _low0 _high0)
               (define min-iter (lambda (_index0 _pos-of-min-so-far0)
                     (if (<= _index0 _high0)
                        (if (< (vector-ref _vector2 _index0) (vector-ref _vector2 _pos-of-min-so-far0))
                           (min-iter (+ _index0 1) _index0)
                           (min-iter (+ _index0 1) _pos-of-min-so-far0))
                        _pos-of-min-so-far0)))
               (min-iter (+ _low0 1) _low0)))
         (let ((_high1 (- (vector-length _vector0) 1)))
            (define selection-sort-iter (lambda (_index1)
                  (if (< _index1 _high1)
                     (begin
                        (swap _vector0 _index1 (pos-of-min _vector0 _index1 _high1))
                        (selection-sort-iter (+ _index1 1)))
                     #f)))
            (selection-sort-iter 0)))))
 
(define vect (vector 5 7 0 9 6 4 3 8 2 1))
 
(selection-sort vect)
 
(equal? vect (vector 0 1 2 3 4 5 6 7 8 9))
 
