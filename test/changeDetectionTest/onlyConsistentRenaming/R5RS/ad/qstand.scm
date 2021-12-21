;; renamed lambdas/lets: 1
 
(define quick-sort (<change>
      (lambda (vector)
         (define swap (lambda (vector index1 index2)
               (let ((temp (vector-ref vector index1)))
                  (vector-set! vector index1 (vector-ref vector index2))
                  (vector-set! vector index2 temp))))
         (define quick-sort-aux (lambda (low high)
               (define quick-sort-aux-iter (lambda (mid-value from to)
                     (define quick-right (lambda (index1)
                           (if (< (vector-ref vector index1) mid-value)
                              (quick-right (+ index1 1))
                              index1)))
                     (define quick-left (lambda (index2)
                           (if (> (vector-ref vector index2) mid-value)
                              (quick-left (- index2 1))
                              index2)))
                     (let ((index1 (quick-right (+ from 1)))
                           (index2 (quick-left to)))
                        (if (< index1 index2)
                           (begin
                              (swap vector index1 index2)
                              (quick-sort-aux-iter mid-value index1 index2))
                           index2))))
               (if (< low high)
                  (begin
                     (if (> (vector-ref vector low) (vector-ref vector high))
                        (swap vector low high)
                        #f)
                     (let ((mid-index (quick-sort-aux-iter (vector-ref vector low) low high)))
                        (swap vector mid-index low)
                        (quick-sort-aux low (- mid-index 1))
                        (quick-sort-aux (+ mid-index 1) high)))
                  #f)))
         (quick-sort-aux 0 (- (vector-length vector) 1)))
      (lambda (_vector0)
         (define swap (lambda (_vector1 _index10 _index20)
               (let ((_temp0 (vector-ref _vector1 _index10)))
                  (vector-set! _vector1 _index10 (vector-ref _vector1 _index20))
                  (vector-set! _vector1 _index20 _temp0))))
         (define quick-sort-aux (lambda (_low0 _high0)
               (define quick-sort-aux-iter (lambda (_mid-value0 _from0 _to0)
                     (define quick-right (lambda (_index11)
                           (if (< (vector-ref _vector0 _index11) _mid-value0)
                              (quick-right (+ _index11 1))
                              _index11)))
                     (define quick-left (lambda (_index21)
                           (if (> (vector-ref _vector0 _index21) _mid-value0)
                              (quick-left (- _index21 1))
                              _index21)))
                     (let ((_index12 (quick-right (+ _from0 1)))
                           (_index22 (quick-left _to0)))
                        (if (< _index12 _index22)
                           (begin
                              (swap _vector0 _index12 _index22)
                              (quick-sort-aux-iter _mid-value0 _index12 _index22))
                           _index22))))
               (if (< _low0 _high0)
                  (begin
                     (if (> (vector-ref _vector0 _low0) (vector-ref _vector0 _high0))
                        (swap _vector0 _low0 _high0)
                        #f)
                     (let ((_mid-index0 (quick-sort-aux-iter (vector-ref _vector0 _low0) _low0 _high0)))
                        (swap _vector0 _mid-index0 _low0)
                        (quick-sort-aux _low0 (- _mid-index0 1))
                        (quick-sort-aux (+ _mid-index0 1) _high0)))
                  #f)))
         (quick-sort-aux 0 (- (vector-length _vector0) 1)))))
 
(define test1 (vector 7 2 4 6 0 8 5 3 1))
 
(quick-sort test1)
 
(define test2 (vector 8 1 4 9 6 3 5 2 7 0))
 
(quick-sort test2)
 
(define test3 (vector 8 3 6 6 1 5 4 2 9 6))
 
(quick-sort test3)
 
(if (equal? test1 (vector 0 1 2 3 4 5 6 7 8))
   (if (equal? test2 (vector 0 1 2 3 4 5 6 7 8 9))
      (equal? test3 (vector 1 2 3 4 5 6 6 6 8 9))
      #f)
   #f)
 
