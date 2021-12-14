;; renamed lambdas/lets: 2
 
(define selection-sort (lambda (vector)
      (define swap (lambda (vector index1 index2)
            (<change>
               (let ((temp (vector-ref vector index1)))
                  (vector-set! vector index1 (vector-ref vector index2))
                  (vector-set! vector index2 temp))
               (let ((_temp0 (vector-ref vector index1)))
                  (vector-set! vector index1 (vector-ref vector index2))
                  (vector-set! vector index2 _temp0)))))
      (define pos-of-min (lambda (vector low high)
            (define min-iter (lambda (index pos-of-min-so-far)
                  (if (<= index high)
                     (if (< (vector-ref vector index) (vector-ref vector pos-of-min-so-far))
                        (min-iter (+ index 1) index)
                        (min-iter (+ index 1) pos-of-min-so-far))
                     pos-of-min-so-far)))
            (min-iter (+ low 1) low)))
      (let ((high (- (vector-length vector) 1)))
         (define selection-sort-iter (<change>
               (lambda (index)
                  (if (< index high)
                     (begin
                        (swap vector index (pos-of-min vector index high))
                        (selection-sort-iter (+ index 1)))
                     #f))
               (lambda (_index0)
                  (if (< _index0 high)
                     (begin
                        (swap vector _index0 (pos-of-min vector _index0 high))
                        (selection-sort-iter (+ _index0 1)))
                     #f))))
         (selection-sort-iter 0))))
 
(define vect (vector 5 7 0 9 6 4 3 8 2 1))
 
(selection-sort vect)
 
(equal? vect (vector 0 1 2 3 4 5 6 7 8 9))
 
