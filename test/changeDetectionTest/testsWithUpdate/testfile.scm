(define (create-y x)
  (<change>
  (let* ((n (vector-length x))
         (result (make-vector n 0)))
    (do ((i (- n 1) (- i 1)))
        ((< i 0) result)
      (vector-set! result i (vector-ref x i))))
  (let* ((n1 (vector-length x))
               (result (make-vector n1 0)))
          (do ((i (- n1 1) (- i 2))) ;; NOT RENAMING
              ((< i 0) result)
            (vector-set! result i (vector-ref x i))))))