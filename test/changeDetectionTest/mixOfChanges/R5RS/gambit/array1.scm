(define create-x (<change>
 (lambda (n)
  (define result (make-vector n 0))
  (do ((i 0 (+ i 1)))
      ((>= i n) result)
    (vector-set! result i i)))
  (lambda (n1)
      (define result (make-vector n1 0))
      (do ((i 0 (+ i 1)))
          ((>= i n1) result)
        (vector-set! result i i)))))


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

(define my-try (<change>
 (lambda (n)
  (vector-length (create-y (create-x n))))
  (lambda (n1)
   (vector-length (create-y (create-x n1))))))

(define (go n)
 (<change>
  (let loop ((repeat 100)
             (result '()))
    (if (> repeat 0)
        (loop (- repeat 1) (my-try n))
        result))
  (let loop0 ((repeat 100)
             (result '()))
    (if (> repeat 0)
        (loop0 (- repeat 1) (my-try n))
        result))))

(= 200 (go 200))