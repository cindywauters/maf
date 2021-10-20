;; check this, doesn't properly run in scheme

(define (fac n)
  (<change>
   (letrec ((loop (lambda (number counter)
                 (if (> counter n)
                     number
                     (loop (* number counter) (+ counter 1))))))
     (loop 1 1))
   (letrec ((iteration (lambda (number counter)
                      (if (= counter ( + 1 n))
                          number
                          (iteration (* number counter) (+ counter 1))))))
     (iteration 1 1))))
(fac 5)