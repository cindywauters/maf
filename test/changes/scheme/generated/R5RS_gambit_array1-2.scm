; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 1
(letrec ((create-x (lambda (n)
                     (letrec ((result (make-vector n 0)))
                        (letrec ((__do_loop (lambda (i)
                                              (if (<change> (>= i n) (not (>= i n)))
                                                 result
                                                 (begin
                                                    (vector-set! result i i)
                                                    (__do_loop (+ i 1)))))))
                           (__do_loop 0)))))
         (create-y (lambda (x)
                     (let* ((n (vector-length x))
                            (result (make-vector n 0)))
                        (letrec ((__do_loop (lambda (i)
                                              (if (< i 0)
                                                 result
                                                 (begin
                                                    (vector-set! result i (vector-ref x i))
                                                    (__do_loop (- i 1)))))))
                           (__do_loop (- n 1))))))
         (my-try (lambda (n)
                   (vector-length (create-y (create-x n)))))
         (go (lambda (n)
               ((letrec ((loop (lambda (repeat result)
                                (if (> repeat 0)
                                   (loop (- repeat 1) (my-try n))
                                   result))))
                  loop)
                  100
                  ()))))
   (<change>
      ()
      (= 200 (go 200)))
   (<change>
      ()
      (display 200))
   (= 200 (go 200)))