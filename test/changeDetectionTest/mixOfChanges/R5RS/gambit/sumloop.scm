;; renamed lambdas/lets: 3
;; Of which consistent renamings: 2

(define sum 0)
 
(define tail-rec-aux (<change>
      (lambda (i n)
         (if (< i n)
            (begin
               (set! sum (+ sum 1))
               (tail-rec-aux (+ i 1) n))
            sum))
      (lambda (_i0 _n0)
         (if (< _i0 _n0)
            (begin
               (set! sum (+ sum 2)) ;; NOT RENAMING: 1 -> 2
               (tail-rec-aux (+ _i0 1) _n0))
            sum))))
 
(define tail-rec-loop (<change>
      (lambda (n)
         (set! sum 0)
         (tail-rec-aux 0 n)
         sum)
      (lambda (_n0)
         (set! sum 0)
         (tail-rec-aux 0 _n0)
         sum)))
 
(define do-loop (lambda (n)
      (set! sum 0)
      (<change>
         (letrec ((__do_loop (lambda (i)
                               (if (>= i n)
                                  sum
                                  (begin
                                     (set! sum (+ sum 1))
                                     (__do_loop (+ i 1)))))))
            (__do_loop 0))
         (letrec ((___do_loop0 (lambda (_i0)
                                 (if (>= _i0 n)
                                    sum
                                    (begin
                                       (set! sum (+ sum 1))
                                       (___do_loop0 (+ _i0 1)))))))
            (___do_loop0 0)))))
 
(= (do-loop 1000) 1000)
 
