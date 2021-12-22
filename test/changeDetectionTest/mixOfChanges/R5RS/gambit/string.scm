;; renamed lambdas/lets: 2
;; Of which consistent renamings: 1
 
(define s "abcdef")
 
(define grow (lambda ()
      (set! s (string-append "123" s "456" s "789"))
      (set! s (string-append
              (substring s (quotient (string-length s) 2) (string-length s))
              (substring s 0 (+ 1 (quotient (string-length s) 2)))))
      s))
 
(define trial (<change>
      (lambda (n)
         (letrec ((__do_loop (lambda (i)
                               (if (> (string-length s) n)
                                  (string-length s)
                                  (begin
                                     (grow)
                                     (__do_loop (+ i 1)))))))
            (__do_loop 0)))
      (lambda (_n0)
         (letrec ((___do_loop0 (lambda (_i0)
                                 (if (> (string-length s) _n0)
                                    (string-length s)
                                    (begin
                                       (grow)
                                       (___do_loop0 (+ _i0 1)))))))
            (___do_loop0 0)))))
 
(define my-try (<change>
      (lambda (n)
         (letrec ((__do_loop (lambda (i)
                               (if (>= i 10)
                                  (string-length s)
                                  (begin
                                     (set! s "abcdef")
                                     (trial n)
                                     (__do_loop (+ i 1)))))))
            (__do_loop 0)))
      (lambda (_n0)
         (letrec ((___do_loop0 (lambda (_i0)
                                 (if (>= _i0 10)
                                    (string-length s)
                                    (begin
                                       (set! s "abcdef")
                                       (trial _n0)
                                       (___do_loop0 (+ _i0 1)))))))
            (___do_loop0 1))))) ;; NOT RENAMING 0 -> 1
 
(= (my-try 500000) 524278)
 
