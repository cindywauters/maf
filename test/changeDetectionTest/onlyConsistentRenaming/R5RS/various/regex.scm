;; renamed lambdas/lets: 9
 
(define debug-trace (<change>
      (lambda ()
         'do-nothing)
      (lambda ()
         'do-nothing)))
 
(define regex-NULL #f)
 
(define regex-BLANK #t)
 
(define regex-alt? (lambda (re)
      (if (pair? re) (eq? (car re) 'alt) #f)))
 
(define regex-seq? (<change>
      (lambda (re)
         (if (pair? re) (eq? (car re) 'seq) #f))
      (lambda (_re0)
         (if (pair? _re0) (eq? (car _re0) 'seq) #f))))
 
(define regex-rep? (lambda (re)
      (if (pair? re) (eq? (car re) 'rep) #f)))
 
(define regex-null? (lambda (re)
      (eq? re #f)))
 
(define regex-empty? (lambda (re)
      (eq? re #t)))
 
(define regex-atom? (<change>
      (lambda (re)
         (let ((__or_res (char? re)))
            (if __or_res __or_res (symbol? re))))
      (lambda (_re0)
         (let ((___or_res0 (char? _re0)))
            (if ___or_res0 ___or_res0 (symbol? _re0))))))
 
(define match-seq (<change>
      (lambda (re f)
         (if (regex-seq? re) (f (cadr re) (caddr re)) #f))
      (lambda (_re0 _f0)
         (if (regex-seq? _re0)
            (_f0 (cadr _re0) (caddr _re0))
            #f))))
 
(define match-alt (lambda (re f)
      (if (regex-alt? re) (f (cadr re) (caddr re)) #f)))
 
(define match-rep (lambda (re f)
      (if (regex-rep? re) (f (cadr re)) #f)))
 
(define seq (<change>
      (lambda (pat1 pat2)
         (if (regex-null? pat1)
            regex-NULL
            (if (regex-null? pat2)
               regex-NULL
               (if (regex-empty? pat1)
                  pat2
                  (if (regex-empty? pat2)
                     pat1
                     (cons 'seq (cons pat1 (cons pat2 ()))))))))
      (lambda (_pat10 _pat20)
         (if (regex-null? _pat10)
            regex-NULL
            (if (regex-null? _pat20)
               regex-NULL
               (if (regex-empty? _pat10)
                  _pat20
                  (if (regex-empty? _pat20)
                     _pat10
                     (cons 'seq (cons _pat10 (cons _pat20 ()))))))))))
 
(define alt (lambda (pat1 pat2)
      (if (regex-null? pat1)
         pat2
         (if (regex-null? pat2)
            pat1
            (cons 'alt (cons pat1 (cons pat2 ())))))))
 
(define rep (<change>
      (lambda (pat)
         (if (regex-null? pat)
            regex-BLANK
            (if (regex-empty? pat)
               regex-BLANK
               (cons 'rep (cons pat ())))))
      (lambda (_pat0)
         (if (regex-null? _pat0)
            regex-BLANK
            (if (regex-empty? _pat0)
               regex-BLANK
               (cons 'rep (cons _pat0 ())))))))
 
(define regex-empty (<change>
      (lambda (re)
         (if (regex-empty? re)
            #t
            (if (regex-null? re)
               #f
               (if (regex-atom? re)
                  #f
                  (let ((__cond-empty-body (match-seq re (lambda (pat1 pat2) (seq (regex-empty pat1) (regex-empty pat2))))))
                     (if __cond-empty-body
                        __cond-empty-body
                        (let ((__cond-empty-body (match-alt re (lambda (pat1 pat2) (alt (regex-empty pat1) (regex-empty pat2))))))
                           (if __cond-empty-body
                              __cond-empty-body
                              (if (regex-rep? re) #t #f)))))))))
      (lambda (_re0)
         (if (regex-empty? _re0)
            #t
            (if (regex-null? _re0)
               #f
               (if (regex-atom? _re0)
                  #f
                  (let ((___cond-empty-body0 (match-seq _re0 (lambda (_pat10 _pat20) (seq (regex-empty _pat10) (regex-empty _pat20))))))
                     (if ___cond-empty-body0
                        ___cond-empty-body0
                        (let ((___cond-empty-body1 (match-alt _re0 (lambda (_pat11 _pat21) (alt (regex-empty _pat11) (regex-empty _pat21))))))
                           (if ___cond-empty-body1
                              ___cond-empty-body1
                              (if (regex-rep? _re0) #t #f)))))))))))
 
(define regex-derivative (<change>
      (lambda (re c)
         (debug-trace)
         (if (regex-empty? re)
            regex-NULL
            (if (regex-null? re)
               regex-NULL
               (if (eq? c re)
                  regex-BLANK
                  (if (regex-atom? re)
                     regex-NULL
                     (let ((__cond-empty-body (match-seq
                                                re
                                                (lambda (pat1 pat2)
                                                   (alt (seq (d/dc pat1 c) pat2) (seq (regex-empty pat1) (d/dc pat2 c)))))))
                        (if __cond-empty-body
                           __cond-empty-body
                           (let ((__cond-empty-body (match-alt re (lambda (pat1 pat2) (alt (d/dc pat1 c) (d/dc pat2 c))))))
                              (if __cond-empty-body
                                 __cond-empty-body
                                 (let ((__cond-empty-body (match-rep re (lambda (pat) (seq (d/dc pat c) (rep pat))))))
                                    (if __cond-empty-body
                                       __cond-empty-body
                                       regex-NULL)))))))))))
      (lambda (_re0 _c0)
         (debug-trace)
         (if (regex-empty? _re0)
            regex-NULL
            (if (regex-null? _re0)
               regex-NULL
               (if (eq? _c0 _re0)
                  regex-BLANK
                  (if (regex-atom? _re0)
                     regex-NULL
                     (let ((___cond-empty-body0 (match-seq
                                                  _re0
                                                  (lambda (_pat10 _pat20)
                                                     (alt (seq (d/dc _pat10 _c0) _pat20) (seq (regex-empty _pat10) (d/dc _pat20 _c0)))))))
                        (if ___cond-empty-body0
                           ___cond-empty-body0
                           (let ((___cond-empty-body1 (match-alt _re0 (lambda (_pat11 _pat21) (alt (d/dc _pat11 _c0) (d/dc _pat21 _c0))))))
                              (if ___cond-empty-body1
                                 ___cond-empty-body1
                                 (let ((___cond-empty-body2 (match-rep _re0 (lambda (_pat0) (seq (d/dc _pat0 _c0) (rep _pat0))))))
                                    (if ___cond-empty-body2
                                       ___cond-empty-body2
                                       regex-NULL)))))))))))))
 
(define d/dc regex-derivative)
 
(define regex-match (<change>
      (lambda (pattern data)
         (if (null? data)
            (regex-empty? (regex-empty pattern))
            (regex-match (d/dc pattern (car data)) (cdr data))))
      (lambda (_pattern0 _data0)
         (if (null? _data0)
            (regex-empty? (regex-empty _pattern0))
            (regex-match (d/dc _pattern0 (car _data0)) (cdr _data0))))))
 
(define check-expect (lambda (check expect)
      (equal? check expect)))
 
(define res (check-expect
      (regex-match
         (__toplevel_cons
            'seq
            (__toplevel_cons 'foo (__toplevel_cons (__toplevel_cons 'rep (__toplevel_cons 'bar ())) ())))
         (__toplevel_cons 'foo (__toplevel_cons 'bar ())))
      #t))
 
res
 
