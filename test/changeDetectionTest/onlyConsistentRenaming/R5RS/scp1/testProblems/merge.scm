;; renamed lambdas/lets: 4

(define first-el (<change>
      (lambda (best)
         (if (not (null? best)) (caar best) #f))
      (lambda (_best0)
         (if (not (null? _best0)) (caar _best0) #f))))

(define smaller? (<change>
      (lambda (el1 el2)
         (string<? (symbol->string el1) (symbol->string el2)))
      (lambda (_el10 _el20)
         (string<? (symbol->string _el10) (symbol->string _el20)))))

(define same? (<change>
      (lambda (el1 el2)
         (equal? el1 el2))
      (lambda (_el10 _el20)
         (equal? _el10 _el20))))

(define merge (<change>
      (lambda (best1 best2)
         (define merge-in (lambda (curr1 curr2 prev)
               (if (null? curr1)
                  (set-cdr! prev curr2)
                  (if (null? curr2)
                     (set-cdr! prev curr1)
                     (if (same? (first-el curr1) (first-el curr2))
                        (begin
                           (set-cdr! prev curr1)
                           (merge-in (cdr curr1) (cdr curr2) curr1))
                        (if (smaller? (first-el curr1) (first-el curr2))
                           (begin
                              (set-cdr! prev curr1)
                              (merge-in (cdr curr1) curr2 curr1))
                           (begin
                              (set-cdr! prev curr2)
                              (merge-in curr1 (cdr curr2) curr2))))))))
         (let* ((result (if (smaller? (first-el best1) (first-el best2))
                          best1
                          best2))
                (curr1 (if (eq? result best1) (cdr best1) best1))
                (curr2 (if (eq? result best2) (cdr best2) best2)))
            (merge-in curr1 curr2 result)
            result))
      (lambda (_best10 _best20)
         (define merge-in (lambda (_curr10 _curr20 _prev0)
               (if (null? _curr10)
                  (set-cdr! _prev0 _curr20)
                  (if (null? _curr20)
                     (set-cdr! _prev0 _curr10)
                     (if (same? (first-el _curr10) (first-el _curr20))
                        (begin
                           (set-cdr! _prev0 _curr10)
                           (merge-in (cdr _curr10) (cdr _curr20) _curr10))
                        (if (smaller? (first-el _curr10) (first-el _curr20))
                           (begin
                              (set-cdr! _prev0 _curr10)
                              (merge-in (cdr _curr10) _curr20 _curr10))
                           (begin
                              (set-cdr! _prev0 _curr20)
                              (merge-in _curr10 (cdr _curr20) _curr20))))))))
         (let* ((_result0 (if (smaller? (first-el _best10) (first-el _best20))
                            _best10
                            _best20))
                (_curr11 (if (eq? _result0 _best10) (cdr _best10) _best10))
                (_curr21 (if (eq? _result0 _best20) (cdr _best20) _best20)))
            (merge-in _curr11 _curr21 _result0)
            _result0))))

(define best1 '((ann (meiboomstraat 12 1820 Eppegem))
                (bert (populierendreef 7 1050 Brussel))
                (kurt (Mechelsesteenweg 50 1800 Vilvoorde))))

(define best2 '((bert (populierendreef 7 1050 Brussel))
                (jan (eikestraat 1 9000 Gent))
                (sofie (boerendreef 5  2800 Mechelen))))

(equal? (merge best1 best2)
        '((ann (meiboomstraat 12 1820 Eppegem)) (bert (populierendreef 7 1050 Brussel)) (jan (eikestraat 1 9000 Gent)) (kurt (Mechelsesteenweg 50 1800 Vilvoorde)) (sofie (boerendreef 5 2800 Mechelen))))