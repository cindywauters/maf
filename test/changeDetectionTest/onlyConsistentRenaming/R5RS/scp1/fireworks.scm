;; renamed lambdas/lets: 3
 
(define atom? (lambda (x)
      (not (pair? x))))
 
(define mijn-vuurwerk (__toplevel_cons
      'groen
      (__toplevel_cons
         (__toplevel_cons
            (__toplevel_cons
               'blauw
               (__toplevel_cons
                  (__toplevel_cons
                     'X
                     (__toplevel_cons
                        (__toplevel_cons 'blauw (__toplevel_cons (__toplevel_cons 'X (__toplevel_cons 'X ())) ()))
                        (__toplevel_cons 'X (__toplevel_cons 'X ()))))
                  ()))
            (__toplevel_cons
               (__toplevel_cons
                  'rood
                  (__toplevel_cons
                     (__toplevel_cons
                        (__toplevel_cons 'groen (__toplevel_cons (__toplevel_cons 'X (__toplevel_cons 'X ())) ()))
                        (__toplevel_cons 'X ()))
                     ()))
               (__toplevel_cons
                  'X
                  (__toplevel_cons
                     (__toplevel_cons 'geel (__toplevel_cons (__toplevel_cons 'X (__toplevel_cons 'X ())) ()))
                     ()))))
         ())))
 
(define kleur (lambda (vuurwerk)
      (car vuurwerk)))
 
(define takken (lambda (vuurwerk)
      (cadr vuurwerk)))
 
(define low-energy? (<change>
      (lambda (vuurwerk)
         (eq? vuurwerk 'X))
      (lambda (_vuurwerk0)
         (eq? _vuurwerk0 'X))))
 
(define tel-knallen (<change>
      (lambda (vuurwerk)
         (if (null? vuurwerk)
            0
            (if (low-energy? vuurwerk)
               0
               (if (atom? vuurwerk)
                  1
                  (+ (tel-knallen (car vuurwerk)) (tel-knallen (cdr vuurwerk)))))))
      (lambda (_vuurwerk0)
         (if (null? _vuurwerk0)
            0
            (if (low-energy? _vuurwerk0)
               0
               (if (atom? _vuurwerk0)
                  1
                  (+ (tel-knallen (car _vuurwerk0)) (tel-knallen (cdr _vuurwerk0)))))))))
 
(define tel-low-energies (lambda (v)
      (if (null? v)
         0
         (if (low-energy? v)
            1
            (if (atom? v)
               0
               (+ (tel-low-energies (car v)) (tel-low-energies (cdr v))))))))
 
(define tel-einde-in (lambda (takken een-kleur)
      (if (null? takken)
         0
         (if (low-energy? (car takken))
            0
            (+ (tel-einde (car takken) een-kleur) (tel-einde-in (cdr takken) een-kleur))))))
 
(define tel-einde (lambda (vuurwerk een-kleur)
      (if (eq? (kleur vuurwerk) een-kleur)
         (tel-low-energies (takken vuurwerk))
         (tel-einde-in (takken vuurwerk) een-kleur))))
 
(define ster? (<change>
      (lambda (vuurwerk)
         (not (member 'X (takken vuurwerk))))
      (lambda (_vuurwerk0)
         (not (member 'X (takken _vuurwerk0))))))
 
(if (eq? (kleur mijn-vuurwerk) 'groen)
   (if (equal? (takken mijn-vuurwerk) (__toplevel_cons (__toplevel_cons 'blauw (__toplevel_cons (__toplevel_cons 'X (__toplevel_cons (__toplevel_cons 'blauw (__toplevel_cons (__toplevel_cons 'X (__toplevel_cons 'X ())) ())) (__toplevel_cons 'X (__toplevel_cons 'X ())))) ())) (__toplevel_cons (__toplevel_cons 'rood (__toplevel_cons (__toplevel_cons (__toplevel_cons 'groen (__toplevel_cons (__toplevel_cons 'X (__toplevel_cons 'X ())) ())) (__toplevel_cons 'X ())) ())) (__toplevel_cons 'X (__toplevel_cons (__toplevel_cons 'geel (__toplevel_cons (__toplevel_cons 'X (__toplevel_cons 'X ())) ())) ())))))
      (if (not (low-energy? mijn-vuurwerk))
         (if (low-energy? 'X)
            (if (= (tel-knallen mijn-vuurwerk) 6)
               (if (= (tel-einde mijn-vuurwerk 'blauw) 5)
                  (not (ster? mijn-vuurwerk))
                  #f)
               #f)
            #f)
         #f)
      #f)
   #f)
 
