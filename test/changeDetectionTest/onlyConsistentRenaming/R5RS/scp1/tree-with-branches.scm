;; renamed lambdas/lets: 7
 
(define atom? (lambda (x)
      (not (pair? x))))
 
(define maak-blad (lambda (type)
      type))
 
(define geef-type (lambda (blad)
      blad))
 
(define maak-knoop (lambda (deelbomen)
      deelbomen))
 
(define geef-deelbomen (<change>
      (lambda (boom)
         boom)
      (lambda (_boom0)
         _boom0)))
 
(define maak-hybride-tak (lambda (knopen)
      knopen))
 
(define geef-knopen (lambda (tak)
      tak))
 
(define leeg? (<change>
      (lambda (boom)
         (null? boom))
      (lambda (_boom0)
         (null? _boom0))))
 
(define knoop? (<change>
      (lambda (boom)
         (pair? boom))
      (lambda (_boom0)
         (pair? _boom0))))
 
(define blad? (lambda (boom)
      (atom? boom)))
 
(define hybride-tak (maak-hybride-tak
      (list
         (maak-knoop
            (list
               (maak-knoop (list (maak-blad 'appel) (maak-blad 'appel) (maak-blad 'blad)))
               (maak-blad 'peer)))
         (maak-knoop (list (maak-blad 'blad) (maak-blad 'peer)))
         (maak-knoop (list (maak-blad 'appel) (maak-knoop (list (maak-blad 'appel) (maak-blad 'blad))))))))
 
(define tak (maak-hybride-tak
      (list
         (maak-knoop
            (list
               (maak-knoop (list (maak-blad 'appel) (maak-blad 'appel) (maak-blad 'blad)))
               (maak-blad 'peer)))
         (maak-knoop (list (maak-blad 'blad) (maak-blad 'peer) (maak-blad 'appel)))
         (maak-knoop (list (maak-blad 'appel) (maak-knoop (list (maak-blad 'appel) (maak-blad 'blad))))))))
 
(define tel (lambda (boom)
      (define combine-results (<change>
            (lambda (l1 l2)
               (list (+ (car l1) (car l2)) (+ (cadr l1) (cadr l2)) (+ (caddr l1) (caddr l2))))
            (lambda (_l10 _l20)
               (list (+ (car _l10) (car _l20)) (+ (cadr _l10) (cadr _l20)) (+ (caddr _l10) (caddr _l20))))))
      (define tel-hulp (lambda (boom)
            (if (leeg? boom)
               (list 0 0 0)
               (if (if (blad? boom) (eq? boom 'appel) #f)
                  (list 1 0 0)
                  (if (if (blad? boom) (eq? boom 'peer) #f)
                     (list 0 1 0)
                     (if (blad? boom)
                        (list 0 0 1)
                        (tel-hulp-in (geef-knopen boom))))))))
      (define tel-hulp-in (lambda (lst)
            (if (null? lst)
               (list 0 0 0)
               (combine-results (tel-hulp (car lst)) (tel-hulp-in (cdr lst))))))
      (tel-hulp boom)))
 
(define member? (<change>
      (lambda (x lst)
         (pair? (memq x lst)))
      (lambda (_x0 _lst0)
         (pair? (memq _x0 _lst0)))))
 
(define normaal? (lambda (knoop)
      (let ((types (map (lambda (x) (if (pair? x) 'tak x)) knoop)))
         (not (if (member? 'appel types) (member? 'peer types) #f)))))
 
(define check-normaal (<change>
      (lambda (boom)
         (if (leeg? boom)
            #t
            (if (blad? boom)
               #t
               (if (knoop? boom)
                  (if (normaal? boom)
                     (check-normaal-in (geef-knopen boom))
                     #f)
                  (check-normaal-in (geef-knopen boom))))))
      (lambda (_boom0)
         (if (leeg? _boom0)
            #t
            (if (blad? _boom0)
               #t
               (if (knoop? _boom0)
                  (if (normaal? _boom0)
                     (check-normaal-in (geef-knopen _boom0))
                     #f)
                  (check-normaal-in (geef-knopen _boom0))))))))
 
(define check-normaal-in (<change>
      (lambda (lst)
         (if (null? lst)
            #t
            (if (check-normaal (car lst))
               (check-normaal-in (cdr lst))
               #f)))
      (lambda (_lst0)
         (if (null? _lst0)
            #t
            (if (check-normaal (car _lst0))
               (check-normaal-in (cdr _lst0))
               #f)))))
 
(if (equal? (tel hybride-tak) (__toplevel_cons 4 (__toplevel_cons 2 (__toplevel_cons 3 ()))))
   (check-normaal hybride-tak)
   #f)
 
