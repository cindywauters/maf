;; renamed lambdas/lets: 9
 
(define atom? (lambda (x)
      (not (pair? x))))
 
(define maak-dier (lambda (naam eigenschappen)
      (list naam eigenschappen)))
 
(define naam (<change>
      (lambda (dier)
         (car dier))
      (lambda (_dier0)
         (car _dier0))))
 
(define eigenschappen (<change>
      (lambda (dier)
         (cadr dier))
      (lambda (_dier0)
         (cadr _dier0))))
 
(define dier? (<change>
      (lambda (dier)
         (if (pair? dier)
            (if (atom? (naam dier))
               (pair? (eigenschappen dier))
               #f)
            #f))
      (lambda (_dier0)
         (if (pair? _dier0)
            (if (atom? (naam _dier0))
               (pair? (eigenschappen _dier0))
               #f)
            #f))))
 
(define maak-boom (lambda (knoop deelbomen)
      (list knoop deelbomen)))
 
(define knoop (lambda (boom)
      (car boom)))
 
(define deelbomen (lambda (boom)
      (cadr boom)))
 
(define leeg? (<change>
      (lambda (boom)
         (null? boom))
      (lambda (_boom0)
         (null? _boom0))))
 
(define knoop? (<change>
      (lambda (boom)
         (dier? boom))
      (lambda (_boom0)
         (dier? _boom0))))
 
(define classificatieboom (maak-boom
      (maak-dier 'dier (__toplevel_cons 'kan-ademen (__toplevel_cons 'kan-bewegen ())))
      (list
         (maak-boom
            (maak-dier
               'vis
               (__toplevel_cons 'kan-zwemmen (__toplevel_cons 'heeft-schubben (__toplevel_cons 'heeft-vinnen ()))))
            (list (maak-dier 'ballonvis (__toplevel_cons 'kan-zwellen (__toplevel_cons 'is-geel ())))))
         (maak-boom
            (maak-dier
               'landdier
               (__toplevel_cons 'heeft-huid (__toplevel_cons 'kan-lopen (__toplevel_cons 'heeft-poten ()))))
            (list (maak-dier 'olifant (__toplevel_cons 'is-groot ()))))
         (maak-boom
            (maak-dier
               'vogel
               (__toplevel_cons 'kan-vliegen (__toplevel_cons 'heeft-vleugels (__toplevel_cons 'heeft-veren ()))))
            (list
               (maak-dier 'kanarie (__toplevel_cons 'kan-zingen (__toplevel_cons 'is-geel ())))
               (maak-dier 'arend (__toplevel_cons 'is-groot ())))))))
 
(define all-kinds (<change>
      (lambda (boom)
         (if (leeg? boom)
            ()
            (if (dier? boom)
               (list (naam boom))
               (if (dier? (knoop boom))
                  (append (list (naam (knoop boom))) (all-kinds-in (deelbomen boom)))
                  (all-kinds-in (deelbomen boom))))))
      (lambda (_boom0)
         (if (leeg? _boom0)
            ()
            (if (dier? _boom0)
               (list (naam _boom0))
               (if (dier? (knoop _boom0))
                  (append (list (naam (knoop _boom0))) (all-kinds-in (deelbomen _boom0)))
                  (all-kinds-in (deelbomen _boom0))))))))
 
(define all-kinds-in (<change>
      (lambda (lst)
         (if (null? lst)
            ()
            (append (all-kinds (car lst)) (all-kinds-in (cdr lst)))))
      (lambda (_lst0)
         (if (null? _lst0)
            ()
            (append (all-kinds (car _lst0)) (all-kinds-in (cdr _lst0)))))))
 
(define geef-eigenschappen (lambda (boom soort)
      (define geef-eig (<change>
            (lambda (boom eig)
               (if (dier? boom)
                  (if (eq? (naam boom) soort)
                     (append eig (list (eigenschappen boom)))
                     #f)
                  (if (if (dier? (knoop boom)) (eq? (naam (knoop boom)) soort) #f)
                     (append eig (eigenschappen (knoop boom)))
                     (geef-eig-in (deelbomen boom) (append eig (eigenschappen (knoop boom)))))))
            (lambda (_boom0 _eig0)
               (if (dier? _boom0)
                  (if (eq? (naam _boom0) soort)
                     (append _eig0 (list (eigenschappen _boom0)))
                     #f)
                  (if (if (dier? (knoop _boom0)) (eq? (naam (knoop _boom0)) soort) #f)
                     (append _eig0 (eigenschappen (knoop _boom0)))
                     (geef-eig-in (deelbomen _boom0) (append _eig0 (eigenschappen (knoop _boom0)))))))))
      (define geef-eig-in (lambda (lst eig)
            (if (null? lst)
               #f
               (let ((__or_res (geef-eig (car lst) eig)))
                  (if __or_res
                     __or_res
                     (geef-eig-in (cdr lst) eig))))))
      (geef-eig boom ())))
 
(define ask? (lambda (boom soort eig)
      (<change>
         (let ((eigenschappen (geef-eigenschappen boom soort)))
            (pair? (memq eig eigenschappen)))
         (let ((_eigenschappen0 (geef-eigenschappen boom soort)))
            (pair? (memq eig _eigenschappen0))))))
 
(if (equal? (all-kinds classificatieboom) (__toplevel_cons 'dier (__toplevel_cons 'vis (__toplevel_cons 'ballonvis (__toplevel_cons 'landdier (__toplevel_cons 'olifant (__toplevel_cons 'vogel (__toplevel_cons 'kanarie (__toplevel_cons 'arend ())))))))))
   (if (ask? classificatieboom 'landdier 'kan-lopen)
      (if (ask? classificatieboom 'ballonvis 'heeft-vinnen)
         (not (ask? classificatieboom 'olifant 'kan-vliegen))
         #f)
      #f)
   #f)
 
