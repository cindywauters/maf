(letrec ((f (lambda (x) (+ (* x x) (* x x)))) (g (lambda (x) (+ (* x x) (* x x))))) (let ((_tmp1 (f 1))) (let ((_tmp2 (f 2))) (let ((_tmp3 (f 3))) (let ((_tmp4 (f 4))) (let ((_tmp5 (f 5))) (let ((_tmp6 (f 6))) (let ((_tmp7 (f 7))) (let ((_tmp8 (f 8))) (let ((_tmp9 (g 9))) (f 10)))))))))))