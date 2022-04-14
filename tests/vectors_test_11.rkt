(let ([i 1000])
    (let ([y (vector 2)])
     (let ([z (vector 2 3)])
      (let ([x (vector 38 42 42 42 42 42 42 42 42 42 42 42 42 42)])
        (begin 
          (while (> i 0)
                 (begin 
                   (set! i (- i 1))
                   (let ([v (vector 1 2 3 4 5 6 7 8 9 10)]) 0)
                   ))
          (+ (+ (vector-ref x 0) (vector-ref z 0)) (vector-ref y 0)))))))
