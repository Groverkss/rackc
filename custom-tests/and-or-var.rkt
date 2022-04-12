(+ 1 (if (let ([x 5]) (let ([x (+ 1 x)]) (and (eq? x 1) (or (< x 1) (> x 1))))) 1 2))
