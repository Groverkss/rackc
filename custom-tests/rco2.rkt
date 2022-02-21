(let ([x 1])
  (let ([y 2])
    (let ([xy (- x y)])
      (+
       (+
        (- x)
        (- y 1))
       (- 1 y)))))
