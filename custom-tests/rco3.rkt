(let ([x 1])
  (let ([y 2])
    (let ([xy (- x y)])
      (let ([yx (+ xy (+ x y))])
        (+
         (+
          (- x)
          (- xy 1))
         (- 1 (+ yx (- y))))))))
