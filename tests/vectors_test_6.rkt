(let ([v (vector 7 2 3 30)])
  (+ (vector-ref v 0)
     (+ (vector-ref v 1)
        (+ (vector-ref v 2) (vector-ref v 3)))))
