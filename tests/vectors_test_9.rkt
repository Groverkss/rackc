(let ([v (vector (vector 1 6) (vector 2) 3 (vector 1 (vector 1 35)))])
  (+ (vector-length (vector-ref v 0))
     (+ (vector-ref (vector-ref v 1) 0)
        (+ (vector-ref v 2) (vector-ref (vector-ref (vector-ref v 3) 1) 1)))))
