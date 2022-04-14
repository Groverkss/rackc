(let ([v (vector 7 2 3 30)])
  (begin
    (vector-set! v 0 1)
    (vector-set! v 1 1)
    (vector-set! v 2 1)
    (vector-set! v 3 1)
  (+ (vector-ref v 0)
     (+ (vector-ref v 1)
        (+ (vector-ref v 2) (vector-ref v 3))))))
