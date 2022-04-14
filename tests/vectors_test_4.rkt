(let ([v (vector 1 2)])
  (begin
    (vector-set! v 0 3)
    (vector-ref v 0)))
