(let ([iters (read)])
  (let ([sum 0])
    (begin
      (while (> iters 0)
        (begin
          (let ([x (read)]) (set! sum (+ sum x)))
          (set! iters (- iters 1))))
      sum)))
