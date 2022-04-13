(let ([sum 0])
  (let ([i 5])
    (begin
      (while (and (> i 0) (eq? 1 1))
        (begin
          (set! sum (+ (let ([sum 0])
                       (let ([i 5])
                         (begin
                           (while (> i 0)
                                  (begin
                                    (set! sum (+ sum i))
                                    (set! i (- i 1))))
                           sum))) sum))
          (set! i (- i 1))))
      sum)))
