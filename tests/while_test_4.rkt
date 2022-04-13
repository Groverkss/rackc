(let ([i 1])
  (begin 
    (while (> i 0)
          (begin
            (read)
            (set! i (- i 1))
            (+ i i)))
    1))
