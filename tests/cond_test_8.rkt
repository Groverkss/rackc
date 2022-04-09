(let ([x 5])
	(if (eq? (read) x)
		42 (if (let ([y (and (>= 4 4)
				(< 0 1))])
			(not y)) 5 (+ x
				(read)))))