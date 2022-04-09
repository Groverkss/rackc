(let ([x (read)])
	(let ([y (read)])
		(if (or (eq? x 0) (> y 0))
			42 8)))