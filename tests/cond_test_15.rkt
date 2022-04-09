(let ([x (read)])
	(if (eq? (read) x)
		42 (if (let ([y (and (>= 4 4)
						(< 0 1))])
					(not y))
				10
				(if (> x 15)
					5
					(+ x (read))))))
