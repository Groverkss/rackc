(let ([x #t])
	(if x
		(- (let ([y 10])
			(+ y (let ([y 10])
				(+ y (let ([y 10])
					(+ y (let ([y 10])
						(+ y y))))))))
			8)
		10))