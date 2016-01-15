(defun shift (l)
	(cond
		(
			(null l)
			l
		)
		(
			t
			(append (cdr l) (list (car l)))
		)
	)

)

(defun cycle (l i)
	(cond 
		(
			(or (null l) (not (listp l)))
			l
		)
		(
			t
			(if (< i (length l))
				(append
					(cycle 
						(shift l)
						(1+ i)
					)
					(list l)
				)
				(list l)
			)
		)
	)
)

(defun cycle_helper (l i) 
	(cond
		(
			(null l)
			l
		)
		(
			t
			(append
				(cycle (car l))
				(cdr l)
			)

		)
	)
)