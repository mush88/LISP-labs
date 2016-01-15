(defun merge_lists (l1 l2) 
	(cond 
		(
			(null l1)
			l2
		)
		(
			(null l2)
			l1
		)
		(
			(if (< (car l1) (car l2))
				(cons (car l1) (merge_lists (cdr l1) l2))
				(cons (car l2) (merge_lists (cdr l2) l1))
			)
		)
	)
)
