
(defun get_items_that_satisfy (l main_item comparators)  
	(cond 
		(
			(null l)
			()
		)
		(
			t
			(if 
				(funcall comparators (car l) main_item)
				(concatLists 
					(car l)
					(get_items_that_satisfy (cdr l) main_item comparators)
				)
				(get_items_that_satisfy (cdr l) main_item comparators)
			)
		)
	)
)


(defun quick_sort (l) 

	(cond
		(
			(or (null l) (eql (length l) 1))
			l
		)
		(
			t
			(concatListsMany 
				(list
					(quick_sort 
						(concatLists
							(quick_sort (get_items_that_satisfy (get_elements l 1 (1- (ceiling (/ (length l) 2)))) (pick l (ceiling (/ (length l) 2))) '<))
							(quick_sort (get_items_that_satisfy (get_elements l  (1+ (ceiling (/ (length l) 2))) (length l)) (pick l (ceiling (/ (length l) 2))) '<))
						)						
					)
					(pick l (ceiling (/ (length l) 2)))
					(quick_sort
						(concatLists
							(quick_sort (get_items_that_satisfy (get_elements l 1 (1- (ceiling (/ (length l) 2)))) (pick l (ceiling (/ (length l) 2))) '>=))
							(quick_sort (get_items_that_satisfy (get_elements l  (1+ (ceiling (/ (length l) 2))) (length l)) (pick l (ceiling (/ (length l) 2))) '>=))
						)
					)
				)			
			)
		)
	)
)