(defun concatListsMany (l)
	(cond
		(
			(null l)
			l
		)
		(
			t
			(concatLists (car l) (concatListsMany (cdr l)))
		)
	)
)
(defun concatLists (l1 l2)   ;; concate 2 S-exprassions in 1 list, if it is 2 lists, then concate them
	(cond 
		(
			(null l1)			;; if one of lists is empty
			(if 
				(listp l2)
				l2
				(list l2)
			)
		)
		(
			(null l2)			;; if one of lists is empty
			(if 
				(listp l1)
				l1
				(list l1)
			)
		)
		(
			(not (listp l1))	;; if the first element is not list
			(cond 
				(
					(listp l2)	;; and second element is list
					(cons 
						l1
						l2
					)
				)
				(
					t 			;; if both elements not lists
					(list l1 l2)
				)
			)
		)
		(   					;; first element - non empty list
			(null (cdr l1))		;; if first list contain only one element
			(cond
				(
					(not (listp l2))  ;; if secon element - not a list
					(cons 			  ;; add head of l1 to l1
						(car l1)
						(list l2)		;; make list from it
					)
				)
				(
					t   			 ;; if secon element is list
					(cons 			
						(car l1)
						l2
					)
				)
			)
		)
		( 							;; if l1 - list and contain more than 1 element
			t
			(cons
				(car l1)
				(concatLists (cdr l1) l2)
			)
		)
	)
)

(defun reverseListsHelper (l)	;; reverse order in list recurently
	(cond
		(
			(null l)			;; empty list
			l
		)
		(
			(not (listp l))		;; not list
			l
		)
		(
			(null (cdr l))		;; list contain only one element
			(list (reverseListsHelper (car l)))
		)
		(
			t
			(concatLists
				
			 	(reverseListsHelper (cdr l))
			 	(list (reverseListsHelper (car l)))

			 )
		)
	)
)

(defun reverseLists (l)  ;; reverse order in each list-item recurently
	(cond 
		(
			(null l)				;; empty list
			l
		)
		(
			(not (listp l))			;; not list
			l
		)
		(
			(null (cdr l))  			;; list contain only one element
			(list (reverseListsHelper (car l)))
		)
		(
			t
			(concatLists
				(list (reverseListsHelper (car l)))
				(reverseLists (cdr l))
			)
		)

	)
)

