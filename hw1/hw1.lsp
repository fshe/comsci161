;Frank She 204172020
;TREE-CONTAINS checks if the number N is present in the ordered-tree TREE
;If N is present in TREE, then TREE-CONTAINS returns t, otherwise nil
(defun TREE-CONTAINS(N TREE)
	(cond ((atom TREE) (= N TREE))
		((= N (second TREE)) t)	;check if the number is in the middle
		((TREE-CONTAINS N (first TREE)) t) ;check the left side
		((TREE-CONTAINS N (third TREE)) t) ;check the right side
		(t nil)))

;TREE-MAX takes the argument TREE, an ordered tree, and returns the maximum number in TREE
(defun TREE-MAX(TREE)
	(cond ((atom TREE) TREE)
		(t (TREE-MAX (third TREE))))) ;Continues until it reaches the rightmost (maximum) number

;TREE-ORDER takes the ordered-tree TREE, and returns an in-order list of numbers that appear in TREE
(defun TREE-ORDER(TREE)
	(cond ((atom TREE) (list TREE)) ;return atoms as lists
		(t (append 
		(TREE-ORDER (first TREE)) 
		(TREE-ORDER (second TREE)) 
		(TREE-ORDER (third TREE)))))) ;append all three components of the tree together

;SUB-LIST takes a list L and two non-negative integers START and LEN. It returns the sub-list of L starting at position START and having length LEN. The first element of L has position 0.
(defun SUB-LIST(L START LEN)
	(cond ((= LEN 0) nil)
	((> START 0) (SUB-LIST (rest L) (- START 1) LEN)) ;remove characters until you reach your starting position
	(t (cons (first L) (SUB-LIST (rest L) START (- LEN 1)))))) ;Add to the list until LEN reaches 0

;SPLIT-LIST takes a list L and returns a list of two lists L1 and L2, such that L is the result of appending L1 and L2 and the length of L1 and L2 differ by no more than 1
(defun SPLIT-LIST (L)
	(cond ((oddp (length L)) (let ((half (/ (- (length L) 1) 2))) ;for odd lengths, calculate half by subtracting one and dividing by 2
		(cons (SUB-LIST L 0 half) (cons (SUB-LIST L half (- (length L) half)) NIL)))) ;split the list using those indices
		(t (let ((half (/ (length L) 2))) 
		(cons (SUB-LIST L 0 half) (cons (SUB-LIST L half (- (length L) half)) NIL)))))) ;just divide by two to find the cutoff

;LIST2BTREE takes a non-empty list of atoms LEAVES and returns a binary tree
(defun LIST2BTREE(LEAVES)
	(cond ((NULL LEAVES) nil)
		((= 1 (length LEAVES)) 
			(first LEAVES)) ;if there is only one leaf, return it
		((> (length LEAVES) 2) 
			(list (LIST2BTREE (first (SPLIT-LIST LEAVES))) (LIST2BTREE (second (SPLIT-LIST LEAVES))))) ;If there are > 2 leaves, split the inner leaves
		(t LEAVES)))

;BTREE2LIST takes a binary tree, TREE, and returns a list of atoms
(defun BTREE2LIST(TREE)
	(cond ((atom TREE) (list TREE)) ;return the atom as a list
		(t (append (BTREE2LIST (first TREE)) (BTREE2LIST (second TREE)))))) ;append the deconstructed first element and deconstructed second element
