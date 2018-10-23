;Lauren Fromm
;404751250
;hw 1


; #1 TREE-CONTAINS
; ARGUMENTS: N, TREE
; RETURN: Returns t if N is in TREE, nil otherwise
; Checks if tree is null and returns nil
; Checks whether tree is just number and if number equals N
; Otherwise checks second number of tree (m) if equal to N
; Otherwise, if N is less than m, checks first half of list
; Otherwise checks second half of list

(defun TREE-CONTAINS (N TREE)
  (cond ((null TREE) NIL)
   ((numberp TREE)
    (cond ((equal TREE N) t)
	  (t NIL))
   )
   ((equal N (second TREE)) t)
   ((< N (second TREE)) (TREE-CONTAINS N (first TREE)))
   (t (TREE-CONTAINS N (third TREE)))
  )
)

; #2 TREE-MIN
; ARGUMENT: TREE
; RETURN: Returns minimum number in tree
; First checks if tree is empty and returns NIL
; Then checks if tree is just a single number and returns that number
; Checks if the first list is just a number and returns that number
; otherwise runs the function on the first list

(defun TREE-MIN (TREE)
  (cond ((null TREE) NIL)
    ((numberp TREE) TREE)
    ((numberp (first TREE)) (first TREE))
    (t (TREE-MIN (first TREE)))
  )
)

; #3 TREE-ORDER
; ARGUMENT: TREE
; RETURN: Returns ordered list of numbers appearing in TREE
; First checks if tree is empty and returns NIL
; Checks if tree is just a number and returns the number
; Then appends to the list the second number of TREE (m), the tree ordered left list, and the tree ordered right list

(defun TREE-ORDER (TREE)
  (cond ((null TREE) NIL)
    ((numberP TREE) (list TREE))
    (t (append (list (second TREE)) (TREE-ORDER (first TREE)) (TREE-ORDER (third TREE))))
  )
)

; #4 SUB-LIST
; ARGUMENTS: L, START, LEN
; RETURN: Returns a sublist of L starting at position START and having length LEN
; First checks if list is null and returns nill
; Then checks if length is 0 and returns NIL
; Then checks if start is at 0, and appends list using the first element of the list and then the sublist of the rest of L and a smaller length
; Otherwise, it creates a sublest with the rest of L and moves the start closer to 0

(defun SUB-LIST (L START LEN)
  (cond ((null L) NIL)
    ((equal 0 LEN) NIL)
    ((equal START 0) (append (list (first L)) (SUB-LIST (rest L) START (- LEN 1))))
    (t (SUB-LIST (rest L) (- START 1) LEN))
   )
)

; #5 SPLIT-LIST
; ARGUMENT: L
; RETURN: Returns two lists where appending L1 and L2 gives L
; If the list is null, it returns nill
; if the length of the list is even, it creates two sublists of dividing the length of the list by 2
; Otherwise, it creates sublists by having one length be 1 unit longer that the other

(defun SPLIT-LIST (L)
  (cond ((null L) NIL)
    ((evenp (length L)) (append (list (SUB-LIST L 0 (/ (length L) 2)) (SUB-LIST L (/ (length L) 2) (/ (length L) 2))))) 
    (t (append (list (SUB-LIST L 0 (+ (/ (length L) 2) (/ 1 2))) (SUB-LIST L (+ (/ (length L) 2) (/ 1 2)) (- (/ (length L) 2) (/ 1 2))))))
  )
)


; #6 BTREE-HEIGHT
; ARGUMENT: TREE
; RETURN: Returns height of given tree TREE
; If tree is null, returns nil
; If tree is just a number, returns 0
; If the second element is a list, it adds one to the count and calls the function on the second elemetn
; If the first element is a list, it adds one and calls the functino on the the first element
; Otherwise, it returns one


(defun BTREE-HEIGHT (TREE)
  (cond ((NULL TREE) NIL)
	((numberp TREE) 0)
	((listp (second TREE)) (+ 1 (BTREE-HEIGHT (second TREE))))
	((listp (first TREE)) (+ 1 BTREE-HEIGHT (SECOND TREE)))
	(t 1)))

; #7 LIST2BTREE
; ARGUMENT: LEAVES
; RETURN: Returns a binary tree when given a list of atoms LEAVES
; If leaves is null, returns nil
; if list is just one number, it returns just the number
; If list is just two numbers, returns the list
; Otherwise, splits list and calls LIST2BTREE on both halves

(defun LIST2BTREE (LEAVES)
  (cond ((null LEAVES) NIL)
	((null (rest LEAVES)) 
	       (cond ((numberP (first LEAVES)) (first LEAVES))
	       )
	)
	((equal 2 (length LEAVES)) LEAVES)
        (t (list (LIST2BTREE (first (SPLIT-LIST LEAVES))) (LIST2BTREE  (second (SPLIT-LIST LEAVES)))))
  )
)

; #8 BTREE2LIST
; ARGUMENT: TREE
; RETURN: Returns a list of atoms when given a binary tree TREE
; If tree is null, returns nil
; if TREE is just a number, returns a list of number
; Othewise, appends a list of the first half and second half being called with BTREE2LIST

(defun BTREE2LIST (TREE)
  (cond ((null TREE) NIL)
	((numberp TREE) (list TREE))
	(t (append (BTREE2LIST (first TREE)) (BTREE2LIST (second TREE))))
  )
)

; #9 IS-SAME
; ARGUMENTS: E1 E2
; RETURNS: Returns t if E1 and E2 are identical, and nil otherwise
; First checks if both are null and returns true if so
; Checks if one is null and the other isn't and returns false
; Checks if one is a number and another isnt and returns false
; Checks if both are number and then if both are equal and returns true
; Calls thefunction on the first of both and the rest of both

(defun IS-SAME (E1 E2)
  (cond ((null E1) 
	 (cond ((null E2) t)
	       (t NIL)))
       ((null E2) NIL)
       ((numberp E1) 
	(cond ((not(numberp E2)) nil)
	      ((= E1 E2) t)))
       ((and (not(numberp E1)) (numberp E2)) nil)
       (t (IS-SAME (first E1) (first E2)) (IS-SAME (rest E1)(rest E2)))
))
