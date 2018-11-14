; Lauren Fromm
; 404751250
; CS 161


; ***GIVEN TO ME*****
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "~/cs161/hw3/hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "~/cs161/hw3/a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 


; ***END OF GIVEN FUNCTIONS****

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
;

; Goal-test is given a state, if there are no more columns to look through, it returns true
; Otherwise, it will check to see if the first row is okay using goal-row
; Goal-row makes sure there are no 3's or 2's (no boxes or keepers left without a goal)
; If the first row is okay, goal-test continues to move through each row
; Otherwise if a row is not okay, NIL is returned

(defun goal-row (r)
  (cond ((null r) t) ; If the row is null, then return true
	((or (equal (car r) 3) (equal (car r) 2)) NIL) ; If there is an unmatched box or keeper as the first element (2 or 3) return false
	((equal (length r) 1) t) ; If this is the last element in the row, return true
	(t (goal-row (cdr r))) ; Otherwise, call the function on the next elements of the row
  );end cond
);end defun

(defun goal-test (s)
  (cond ((null s) t) ; If the row is null, return true 
	((equal t (goal-row (car s))) (goal-test (cdr s))) ; Call goal-row on the first row, if it is okay, call goal-test on the rest of the rows
	(t NIL) ; Otherwise a row was not in final state, so return false
   );end cond
  );end defun


; EXERCISE: Modify this function to return the list of 
; sucessor states of s.


; Get-square returns the value at row r and column c in state s
; Uses find-row to first get to the row by recursivily calling find-row
; Then goes through the row to get to the right column and returns the value

(defun find-row (s r) 
  (cond ((equal r 0) (car s))
	(t (find-row (cdr s) (- r 1)))
  );end cond
);end defun

(defun find-coord (st c) 
  (cond ((equal c 0) (car st))
	(t (find-coord (cdr st) (- c 1)))
  );end cond
);end defun

(defun get-square (s r c) 
  (cond ((> r (length s)) 1)
	((or (< r 0) (< c 0) (> c (length (car s)))) 1)
	(t (let ((st (find-row s r))) 
		 (find-coord st c)))
  );end cond
);end defun

; Set-square sets the given coordinate to the value given
; First it reassigns the state to another variable and then calls find-set
; Find-set first finds the row by recursively calling the find-set function and the previous row to the new list
; Once the right row is found, set-coord is found and the rest of the state is appended to the new list
; In set-coord, the function is recursively called to find the correct column and the row is appended with all of the old values
; When the correct spot is found, the new value is appended with the rest of the row, so that a full state is returned

(defun set-coord (row c v)
  (cond ((equal c 0) (append (list v) (cdr row)))
	(t (append (list (car row)) (set-coord (cdr row) (- c 1) v)))
  );end cond
);end defun

(defun find-set (st r c v)
  (cond ((equal r 0) (append (list (set-coord (car st) c v)) (cdr st)))
	(t (append (list (car st)) (find-set (cdr st) (- r 1) c v)))
  );end cond
);end defun

(defun set-square (s r c v)
  (let ((ss s)) (find-set ss r c v))
);end defun


; try-left, try-right, try-up, and try-down all do similar things but go in different directions
; each function gets the value of the current keeper, the next location, and the location after that
; First, it checks if the value of the current location is 6, meaning the keeper is in the goal
; Next, it checks if the next location is a 0 and resets the state so that the keeper moves and the goal state is free
; If the next location is a box, it checks if the location after is a blank or a goal, and moves the box to be in the next spot, moves the keeper to the next location, and resets the keepers location to goal state
; If the next location is a goal, it moves the keeper and clears the old location to a goal state
; If the next location is a box in a goal, it checks if the spot after is a blank or goal state, and moves it accordingly
; If the current keeper is not in a goal state, it does the same process, but the keepers location gets cleared to a blank space instead
; Otherwise, the keeper can't move so nil is returned 

(defun try-left (s x y)
 (let ((res (get-square s x (- y 1))) (res-next (get-square s x (- y 2))) (curr (get-square s x y )))
   (cond ((equal curr 6) 
	  (cond
	   ((equal 0 res) (set-square (set-square s x y 4)  x (- y 1) 3))
	   ((and (equal 2 res) (equal 0 res-next)) (set-square (set-square (set-square s x y 4) x (- y 1) 3) x (- y 2) 2))
	   ((and (equal 2 res) (equal 4 res-next)) (set-square (set-square (set-square s x y 4) x (- y 1) 3) x (- y 2) 5))
	   ((equal 4 res) (set-square (set-square s x y 4) x (- y 1) 6))
	   ((and (equal 5 res) (equal 0 res-next)) (set-square (set-square (set-square s x y 4) x (- y 1) 6) x (- y 2) 2))
	   ((and (equal 5 res) (equal 4 res-next)) (set-square (set-square (set-square s x y 4) x (- y 1) 6) x (- y 2) 5))
	   (t nil)))
	 (t 
	  (cond 
	   ((equal 0 res) (set-square (set-square s x y 0)  x (- y 1) 3))
	   ((and (equal 2 res) (equal 0 res-next)) (set-square (set-square (set-square s x y 0) x (- y 1) 3) x (- y 2) 2))
	   ((and (equal 2 res) (equal 4 res-next)) (set-square (set-square (set-square s x y 0) x (- y 1) 3) x (- y 2) 5))
	   ((equal 4 res) (set-square (set-square s x y 0) x (- y 1) 6))
	   ((and (equal 5 res) (equal 0 res-next)) (set-square (set-square (set-square s x y 0) x (- y 1) 6) x (- y 2) 2))
	   ((and (equal 5 res) (equal 4 res-next)) (set-square (set-square (set-square s x y 0) x (- y 1) 6) x (- y 2) 5))
	   (t nil)
	   )
	  );end else staement
   );end cond
  );end let
);end defun


(defun try-right (s x y)
 (let ((res (get-square s x (+ y 1))) (res-next (get-square s x (+ y 2))) (curr (get-square s x y )))
   (cond ((equal curr 6) 
	  (cond
	   ((equal 0 res) (set-square (set-square s x y 4)  x (+ y 1) 3))
	   ((and (equal 2 res) (equal 0 res-next)) (set-square (set-square (set-square s x y 4) x (+ y 1) 3) x (+ y 2) 2))
	   ((and (equal 2 res) (equal 4 res-next)) (set-square (set-square (set-square s x y 4) x (+ y 1) 3) x (+ y 2) 5))
	   ((equal 4 res) (set-square (set-square s x y 4) x (+ y 1) 6))
	   ((and (equal 5 res) (equal 0 res-next)) (set-square (set-square (set-square s x y 4) x (+ y 1) 6) x (+ y 2) 2))
	   ((and (equal 5 res) (equal 4 res-next)) (set-square (set-square (set-square s x y 4) x (+ y 1) 6) x (+ y 2) 5))
	   (t nil)))
	 (t 
	  (cond 
	   ((equal 0 res) (set-square (set-square s x y 0)  x (+ y 1) 3))
	   ((and (equal 2 res) (equal 0 res-next)) (set-square (set-square (set-square s x y 0) x (+ y 1) 3) x (+ y 2) 2))
	   ((and (equal 2 res) (equal 4 res-next)) (set-square (set-square (set-square s x y 0) x (+ y 1) 3) x (+ y 2) 5))
	   ((equal 4 res) (set-square (set-square s x y 0) x (+ y 1) 6))
	   ((and (equal 5 res) (equal 0 res-next)) (set-square (set-square (set-square s x y 0) x (+ y 1) 6) x (+ y 2) 2))
	   ((and (equal 5 res) (equal 4 res-next)) (set-square (set-square (set-square s x y 0) x (+ y 1) 6) x (+ y 2) 5))
	   (t nil)
	   )
	  );end else staement
   );end cond
  );end let
);end defun


(defun try-up (s x y)
 (let ((res (get-square s (- x 1) y)) (res-next (get-square s (- x 2) y)) (curr (get-square s x y )))
   (cond ((equal curr 6) 
	  (cond
	   ((equal 0 res) (set-square (set-square s x y 4) (- x 1) y 3))
	   ((and (equal 2 res) (equal 0 res-next)) (set-square (set-square (set-square s x y 4) (- x 1) y 3) (- x 2) y 2))
	   ((and (equal 2 res) (equal 4 res-next)) (set-square (set-square (set-square s x y 4) (- x 1) y 3) (- x 2) y 5))
	   ((equal 4 res) (set-square (set-square s x y 4) (- x 1) y 6))
	   ((and (equal 5 res) (equal 0 res-next)) (set-square (set-square (set-square s x y 4) (- x 1) y 6) (- x 2) y 2))
	   ((and (equal 5 res) (equal 4 res-next)) (set-square (set-square (set-square s x y 4) (- x 1) y 6) (- x 2) y 5))
	   (t nil)))
	 (t 
	  (cond 
	   ((equal 0 res) (set-square (set-square s x y 0) (- x 1) y 3))
	   ((and (equal 2 res) (equal 0 res-next)) (set-square (set-square (set-square s x y 0) (- x 1)  y 3) (- x 2) y 2))
	   ((and (equal 2 res) (equal 4 res-next)) (set-square (set-square (set-square s x y 0) (- x 1) y 3) (- x 2) y 5))
	   ((equal 4 res) (set-square (set-square s x y 0) (- x 1) y 6))
	   ((and (equal 5 res) (equal 0 res-next)) (set-square (set-square (set-square s x y 0) (- x 1) y 6) (- x 2) y 2))
	   ((and (equal 5 res) (equal 4 res-next)) (set-square (set-square (set-square s x y 0) (- x 1) y 6) (- x 2) y 5))
	   (t nil)
	   )
	  );end else staement
   );end cond
  );end let
);end defun


(defun try-down (s x y)
 (let ((res (get-square s (+ x 1) y)) (res-next (get-square s (+ x 2) y)) (curr (get-square s x y )))
   (cond ((equal curr 6) 
	  (cond
	   ((equal 0 res) (set-square (set-square s x y 4) (+ x 1) y 3))
	   ((and (equal 2 res) (equal 0 res-next)) (set-square (set-square (set-square s x y 4) (+ x 1) y 3) (+ x 2) y 2))
	   ((and (equal 2 res) (equal 4 res-next)) (set-square (set-square (set-square s x y 4) (+ x 1) y 3) (+ x 2) y 5))
	   ((equal 4 res) (set-square (set-square s x y 4) (+ x 1) y 6))
	   ((and (equal 5 res) (equal 0 res-next)) (set-square (set-square (set-square s x y 4) (+ x 1) y 6) (+ x 2) y 2))
	   ((and (equal 5 res) (equal 4 res-next)) (set-square (set-square (set-square s x y 4) (+ x 1) y 6) (+ x 2) y 5))
	   (t nil)))
	 (t 
	  (cond 
	   ((equal 0 res) (set-square (set-square s x y 0) (+ x 1) y 3))
	   ((and (equal 2 res) (equal 0 res-next)) (set-square (set-square (set-square s x y 0) (+ x 1)  y 3) (+ x 2) y 2))
	   ((and (equal 2 res) (equal 4 res-next)) (set-square (set-square (set-square s x y 0) (+ x 1) y 3) (+ x 2) y 5))
	   ((equal 4 res) (set-square (set-square s x y 0) (+ x 1) y 6))
	   ((and (equal 5 res) (equal 0 res-next)) (set-square (set-square (set-square s x y 0) (+ x 1) y 6) (+ x 2) y 2))
	   ((and (equal 5 res) (equal 4 res-next)) (set-square (set-square (set-square s x y 0) (+ x 1) y 6) (+ x 2) y 5))
	   (t nil)
	   )
	  );end else staement
   );end cond
  );end let
);end defun


; try-move gets the keepers current position and translates that to a row and column, and then calls the direction function on the direction given

(defun try-move (s d)
  (let* ((keep (getKeeperPosition s 0)) (keep_row (cadr keep)) (keep_col (car keep))) 
    (cond ((equal d 0) (try-up s keep_row keep_col))
	  ((equal d 1) (try-right s keep_row keep_col))
	  ((equal d 2) (try-down s keep_row keep_col))
	  ((equal d 3) (try-left s keep_row keep_col))
    );end cond
  );end let
);end defun

;next-states creates a list of all possible moves in every direction for the current state

(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s 0) (try-move s 1) (try-move s 2) (try-move s 3)))
	 )
    (cleanUpList result);end
   );end let
  );end defun

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
; Trivial function returns 0 everytime

(defun h0 (s)
  0
);end defun

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.

; h1 counts the number of misplaced boxes by first counting the amount in the first row and the recursively calling the function on the rest and adding those two values together
; h1-row is called on each row
; if the row is null, then 0 is returned
; otherwise if the first value is a 2, which is an unmatched box, 1 is added to the result and the function is called on the rest of the row
; if the value isn't a two, the function is called on the rest of the row

; This heuristic is admissible because it will never overestimate the goal cost. If there are mismatched boxes, the cost is guaranteed to be at least the amount of mismatched boxes because you can only move one box at a time.

(defun h1-row (r) 
  (cond ((null r) 0)
	((equal (car r) 2) (+ 1 (h1-row (cdr r))))
	(t (h1-row (cdr r)))
  );end cond
);end defun


(defun h1 (s)
  (cond ((null s) 0)
	(t (+ (h1-row (car s)) (h1 (cdr s)))))
);end defun

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;

; Basic Idea for implementation :
; Find minimum distance from keeper to each box
; Find minimum distance from each box to goal state
; Add all distances to get heuristic, because keeper has to move to at least the box, and then move at least the amount of spots to the goal


; Find-box returns the coordinates of all the boxes
; It is given the states and the start row (0 when called)
; If either the states or start row are null, returns nil
; Otherwise, it finds the box coordinates in the first row (using the row number passed in) and appends it to the box coordinates of the other rows
; The box coordinates in a row are found using get-box-cord
; Get-box-cord gets the row of values, the y value to make the coordinate, and the start x value (0 initially)
; If any of the inputs are null, nil is returned
; If the first value in the row is 2, then it adds the coordinates to a list and calls the function on the rest of the row
; Otherwise it just calls the function on the rest of the row


(defun get-box-cord (row y x)
  (cond ((or (null x) (null y) (null row)) nil)
	((equal 2 (car row)) (append (list (list y x)) (get-box-cord (cdr row) y (+ x 1))))
	((get-box-cord (cdr row) y (+ x 1))))
)

(defun find-box (s l)
  (cond ((or (null s) (null l)) nil) 
	((or (equal s nil) (null s)) nil)
	(t (append (get-box-cord (car s) l 0) (find-box (cdr s) (+ 1 l))))
))

; Find-goals and get-goal-cord do the same as the above functions, but instead look for goal states (4)
; Creates a list of coordinates where there are goals

(defun get-goal-cord (row y x)
  (cond ((or (null y) (null x) (null row)) nil)
	((equal 4 (car row)) (append (list (list y x)) (get-goal-cord (cdr row) y (+ x 1))))
	((get-goal-cord (cdr row) y (+ x 1))))
)

(defun find-goals (s l)
  (cond ((or (null s) (null l)) nil)
	((or (equal s nil) (null s)) nil)
	(t (append (get-goal-cord (car s) l 0) (find-goals (cdr s) (+ 1 l))))
))

; Absoulte takes the absolute value of the number
; If the number is negative, it multiplies it by -1, otherwise it returns the number

(defun absolute (num) 
  (cond ((null num) nil)
	((< num 0) (* num -1))
	(t num)
  )
)

; Minimum-distance-array helps create a list of distances from a box to each goal state
; It assigns the coordinates of the box to bx and by and the coodrinates of the first goal to gx and gy
; It calculates the distance by adding the absoulte value of the difference between bx and gx, and by and gy
; A list of these values are created and appended to the recursive call on the rest of the goals

(defun minimum-distance-array (b g)
  (cond ( (or (null b) (null g)) nil)
	(t (let ((bx (car b)) (by (cadr b)) (gx (caar g)) (gy (cadar g)))
	     (append (list (list (+ (absolute (- bx gx)) (absolute (- by gy))))) (minimum-distance-array b (cdr g)))
	     ))
))

; Minimum distance is called on the array of distances from each box to each goal state
; Meant to find minimum distance
; First checks if there is only one distance value and returns that value
; Otherwise checks if the first value is less than the second value, and if so checks the first against the rest of the values
; Otherwsie, it checks the rest of values for the minimum 

(defun minimum-distance (arr)
    (cond ((null arr) nil)
	  ((equal (length arr) 1) (caar arr))
	  ((< (caar arr) (caadr arr)) (minimum-distance (append (list (car arr)) (cddr arr))))
	  (t (minimum-distance (cdr arr)))))

; find-sum gets a list of box coordinates and goal coordinates
; It adds the result of the smallest distance for each box to a goal and then recursively calls the function for all boxes

(defun find-sum (b g)
  (cond ((or (null b) (equal b nil)) 0)
	(t (+ (minimum-distance (minimum-distance-array (car b) g)) (find-sum (cdr b) g))))
)

; The heuristic function uses the above helper functions to find the minimum amount of steps the keeper must take without overestimating
; It first gets a list of all boxes, all goal states, and all keeper locations
; It then uses find-sum on both the boxes and goals and for boxes and keeper to find the minimum distance for all the boxes to get to goal states, and for the keeper to get to the boxes
; It adds these numbers together to get the hueristic


(defun h404751250 (s)
  (cond ((null s) 0)
	(t (let* ((bx (find-box s 0)) (goals (find-goals s 0)) (keeper (list (list (cadr (getKeeperPosition s 0)) (car (getKeeperPosition s 0))))))
	     (+ (find-sum bx goals) (find-sum bx keeper))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

;(15)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(?)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(?)
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(?)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
