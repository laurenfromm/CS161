; Lauren Fromm
; 404751250
; HW 2
;95;0c

; #1
; ARGUMENT: L
; RETURN: Return top-level list of the nodes in the order they would be visited
; First checks if input list is null and returns nil
; Then checks if input is just a number and returns a list of that number
; Then checks if list is just length of one and calls the function on the one element in the list
; Then calls the function on the rest of the list before calling it on the first element

(defun DFS (L) 
  (cond ((null L) NIL)
	((atom L) (list L))
	((and (numberp (first L)) (equal NIL (rest L))) (append (DFS (first L))))
	(t (append (DFS (rest L)) (DFS (first L))))
))


; #2

; helper
; Arguments : List L and max-depth m
; RETURN: Helper function that goes to every level and returns the list of nodes in the order they would be visited for a DFS
; Checks if m is -1 and returns NIL
; Otherwise checks if L is null and returns NIL
; Checks if L is an atom and returns a list of the atom
; Checks if L is just a list with one element and returns the list 
; Otherwise calls the function on the rest of the list with the same depth and the first of the list with one less depth

(defun helper (L m)
  (cond ((equal -1 m) NIL)
	((null L) NIL)
	((atom L) (list L))
	((and (numberp (first L)) (equal NIL (rest L))) (append (helper L m)))
	(t (append (helper (rest L) m) (helper (first L) (- m 1)))))
)

; DFID
; Arguments : List L and max-depth m
; Checks if L is null or m is -1 and returns NIL
; Otherwise calls the helper function to look at the nodes

(defun DFID (L m)
  (cond ((null L) NIL)
	((equal -1  m) NIL)
	(t (append (DFID L (- m 1)) (helper L m))))
)

; #3

; These functions implement a depth-first solver for the missionary-cannibal
; problem. In this problem, three missionaries and three cannibals are trying to
; go from the east side of a river to the west side. They have a single boat
; that can carry two people at a time from one side of the river to the
; other. There must be at least one person in the boat to cross the river. There
; can never be more cannibals on one side of the river than missionaries. If
; there are, the cannibals eat the missionaries.

; In this implementation, a state is represented by a single list
; (missionaries cannibals side). side represents which side the boat is
; currently on, and is T if it is on the east side and NIL if on the west
; side. missionaries and cannibals represent the number of missionaries and
; cannibals on the same side as the boat. Thus, the initial state for this
; problem is (3 3 T) (three missionaries, three cannibals, and the boat are all
; on the east side of the river) and the goal state is (3 3 NIL).

; The main entry point for this solver is the function MC-DFS, which is called
; with the initial state to search from and the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, MC-DFS returns NIL.

; To call MC-DFS to solve the original problem, one would call (MC-DFS '(3 3 T)
; NIL) -- however, it would be possible to call MC-DFS with a different initial
; state or with an initial path.

; Examples of calls to some of the helper functions can be found after the code.



; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
  (cond ((equal s '(3 3 NIL)) T) ;If final state, return true, otherwise, return false
	(t NIL)))

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; missionaries to move (m), and a number of cannibals to move (c). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
  (cond ((> m (first s)) NIL) ;if amount of missionaries is more than able to moved, return false
	((> c (second s)) NIL);if amount of cannibals is more than able to be moved, return false
	((and (> (- (second s) c) (- (first s) m)) (> (- (first s) m) 0)) NIL) ;if there are more cannibals than missionaries, and the amount of missionaries is positive, return false
	((and (> (- 3 (- (second s) c)) (- 3 (- (first s) m))) (> (- 3 (- (first s) m)) 0)) NIL) ;if there are more cannibals that will be on the other side, and the amount of missionaries is positive, return false
        (t (list (list (- 3 (- (first s) m)) (- 3 (- (second s) c)) (not (third s)))))) ; otherwise return the amount of cannibals and missionaries on the other side, and the state of the other side
)

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun succ-fn (s)
  (append (if (not (equal NIL (next-state s 1 1))) (next-state s 1 1)) ; test all test cases if they dont return NIL
	(if (not (equal NIL (next-state s 0 1))) (next-state s 0 1)) ; test cases can't have missionaries + cannibals be more than 2
	(if (not (equal NIL (next-state s 1 0))) (next-state s 1 0))
	(if (not (equal NIL (next-state s 0 2))) (next-state s 0 2))
	(if (not (equal NIL (next-state s 2 0))) (next-state s 2 0))
)
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.
(defun on-path (s states)
  (cond ((or (null states) (equal (length states) 0)) NIL) ;if states is null or empty, return nil
	((equal s (first states)) t) ;;if the current state is on the top of the list of states, return true
	(t (on-path s (rest states)))) ;otherwise look at the rest of the list

 )

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states to the last state on that stack (states). states is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun mult-dfs (states path)
  (cond ((or (null states) (equal (length states) 0)) NIL) ;if states is null or empty, return nil
	((mc-dfs (first states) path) (mc-dfs (first states) path)) ; see if the first state returns something with mc-dfs, if so use that result
	(t (mult-dfs (rest states) path))  ; otherwise, run this function on the rest of the states
))

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun mc-dfs (s path)
  (cond ((final-state s) (append path (list s))) ; if this is the final state, append the state and return the path
	((on-path s path) NIL) ; if the state is on the path, it has been visited so return NIL (no solution)
	(t (mult-dfs (succ-fn s) (append path (list s)))) ;otherwise look at all of the next states and add the current state to the path
))



; Function execution examples

; Applying this operator would result in an invalid state, with more cannibals
; than missionaries on the east side of the river.
; (next-state '(3 3 t) 1 0) -> NIL

; Applying this operator would result in one cannibal and zero missionaries on
; the west side of the river, which is a legal operator. (NOTE that next-state
; returns a LIST of successor states, even when there is only one successor)
; (next-state '(3 3 t) 0 1) -> ((0 1 NIL))

; succ-fn returns all of the legal states that can result from applying
; operators to the current state.
; (succ-fn '(3 3 t)) -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
; (succ-fn '(1 1 t)) -> ((3 2 NIL) (3 3 NIL))
