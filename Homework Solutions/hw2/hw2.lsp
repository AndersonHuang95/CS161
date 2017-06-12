;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

;; BFS 
;; [Purpose] 
;; 	Returns a sequence of leaf nodes for a LISP 
;; 	tree, in the order they are visisted according to 
;; 	breadth-first serach 
;; [Inputs]
;;	FRINGE (tree): a LISP tree, represented as a nested list 
;; [Output] 
;;	A list of leaf nodes
(defun BFS (FRINGE)
	(cond 
		; Base case - if FRINGE is atom, it is either empty list 
		; or a single leaf node 
		((null FRINGE) '())
		((atom FRINGE) (list FRINGE))

		; Recursive case
		; If FRINGE is a list, we recursively call BFS on it
		; depending on whether or not its car is a list or is an atom 
		; The rationale of flipping the car and cdr is that at each level, 
		; we want to evaluate all atoms before further expanding the lists 
		; at the next level 
		((listp FRINGE) 
			(cond 
				; the car of the FRINGE is a list 
				((listp (car FRINGE)) (BFS (append (cdr FRINGE) (car FRINGE))))

				; the car of the FRINGE is an atom 
				(t (cons (car FRINGE) (BFS (cdr FRINGE))))
			)
		)
	)	
)

;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;


; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody 
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call 
; (DFS '(NIL NIL NIL NIL) NIL) 
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.
(defun FINAL-STATE (S)
    (equal S '(T T T T))
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poison and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).
; ASSUMES VALID STATE TO BEGIN WITH!
(defun NEXT-STATE (S A)
	(cond
		; Check that baby is not on the same side as dog or poison, if homer is moving 
		; away from baby 
		; assuming valid state was a precondition, baby is safe if homer moves to baby 
		((equal A 'h) 
			(cond
				((and (equal (first S) (second S)) (not (equal (second S) (third S))) (not (equal (second S) (fourth S)))) (list (list (not (first S)) (second S) (third S) (fourth S))))
				((not (equal (first S) (second S))) (list (list (not (first S)) (second S) (third S) (fourth S))))
				(T NIL)
			)

		)

		; first check that homer and baby are on same side
		; Extra checks are not needed 
		; since homer will be with the baby regardless which side 
		((equal A 'b) 
			(if (equal (first S) (second S))
				(list (list (not (first S)) (not (second S)) (third S) (fourth S)))
				NIL
			)
		)

		; homer and dog are moving
		; first check that homer and dog are on same side
		; if homer is moving away from baby, check that poison is not with baby (no need to check dog because it moves with homer)
		; assuming valid state was a precondition, baby is safe if homer moves to baby 
		((equal A 'd)  
			(if (and (equal (first S) (third S)) (not (equal (second S) (fourth S))))
				(list (list (not (first S)) (second S) (not (third S)) (fourth S)))
				NIL
			)
		)

		; homer and poison are moving 
		; first check that homer and poison are on same side
		; if homer is moving away from baby, check that dog is not with baby (no need to check poison because it moves with homer)
		; assuming valid state was a precondition, baby is safe if homer moves to baby 
		((equal A 'p) 
			(if (and (equal (first S) (fourth S)) (not (equal (second S) (third S))))
				(list (list (not (first S)) (second S) (third S) (not (fourth S))))
				NIL
			)
		)
	)  
)

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun SUCC-FN (S)
    (append (NEXT-STATE S 'h) (NEXT-STATE S 'b) (NEXT-STATE S 'd) (NEXT-STATE S 'p))
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.
(defun ON-PATH (S STATES)
	(cond 
		; assumes current state is valid (meaning non-nil)
		((null STATES) NIL)
		((equal S (car STATES)) T)
		(T (ON-PATH S (cdr STATES)))
	)
)

; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun MULT-DFS (STATES PATH)
	; STATES holds all valid successor states for last state in PATH 
	(cond 
		; dead end 
		((null STATES) NIL)

		; solution found
		; return to DFS, which will signal end of mutual recursion
		((DFS (car STATES) PATH) (DFS (car STATES) PATH))

		; possible solutions exist 
		; exhaustively search, then backtrack if necessary 
		((null (DFS (car STATES) PATH)) (MULT-DFS (cdr STATES) PATH))
	)
)

; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun DFS (S PATH)
    (cond 
    	; are we done? 
    	((FINAL-STATE S) (append PATH (list S)))

    	; is DFS in a cycle? 
    	((ON-PATH S PATH) NIL)

    	; if neither, then keep searching!
    	(T (MULT-DFS (SUCC-FN S) (append PATH (list S))))
    )
)

