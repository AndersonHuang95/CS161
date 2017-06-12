;
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
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

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

;; Helper function for goal-test 
;; Checks each row for boxes 
;; using the isBox function to check each
;; individual cell 
(defun areBoxesInRow (row)
	(cond
		; base case - end of list 
		((null row) nil)

		; base case - found a box 
		((isBox (car row)) t)

		; recursive case - keep searching
		(t (areBoxesInRow (cdr row)))
	)
)

; EXERCISE: Modify this function to return true (t)
; [DONE]
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
;; s is a list of lists 
;; we iterate over each list individually 
;; and seek out any individual boxes 
;; that have not been moved onto stars 
;; this function assumes that s is a valid problem state 
(defun goal-test (s)
	(cond
		; Base case - we've gone through the entire state
		((null s) t)

		; Recursive case - we still have lists to examine
		; an individual box exists, therefore cannot be a goal state 
		((areBoxesInRow (car s)) nil)

		; check the next row 
		(t (goal-test (cdr s)))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions for next-states 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-square
;; [Purpose] 
;;	returns integer constant of whatever
;; 	object is at cell (r, c) in state S 
;; [Inputs]
;;	S (state): a list of lists, referencing a problem state
;; 	r (row): row # of cell in question
;; 	c (column): column # of cell in question
;; [Output] 
;;	an integer constant denoting type of object at cell (r, c)
;; 	if (r, c) is out of range, then the integer constant 
;;	of a wall object is returned, which is equal to 1
(defun get-square (S r c)
	(cond
		; base cases 
		; (r, c) has brought us out of range 
		; this can happen when (r,c) is greater than the width and height of the grid
		((or (null S) (null (car S))) 1)

		; it can also happen when there are no walls on the edges of the grid 
		; which gives rise to negative (r, c) through recursion
		((or (< r 0) (< c 0)) 1)

		; we have arrived at (r,c) through recursion 
		((and (= r 0) (= c 0)) (caar S))

		; advance rows until r == 0 
		((> r 0) (get-square (cdr S) (- r 1) c))

		; advance columns until c == 0
		; (cdar x) == (cdr (car x))
		; (cddr x) == (cdr (cdr x))
		; by cons-ing the above two expressions, we arrive at 
		; a state S', which is S with its first column removed 
		((> c 0) (get-square (cons (cdar S) (cdr S)) r (- c 1)))
	)
)

;; set-square
;; [Purpose] 
;;	Returns a modified state 
;; 	with (r, c) set to content v
;; [Inputs]
;;	S (state): a list of lists, referencing a problem state
;; 	r (row): row # of cell in question
;; 	c (column): column # of cell in question
;; 	v (value): integer value of an object 
;; [Output] 
;; 	a state S'
;; 	if (r,c) is out of range, no changes are made 
(defun set-square (S r c v)
	(cond
		; base cases 
		; (r, c) has brought us out of range 
		((or (null S) (null (car S))) S)

		; we have arrived at (r,c) through recursion 
		; replace caar S with v
		; then slap on the rest of the list 
		((and (= r 0) (= c 0)) (cons (cons v (cdar S)) (cdr S)))

		; advance rows until r == 0 
		; cons the row we just advanced past to 
		; the result of recursing through following rows 
		((> r 0) (cons (car S) (set-square (cdr S) (- r 1) c v)))

		; advance columns until c == 0
		; cons the column we are advance past to 
		; the result of recursing through following columns
		((> c 0) (let ((tail (set-square (cons (cdar S) (cdr S)) r (- c 1) v)))
			(cons (cons (caar S) (car tail)) (cdr tail))
		))
	)
)

;; modify-state 
;; [Purpose]
;; 	Helper function for try-move 
;; 	Generalizes a return state given the two cells following 
;; 	the direction of movement given to try-move 
;; [Inputs]
;; 	S (state): list of lists referencing problem state
;; 	x (keeper row): 0-indexed row in state
;; 	y (keeper column): 0-indexed column in state 
;; 	k (keeper content): content at keeper cell (3 or 6)
;; 	c1 (content 1): content of cell one direction of movement away from keeper
;; 	c2 (content 2): content of cell two directions of movement away from keeper
;; [Output]
;; 	a modified State S', or NIL
(defun modify-keeper-state (S x y k c1 c2)
	(cond 
		; In these scenarios, we are only concerned with 
		; moving the keeper... The only invalid states 
		; that may arise are the following 
		; 1. Keeper walks into a wall -> return NIL 
		; 2. Keeper pushes a box, but the cell following the 
		; direction of movement IS NOT a goal or empty square -> return NIL 

		((isWall c1) NIL)
		((and (isBox c1) (not (or (isStar c2) (isBlank c2)))) NIL)

		; Otherwise, we can move the keeper
		((equal k keeper) (set-square S x y blank))
		((equal k keeperstar) (set-square S x y star)) 
	)
)

;; try-move
;; [Purpose] 
;;	Returns a modified state 
;; 	by moving S in direction D 
;; [Inputs]
;;	S (state): a list of lists, referencing a problem state
;; 	D (direction): one of four directions, 'u, 'd, 'l, 'r
;; [Output] 
;; 	a state S'
;; 	or NIL if an invalid state was reached
;; Notice how we have to switch the y and x coordinates 
;; because getKeeperPosition returns the tuple in reverse order!
(defun try-move (S D)
	; get the keeper position, (x,y)
	; and the content at (x,y)
	(let* ((pos (getKeeperPosition S 0))
		(y (car pos))
		(x (cadr pos))
		(k (get-square S x y)))

		(cond
			; TODO: Worry about nulls? 
			; <dir>1, <dir>2 specify contents of two cells, respectively 
			; Sk is a temporary modified state, with the keeper moved already 

			; Only 4 objects can possibly be adjacent to the keeper 
			; 1. blank 
			; 2. box 
			; 3. goal 
			; 4. box + goal 
			; Determine modified state based on content types of keeper, <dir>1, <dir>2

			; up
			((equal D 'u) (let* ((up1 (get-square S (- x 1) y)) (up2 (get-square S (- x 2) y)) (Sk (modify-keeper-state S x y k up1 up2)))
				(cond
					((isBlank up1) (set-square Sk (- x 1) y keeper))
					((and (isBox up1) (isBlank up2)) (set-square (set-square Sk (- x 1) y keeper) (- x 2) y box))
					((and (isBox up1) (isStar up2)) (set-square (set-square Sk (- x 1) y keeper) (- x 2) y boxstar))
					((isStar up1) (set-square Sk (- x 1) y keeperstar))
					((and (isBoxStar up1) (isBlank up2)) (set-square (set-square Sk (- x 1) y keeperstar) (- x 2) y box))
					((and (isBoxStar up1) (isStar up2)) (set-square (set-square Sk (- x 1) y keeperstar) (- x 2) y boxstar))
				)
			))

			; down 
			((equal D 'd) (let* ((down1 (get-square S (+ x 1) y)) (down2 (get-square S (+ x 2) y)) (Sk (modify-keeper-state S x y k down1 down2)))
				(cond
					((isBlank down1) (set-square Sk (+ x 1) y keeper))
					((and (isBox down1) (isBlank down2)) (set-square (set-square Sk (+ x 1) y keeper) (+ x 2) y box))
					((and (isBox down1) (isStar down2)) (set-square (set-square Sk (+ x 1) y keeper) (+ x 2) y boxstar))
					((isStar down1) (set-square Sk (+ x 1) y keeperstar))
					((and (isBoxStar down1) (isBlank down2)) (set-square (set-square Sk (+ x 1) y keeperstar) (+ x 2) y box))
					((and (isBoxStar down1) (isStar down2)) (set-square (set-square Sk (+ x 1) y keeperstar) (+ x 2) y boxstar))
				)
			))

			; left 
			((equal D 'l) (let* ((left1 (get-square S x (- y 1))) (left2 (get-square S x (- y 2))) (Sk (modify-keeper-state S x y k left1 left2)))
				(cond 
					((isBlank left1) (set-square Sk x (- y 1) keeper))
					((and (isBox left1) (isBlank left2)) (set-square (set-square Sk x (- y 1) keeper) x (- y 2) box))
					((and (isBox left1) (isStar left2)) (set-square (set-square Sk x (- y 1) keeper) x (- y 2) boxstar))
					((isStar left1) (set-square Sk x (- y 1) keeperstar))
					((and (isBoxStar left1) (isBlank left2)) (set-square (set-square Sk x (- y 1) keeperstar) x (- y 2) box))
					((and (isBoxStar left1) (isStar left2)) (set-square (set-square Sk x (- y 1) keeperstar) x (- y 2) boxstar))
				)
			))

			; right
			((equal D 'r) (let* ((right1 (get-square S x (+ y 1))) (right2 (get-square S x (+ y 2))) (Sk (modify-keeper-state S x y k right1 right2)))
				(cond
					((isBlank right1) (set-square Sk x (+ y 1) keeper))
					((and (isBox right1) (isBlank right2)) (set-square (set-square Sk x (+ y 1) keeper) x (+ y 2) box))
					((and (isBox right1) (isStar right2)) (set-square (set-square Sk x (+ y 1) keeper) x (+ y 2) boxstar))
					((isStar right1) (set-square Sk x (+ y 1) keeperstar))
					((and (isBoxStar right1) (isBlank right2)) (set-square (set-square Sk x (+ y 1) keeperstar) x (+ y 2) box))
					((and (isBoxStar right1) (isStar right2)) (set-square (set-square Sk x (+ y 1) keeperstar) x (+ y 2) boxstar))
				)
			))
		)
	)
)

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
; [DONE]
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;
(defun next-states (s)
    (cleanUpList (list (try-move s 'u) (try-move s 'd) (try-move s 'l) (try-move s 'r)))
)

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
; [DONE]
;
(defun h0 (s)
	0
)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
; [DONE]
; Function is very similar to goal-test
; but continues to run until complete problem state is 
; examined, instead of early termination in the case 
; of goal-test function
;
; This heuristic is admissible as it most certainly does not overestimate
; the cost to a goal state. At any time, if any boxes are misplaced they will
; have to be moved onto goal spots. In the best case, those goals are adjacent
; to those goals. This requires (# of misplaced boxes) moves. However, in the 
; general case, the goals are much farther away, and may require some maneuvering
; of other boxes already in goals, moving around walls, etc. 
(defun h1 (s)
	(cond
		; Base case - end of state
		((null s) 0)

		; Recursive case - end of a row 
		((null (car s)) (h1 (cdr s)))

		; Recursive case - in the middle of a row
		; Add one to box count if a box is found 
		; otherwise, continue searching 
		((list (car s)) (if (isBox (caar S)) (+ 1 (h1 (cons (cdar S) (cdr S)))) (h1 (cons (cdar S) (cdr S)))))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions for heuristic 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; findKeeperPosition
;; [Purpose]
;;	Finds the keeper position 
;; [Inputs]
;;	S (state): problem state
;;	r (row): row count
;; 	c (column): column count
;; [Output]
;; 	The (r, c) tuple, as a list, where the keeper is
;; 	Note: This function was used instead because
;; 	given function in the assignment returned a 
;; 	reverse tuple, that is (c,r) instead of (r,c) 
;; 	where r is the 0-indexed row, and c is the row-indexed column
(defun findKeeperPosition (S r c)
	(cond
		; end of state -> no keeper (shouldn't be possible)
		((null S) '())

		; end of row
		((null (car S)) (findKeeperPosition (cdr S) (+ r 1) 0))

		; middle of row
		; return immediately if keeper is found
		; otherwise keep searching
		((list (car S)) (if (or (isKeeper (caar S)) (isKeeperStar (caar S))) (list r c) (findKeeperPosition (cons (cdar S) (cdr S)) r (+ c 1)))) 
	)
)

;; findGoalPositions
;; [Purpose]
;;	Finds all cells that have goals 
;; [Inputs]
;;	S (state): problem state
;;	r (row): row count
;; 	c (column): column count
;; [Output]
;;	List of all cells, in tuple form, where goals are
;; 	Note: the list will contain a NIL element that you will
;; 	manually have to clean up with cleanUpList
(defun findGoalPositions (S r c)
	(cond
		; end of state 
		((null S) '())

		; end of row
		((null (car S)) (findGoalPositions (cdr S) (+ r 1) 0))

		; middle of row
		((list (car S)) (if (or (isStar (caar S)) (isKeeperStar (caar S))) (append (list (list r c)) (findGoalPositions (cons (cdar S) (cdr S)) r (+ c 1))) (findGoalPositions (cons (cdar S) (cdr S)) r (+ c 1))))
	)
)

;; findBoxPositions
;; [Purpose]
;;	Finds all cells that have boxes
;; [Inputs]
;;	S (state): problem state
;;	r (row): row count
;; 	c (column): column count
;; [Output]
;;	List of all cells, in tuple form, where boxes are
;; 	Note: the list will contain a NIL element that you will
;; 	manually have to clean up with cleanUpList
(defun findBoxPositions (S r c)
	(cond
		; end of state 
		((null S) '())

		; end of row
		((null (car S)) (findBoxPositions (cdr S) (+ r 1) 0))

		; middle of row
		((list (car S)) (if (isBox (caar S)) (append (list (list r c)) (findBoxPositions (cons (cdar S) (cdr S)) r (+ c 1))) (findBoxPositions (cons (cdar S) (cdr S)) r (+ c 1))))
	)
) 

;; minElement
;; [Purpose]
;;	similar to min, but operates on a list
;; [Inputs]
;; 	list (list)
;; 	x (integer): a large integer, normally most-positive-fixnum
;; [Output]
;; 	smallest element in list
(defun minElement (list x)
	(cond
		((null list) x)
		((< (car list) x) (minElement (cdr list) (car list)))
		(t (minElement (cdr list) x))
	)
)


;; manhattan between two tuples
(defun tupleManhattanDistance (source dest)
	(cond
		((or (null source) (null dest)) 0)
		(t (+ (abs (- (car dest) (car source))) (abs (- (cadr dest) (cadr source)))))
	)
)

;; ManhattanDistance
;; [Purpose] 
;; 	Returns manahattan distance between a source and a list of objects 
;; [Inputs]
;;	source: a tuple denoting where source is
;; 	list: a list of tuples denoting object positions
;; [Output]
;;	a list of distances
(defun ManhattanDistance (source list)
	(cond
		; end of list 
		((null list) '())

		; still elements to examine
		(t (let ((dest (car list))) 
			(append (list (+ (abs (- (car dest) (car source))) (abs (- (cadr dest) (cadr source))))) (ManhattanDistance source (cdr list)))
		))
	)
)

;; list-sum
(defun list-sum (list)
	(cond
		((null list) 0)
		(t (+ (car list) (list-sum (cdr list))))
	)
)

;; ...
;; [Purpose]
;;	Return a (r,c) tuple of closest goal to box 
;; [Inputs]
;;	box (tuple)
;; 	goal-list (list of tuples)
;; 	goal (tuple) 
;; [Output]
;; 	tuple 
(defun closestXToY(source dest-list dest)
	(cond
		((null goal-list) dest)
		((< (tupleManhattanDistance	box (car dest-list)) (tupleManhattanDistance source dest)) (closestXToY source (cdr dest-list) (car dest-list)))
		(t (closestXToY source (cdr dest-list) dest))
	)
)

;; boxGoalManhattanSum
;; [Purpose]
;;	Calculates the minimum distances between a box and 
;; 	its closest goal, then sums all these together 
;; [Input]
;;	box-list
;;	goal-list
;; [Output]
;;	See purpose above 
(defun boxGoalManhattanSum (box-list goal-list)
	(cond 
		; end of box-list 
		((null box-list) 0)

		; still boxes to cross-examine with goals 
		(t (let ((source (car box-list)))
			(+ (minElement (ManhattanDistance source goal-list) most-positive-fixnum) (boxGoalManhattanSum (cdr box-list) goal-list))
		))
	)
)

;; Second try at a heuristic 
(defun boxGoalMatchingManhattanSum (box-list goal-list)
	(cond
		((null box-list) 0)

		(t (+ (minElement (ManhattanDistance (car box-list) goal-list) most-positive-fixnum) 
			(tupleManhattanDistance (car box-list) (closestXToY (car box-list) goal-list (car goal-list)))
			(boxGoalMatchingManhattanSum (cdr box-list) goal-list)
			)
		)
	)
)

;; Another helper function for my heuristic
;; Determines if a box is stuck 
(defun isBoxBlocked (S box)
	(let ((x (car box)) (y (cadr box)))
		(cond 
			((and (isWall (get-square S (- x 1) y)) (isWall (get-square S x (- y 1)))) t)
			((and (isWall (get-square S (- x 1) y)) (isWall (get-square S x (+ y 1)))) t)
			((and (isWall (get-square S (+ x 1) y)) (isWall (get-square S x (- y 1)))) t)
			((and (isWall (get-square S (+ x 1) y)) (isWall (get-square S x (+ y 1)))) t)
		)
	)
)

;; Yet another helper function for heuristic 
;; Determines if any box is blocked in a lsit
(defun anyBoxBlocked (S box-list)
	(cond 
		((null box-list) 0)
		(t (if (isBoxBlocked S (car box-list)) 1000 (anyBoxBlocked S (cdr box-list))))
	)
)

; EXERCISE: Change the name of this function to h<UID> where
; [DONE]
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
; Note: findKeeperPosition is used instead of getKeeperPosition
; and returns (r,c) instead of the reversed (c,r) format 
; The heuristic grades states based on how close boxes are to their
; nearest goal, how close the keeper is to all boxes, and lastly on 
; where the box is stuck. We never want the box to get stuck!
(defun h104299133 (s)
	(let ((k-pos (findKeeperPosition s 0 0)) (box-list (cleanUpList (findBoxPositions s 0 0))) (goal-list (cleanUpList (findGoalPositions s 0 0))))
		; This works slightly better than only boxes & goals 
		(+ (boxGoalManhattanSum box-list goal-list) (list-sum (ManhattanDistance k-pos box-list)) (anyBoxBlocked s box-list))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
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
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

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
