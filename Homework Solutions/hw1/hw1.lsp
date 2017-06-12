;; CS161 
;; Homework 1 

;; PAD
;; [Purpose] 
;; 	Returns the Nth number in Padovan sequence 
;; [Inputs]
;;	N (integer): number in Padovan sequence to calculate
;; [Output] 
;; 	The Nth Padovan number, defined to be 
;; 	PAD(N) = PAD(N-2) + PAD(N-3)
;; 	By default, negative integers yield 1
(defun PAD (N)
	(cond 
		; Base case - call to PAD with integer less than 3
		((<= N 2) 1) 

		; Recursive case 
		(t (+ (PAD (- N 2)) (PAD (- N 3))))
	)
)

;; SUMS 
;; [Purpose]
;; 	Returns the number of additions required by the PAD
;; 	function (see above) to calculate Nth Padovan number 
;; 	The rationale is that each recursive call to a non-base 
;; 	case Padovan number increases the number of additions by 1
;; 	Once you call PAD(0), PAD(1), or PAD(2), the recursion stops
;; 	and no more splits occur in the recursion tree 
;; [Inputs] 
;; 	N (integer): number in Padovan sequence to examine 
;; [Output]
;; 	Number of additions to calculate Nth Padovan number 
;; 	By default, negative integers output 0 
(defun SUMS (N) 
	(cond 
		; Base case - no additions are needed 
		((<= N 2) 0)

		; Recursive case - add 1 to total additions
		; as well as any additions from recursive calls
		(t (+ 1 (SUMS (- N 2)) (SUMS (- N 3))))
	)
)

;; ANON 
;; [Purpose]
;; 	Returns an anonymized tree structure 
;; 	where all elements of the tree are 
;; 	replaced with the '?' character 
;; 	The function works by stepping over the 
;; 	list using the car and cdr functions
;; 	Elements can checked as atoms, lists, or nil
;; [Inputs] 
;; 	TREE (list): a list (possibly nested) 
;; 	to transform 
;; [Output] 	
;; 	An identical tree structure with 
;; 	every element replaced with a question mark 
(defun ANON (TREE)
	(cond 
		; Base case - if TREE is atom, it is either empty list 
		; or a singl leaf node 
		((null TREE) '())
		((atom TREE) '?)

		; Recursive case
		; If TREE is a list, we cons the car and cdr recursively 
		; the car and cdr can only be the empty list or the '?
		; atom (see base case) 
		((listp TREE) (cons (ANON (car TREE)) (ANON (cdr TREE))))
	)	
)
