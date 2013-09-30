;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; student.lisp: Chapter 7's STUDENT program to solve algebra word problems.

(load "patmatch.lisp")

(defstruct (rule (:type list)) pattern response)

(defstruct (exp (:type list)
                (:constructor mkexp (lhs op rhs)))
  op lhs rhs)

(defun exp-p (x) (consp x))
(defun exp-args (x) (rest x))

(pat-match-abbrev '?x* '(?* ?x))
(pat-match-abbrev '?y* '(?* ?y))

(defparameter *student-rules* (mapcar #'expand-pat-match-abbrev
  '(((?x* |.|)                  ?x)
    ((?x* |.| ?y*)          (?x ?y))
    ((if ?x* |,| then ?y*)  (?x ?y))
    ((if ?x* then ?y*)      (?x ?y))
    ((if ?x* |,| ?y*)       (?x ?y))
    ((?x* |,| and ?y*)      (?x ?y))
    ((find ?x* and ?y*)     ((= to-find-1 ?x) (= to-find-2 ?y)))
    ((find ?x*)             (= to-find ?x))
    ((?x* equals ?y*)       (= ?x ?y))
    ((?x* same as ?y*)      (= ?x ?y))
    ((?x* = ?y*)            (= ?x ?y))
    ((?x* is equal to ?y*)  (= ?x ?y))
    ((?x* is ?y*)           (= ?x ?y))
    ((?x* - ?y*)            (- ?x ?y))
    ((?x* minus ?y*)        (- ?x ?y))
    ((difference between ?x* and ?y*)  (- ?y ?x))
    ((difference ?x* and ?y*)          (- ?y ?x))
    ((?x* + ?y*)            (+ ?x ?y))
    ((?x* plus ?y*)         (+ ?x ?y))
    ((sum ?x* and ?y*)      (+ ?x ?y))
    ((product ?x* and ?y*)  (* ?x ?y))
    ((?x* * ?y*)            (* ?x ?y))
    ((?x* times ?y*)        (* ?x ?y))
    ((?x* / ?y*)            (/ ?x ?y))
    ((?x* per ?y*)          (/ ?x ?y))
    ((?x* divided by ?y*)   (/ ?x ?y))
    ((half ?x*)             (/ ?x 2))
    ((one half ?x*)         (/ ?x 2))
    ((twice ?x*)            (* 2 ?x))
    ((square ?x*)           (* ?x ?x))
    ((?x* % less than ?y*)  (* ?y (/ (- 100 ?x) 100)))
    ((?x* % more than ?y*)  (* ?y (/ (+ 100 ?x) 100)))
    ((?x* % ?y*)            (* (/ ?x 100) ?y)))))

(defun match-variable (var input bindings)
  "Does VAR match input?  Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((equal input (binding-val binding)) bindings)
          (t fail))))

(defun make-binding (var val) (cons var val))

(defun binding-var (binding)
  "Get the variable part of a single binding."
  (car binding))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (cons var val)
        ;; Once we add a "real" binding,
        ;; we can get rid of the dummy no-bindings
        (if (eq bindings no-bindings)
            nil
            bindings)))

(defun student (words)
  "Solve certain Algebra Word Problems."
  (solve-equations 
    (create-list-of-equations
      (translate-to-expression (remove-if #'noise-word-p words)))))

(defun translate-to-expression (words)
  "Translate an English phrase into an equation or expression."
  (or (rule-based-translator
        words *student-rules*
        :rule-if #'rule-pattern :rule-then #'rule-response
        :action #'(lambda (bindings response)
                    (sublis (mapcar #'translate-pair bindings)
                              response)))
      (make-variable words)))

(defun translate-pair (pair)
  "Translate the value part of the pair into an equation or expression."
  (cons (binding-var pair)
        (translate-to-expression (binding-val pair))))

(defun create-list-of-equations (exp)
  "Separate out equations embedded in nested parens."
  (cond ((null exp) nil)
        ((atom (first exp)) (list exp))
        (t (append (create-list-of-equations (first exp))
                   (create-list-of-equations (rest exp))))))

(defun noise-word-p (word)
  "Is this a low-content word which can be safely ignored?"
  (member word '(a an the this number of $)))

(defun make-variable (words)
  "Create a variable name based on the given list of words"
  (first words))

(defun solve-origin (equations known)
  "Solve a system of equations by constraint propagation."
  ;; Try to solve for one equation, and substitute its value into 
  ;; the others. If that doesn't work, return what is known.
  (or (some #'(lambda (equation)
                (let ((x (one-unknown equation)))
                  (format t "~% x: ~a" x) 
                  (when x
                    (let ((answer (solve-arithmetic (isolate equation x))))
                      (solve-origin (subst (exp-rhs answer) (exp-lhs answer)
                                    (remove equation equations))
                             (cons answer known))))))
            equations)
      known))

(defun print-equations (header equations)
  "Print a list of equations."
  (format t "~%~a~{~%  ~{ ~a~}~}~%" header
          (mapcar #'prefix->infix equations)))

(defconstant operators-and-inverses
  '((+ -) (- +) (* /) (/ *) (= =)))

(defun inverse-op (op)
  (second (assoc op operators-and-inverses)))

(defun unknown-p (exp)
  (symbolp exp))

(defun in-exp (x exp)
  "True if x appears anywhere in exp"
  (or (eq x exp)
      (and (listp exp)
           (or (in-exp x (exp-lhs exp)) (in-exp x (exp-rhs exp))))))

(defun no-unknown (exp)
  "Returns true if there are no unknowns in exp."
  (cond ((unknown-p exp) nil)
        ((atom exp) t)
        ((no-unknown (exp-lhs exp)) (no-unknown (exp-rhs exp)))
        (t nil)))

(defun one-unknown (exp)
  "Returns the single unknown in exp, if there is exactly one."
  (cond ((unknown-p exp) exp)
        ((atom exp) nil)
        ((no-unknown (exp-lhs exp)) (one-unknown (exp-rhs exp)))
        ((no-unknown (exp-rhs exp)) (one-unknown (exp-lhs exp)))
        (t nil)))


(defun commutative-p (op)
  "Is operator commutative?"
  (member op '(+ * =)))

(defun solve-arithmetic (equation)
  "Do the arithmetic for the right hand side."
  ;; This assumes that the right hand side is in the right form.
  (mkexp (exp-lhs equation) '= (eval (exp-rhs equation))))

(defun binary-exp-p (x)
  (and (exp-p x) (= (length (exp-args x)) 2)))

(defun prefix->infix (exp)
  "Translate prefix to infix expressions."
  (if (atom exp) exp
      (mapcar #'prefix->infix
              (if (binary-exp-p exp)
                  (list (exp-lhs exp) (exp-op exp) (exp-rhs exp))
                  exp))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;
; Task 2 Additional functions 
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solve-equations (equations)
  " solve multiple times"
  (print-equations "The equations to be solved are:" equations)
  (print-equations "The solution is:" 
		   (let ((solved (solve equations nil)))
		     (if (every #'one-unknown solved)
			 solved
			 (solve solved nil)))))


(defun two-unknown (exp)
  " Task2, this is after on-unknown so don't need to consider atom "
  (cond ((one-unknown exp)
	 nil)
	((unknown-p exp) exp)
	((atom exp) nil)
	((and (no-unknown (exp-lhs exp)) (two-unknown (exp-rhs exp)))
	 (two-unknown (exp-rhs exp)))
	((and (one-unknown (exp-lhs exp)) (one-unknown (exp-rhs exp))) 
	 (one-unknown (exp-lhs exp)))
        ((and (two-unknown (exp-lhs exp)) (no-unknown (exp-rhs exp)))
	 (two-unknown (exp-lhs exp)))
	(t nil)))
        
(defun two-unknown-right (exp)
  " return the right most unknown var "
  (cond ((one-unknown exp)
	 nil)
	((unknown-p exp) exp)
	((atom exp) nil)
	((and (no-unknown (exp-lhs exp)) (two-unknown-right (exp-rhs exp)))
	 (two-unknown-right (exp-rhs exp)))
	((and (one-unknown (exp-lhs exp)) (one-unknown (exp-rhs exp))) 
	 (one-unknown (exp-rhs exp)))
        ((and (two-unknown-right (exp-lhs exp)) (no-unknown (exp-rhs exp)))
	 (two-unknown-right (exp-lhs exp)))
	(t nil)))


(defun two-same-unknown-p (e)
  " not nil and left = right "
  (let ((left (two-unknown e))
	(right (two-unknown-right e)))
    (and left (equal left right))))

(defun isolate (e x)
  " Modified version of isolate "
  (cond ((two-same-unknown-p e)
	 ;; Case 0: when there are two var with same symbol
	 (isolate (move-unknown-to-left e x) x))
	((eq (exp-lhs e) x)
         ;; Case I: X = A -> X = n
         e)
        ((in-exp x (exp-rhs e))
         ;; Case II: A = f(X) -> f(X) = A
         (isolate (mkexp (exp-rhs e) '= (exp-lhs e)) x))
        ((in-exp x (exp-lhs (exp-lhs e)))
         ;; Case III: f(X)*A = B -> f(X) = B/A
         (isolate (mkexp (exp-lhs (exp-lhs e)) '=
                         (mkexp (exp-rhs e)
                                (inverse-op (exp-op (exp-lhs e)))
                                (exp-rhs (exp-lhs e)))) x))
        ((commutative-p (exp-op (exp-lhs e)))
         ;; Case IV: A*f(X) = B -> f(X) = B/A
         (isolate (mkexp (exp-rhs (exp-lhs e)) '=
                         (mkexp (exp-rhs e)
                                (inverse-op (exp-op (exp-lhs e)))
                                (exp-lhs (exp-lhs e)))) x))
        (t ;; Case V: A/f(X) = B -> f(X) = A/B
         (isolate (mkexp (exp-rhs (exp-lhs e)) '=
                         (mkexp (exp-lhs (exp-lhs e))
                                (exp-op (exp-lhs e))
                                (exp-rhs e))) x))))


;;;;;;;;;
; the limitation for this program is can't solve two more vars.
;
; Also can't solve 2nd and above order equations 

(defun solve (equations known)
  "modified on solve-origin, able to solve two unknown"
  (or (some #'(lambda (equation)
                (let* ((x (one-unknown equation)) ; if x = nil, x will be left most of two unknowns, or nil
		       (x (or x (two-unknown equation)))) 
                  (when x ; if there are three vars, return nil
                    (let* ((left-is-var (isolate equation x)) ; after iso, lhs is a single var
			   (answer 
			    (if (one-unknown left-is-var)
				(solve-arithmetic (isolate equation x))
				left-is-var))) ;bind somethting to answer
		      (when answer ; proceed only when there is answer
			(solve (subst (exp-rhs answer) (exp-lhs answer)
				      (remove equation equations))
			       (cons answer known)))))))
            equations)
      known))

; limitation: (/ 1 x) will fail
; (/ x 2) works
(defun get-coefficient (exp var) 
  " exp contains no = "
  " var is a symbol, return (list prefix-of-var constant) "
  (cond ((and (atom exp) (equal exp var)) 
	 (list 1 0))
	((equal (exp-op exp) '*)
	 (cond ((and (one-unknown (exp-lhs exp)) 
		     (no-unknown (exp-rhs exp)))
		(mapcar #'(lambda(x) (* x (eval (exp-rhs exp)))) 
			(get-coefficient (exp-lhs exp) var)))
	       ((and (no-unknown (exp-lhs exp))
		     (one-unknown (exp-rhs exp)))
		(mapcar #'(lambda(x) (* x (eval (exp-lhs exp)))) 
			(get-coefficient (exp-rhs exp) var)))
	       (t 
		fail)))
	((equal (exp-op exp) '+)
	 (cond ((and (one-unknown (exp-lhs exp)) ; case one
		     (one-unknown (exp-rhs exp)))
		(mapcar #'+ 
			(get-coefficient (exp-lhs exp) var) 
			(get-coefficient (exp-rhs exp) var)))
	       ((and (no-unknown (exp-lhs exp)) ; case two
		     (one-unknown (exp-rhs exp)))
		(mapcar #'+ 
			(get-coefficient (exp-rhs exp) var)
			(list 0 (eval (exp-lhs exp)))))
	       ((and (one-unknown (exp-lhs exp)) ; case two
		     (no-unknown (exp-rhs exp)))
		(mapcar #'+ 
			(get-coefficient (exp-lhs exp) var)
			(list 0 (eval (exp-rhs exp)))))
	       (t 
		(list 0 (eval exp)))))
	((equal (exp-op exp) '-)
	 (cond ((and (one-unknown (exp-lhs exp)) ; case one
		     (one-unknown (exp-rhs exp)))
		(mapcar #'- 
			(get-coefficient (exp-lhs exp) var) 
			(get-coefficient (exp-rhs exp) var)))
	       ((and (no-unknown (exp-lhs exp)) ; case two
		     (one-unknown (exp-rhs exp)))
		(mapcar #'- 
			(list 0 (eval (exp-lhs exp))) 
			(get-coefficient (exp-rhs exp) var)))
	       ((and (one-unknown (exp-lhs exp)) ; case two
		     (no-unknown (exp-rhs exp)))
		(mapcar #'- 
			(get-coefficient (exp-lhs exp) var)
			(list 0 (eval (exp-rhs exp)))))
	       (t 
		(list 0 (eval exp)))))
	((equal (exp-op exp) '/) ; only one case we can handle e.g. (/ x 1)
	 (cond ((and (one-unknown (exp-lhs exp)) 
		     (no-unknown (exp-rhs exp)))
		(mapcar #'/ 
			(get-coefficient (exp-lhs exp) var) 
			(list (eval (exp-rhs exp)) (eval (exp-rhs exp)))))
	       (t
		fail)))
	(t ; if the operator is not supported 
	 fail)))

; example
; > (get-coefficient '(+ x (* 2 (+ 1 x))) 'x)
; (3 2)
;
; > (get-coefficient '(/ (+ 2 (* 3 x)) 3) 'x)
; (1 2/3)
	

(defun move-unknown-to-left (exp var) 
  " (first result) is coefficient of variable, (second result) is constant. "
  (cond ((one-unknown (exp-rhs exp))
	 (mkexp (combine-var (mkexp (exp-lhs exp) 
				    '- 
				    (exp-rhs exp)) var) 
		'= 
		0))
	(t
	 (mkexp (combine-var (exp-lhs exp) var) (exp-op exp) (exp-rhs exp)))))
; example:    
; > (move-unknown-to-left '(+ (* 4 x) (+ 100 x)) 'x)	
; (+ (* 5 X) 100)

  
(defun combine-var (exp var)
  " exp can not contain = "
  (let* ((result (get-coefficient exp var))
	 (co-var (first result))
	 (co-const (second result)))
    (if (zerop co-const)
	`(* ,co-var ,var)
	(mkexp `(* ,co-var ,var) '+ co-const)))); else

; example
; > (combine-var '(+ x x) 'x)
; (* 2 X)
