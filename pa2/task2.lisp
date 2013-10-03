;;; HWK2 Task2 
;;;
;;; Ruoran Wang & Changkyu Song 


;;; This student is modified to handle two variables. 
;;; Check the bottom of this student.lisp for modified functions
(load "student.lisp")


;;; This file only contains some examples:

(setf equations '((= 100 (+ x (* y 40)))
		  (= x (* 10 y))))
; > (solve-equations equations)
; The equations to be solved are:
;    100 = (X + (Y * 40))
;    X = (10 * Y)
; 
; The solution is:
;    X = 20
;    Y = 2
; NIL


(setf example2 '(if tom's age plus lily's age is 4 |,| and tom's age minus lily's age is 0 |,| then what is tom's age ?))
;CL-USER> (student example2)
;
;The equations to be solved are:
;   (TOM + LILY) = 4
;   (TOM - LILY) = 0
;   WHAT = TOM
;
;The solution is:
;   TOM = 2
;   LILY = 2
;   WHAT = 2
;NIL

(setf two-arg '((= (* 4 x) (- 100 x))))
;CL-USER> (solve-equations two-arg)
;
;The equations to be solved are:
;   (4 * X) = (100 - X)
;
;The solution is:
;   X = 20
;NIL
