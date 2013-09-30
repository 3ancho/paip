
(load "student.lisp")

(setf example1 '(if the number of customers Tom gets is twice the square of 20 % of the number of ads he runs |,| and the number of ads is 45 |,| then what is customers ? ))

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

(setf three '((= 100 (+ x (+ y z)))
		  (= x (- y 10))
		  (= x z)))


(setf two-arg '((= (* 4 x) (- 100 x))))
(solve equations nil)


