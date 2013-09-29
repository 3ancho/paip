
(load "student.lisp")

(setf example1 '(if the number of customers Tom gets is twice the square of 20 % of the number of ads he runs |,| and the number of ads is 45 |,| then what is customers ? ))

(setf equations '((= 100 (+ x (* y 40)))
		  (= x (* 10 y))))

(setf two-arg '((= (* 4 x) (- 100 x))))
(solve equations nil)


