(load "/Users/ruoran/Dropbox/EECS543/piap/pa3/task3/auxfns.lisp")
(load "/Users/ruoran/Dropbox/EECS543/piap/pa3/task3/othello.lisp")


(defparameter *smart-weights*
  '#(0   0   0  0  0  0  0   0   0 0
     0 120 -20 20  5  5 20 -20 120 0
     0 -20 -40 -5 -5 -5 -5 -40 -20 0
     0  20  -5 15  3  3 15  -5  20 0
     0   5  -5  3  3  3  3  -5   5 0
     0   5  -5  3  3  3  3  -5   5 0
     0  20  -5 15  3  3 15  -5  20 0
     0 -20 -40 -5 -5 -5 -5 -40 -20 0
     0 120 -20 20  5  5 20 -20 120 0
     0   0   0  0  0  0  0   0   0 0))

(defparameter *dumb-weights*
  '#(0   0   0  0  0  0  0   0   0 0
     0 -120 20 -20  -5  -5 -20 20 -120 0
     0 20 40 5 5 5 5 40 20 0
     0  -20  5 -15  -3  -3 -15  5  -20 0
     0   -5  5  -3  -3  -3  -3  5   -5 0
     0   -5  5  -3  -3  -3  -3  5   -5 0
     0  -20  5 -15  -3  -3 -15  5  -20 0
     0 20 40 5 5 5 5 40 20 0
     0 -120 20 -20  -5  -5 -20 20 -120 0
     0   0   0  0  0  0  0   0   0 0))

(defparameter *equal-weights*
  '#(0   0   0  0  0  0  0   0   0 0
     0   1   1  1  1  1  1   1   1 0
     0   1   1  1  1  1  1   1   1 0
     0   1   1  1  1  1  1   1   1 0
     0   1   1  1  1  1  1   1   1 0
     0   1   1  1  1  1  1   1   1 0
     0   1   1  1  1  1  1   1   1 0
     0   1   1  1  1  1  1   1   1 0
     0   1   1  1  1  1  1   1   1 0
     0   0   0  0  0  0  0   0   0 0))



;(setf *player-weights*
;      (acons black
;	     (acons black *smart-weights* NIL)
;	     (acons white (acons white *equal-weights* NIL) NIL)))


(setf *player-weights*
      (acons black *equal-weights* (acons white *dumb-weights* NIL)))

(defun get-weights (player) 
  (rest (assoc player *player-weights*)))

(defun weighted-squares (player board)
  "Sum of the weights of player's squares minus opponent's."
  (let ((opp (opponent player)))
    (loop for i in all-squares
       when (eql (bref board i) player) 
       sum (aref (get-weights player) i)
       ;do (format t "~%~a" (equalp (get-weights black) *equal-weights*))
       when (eql (bref board i) opp)
       sum (- (aref (get-weights opp) i))
       ;do (format t "~%~a" (equalp (get-weights white) *dumb-weights*)))))
