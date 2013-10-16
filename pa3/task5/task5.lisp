(load ../task3/task3.lisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Task 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *prediction* NIL)

(defun multimodel-minimax-searcher-task5 (ply eval-fn)  ; strategy will be feed into get-move,
  "A strategy that searches PLY levels and then uses EVAL-FN."
  #'(lambda (player board) ;; called by get-move , this player is the cur player
      (let ((my-move 
                (multiple-value-bind (value move)
                 (minimax-task4 player board ply eval-fn player) ; the call for minimax return tuple (v, m)
                 (declare (ignore value))
                 move)) (w (get-weights (opponent player) player)) )
        (let ((pred-move
                (multiple-value-bind (value move)
                 (minimax (opponent player) 
                          (make-move my-move (opponent player) (copy-board board)) ; board after my move
                          ply 
                          
                          #'(lambda (p b)
                          "Sum of the weights of player's squares minus opponent's."
                          (let ((opp (opponent p)))
                            (loop for i in all-squares
                                  when (eql (bref b i) p)
                                  sum (aref w i)
                                  when (eql (bref b i) opp)
                                  sum (- (aref w i)))))
                           ; the weight of oppenent player in player's thinking
                          )
                 (declare (ignore value))
                 move)))            
           (when *prediction* (format t "~%Prediction was ~s. Predicted was ~s. Actual was ~s." 
                        (if (= *prediction* my-move) "correct" "wrong") (88->h8 *prediction*) (88->h8 my-move) ))
           (setf *prediction* pred-move))
                                         
       my-move)))

