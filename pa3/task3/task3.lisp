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


(setf *player-weights*
      (acons black
	     (acons black *equal-weights* (acons white *smart-weights* NIL))
	     (acons white (acons white *smart-weights* (acons black *smart-weights* NIL)) NIL)))

(setf *player-weights*
      (acons black
	     (acons black *smart-weights* (acons white *smart-weights* NIL))
	     (acons white (acons white *equal-weights* (acons black *equal-weights* NIL)) NIL)))

(setf *player-weights*
      (acons black
	     (acons black *smart-weights* (acons white *equal-weights* NIL))
	     (acons white (acons white *equal-weights* (acons black *equal-weights* NIL)) NIL)))

;(setf *player-weights*
;      (acons black *equal-weights* (acons white *dumb-weights* NIL)))

(defun get-weights (player cur-player)  ; we are cur-player, what strategy we think player is using
  (rest (assoc player (rest (assoc cur-player *player-weights*)))))


(defun get-weight-name (weight) 
  " return name from vector "
  (cond ((equalp weight *smart-weights*)
	 "smart-weight")
	((equalp weight *equal-weights*)
	 "equal-weight")
	((equalp weight *dumb-weights*)
	 "dumb-weight")))
	 
(defun maximizer (eval-fn)
  "Return a strategy that will consider every legal move,
  apply EVAL-FN to each resulting board, and choose
  the move for which EVAL-FN returns the best score.
  FN takes two arguments: the player-to-move and board"
  #'(lambda (player board)
      (let* ((moves (legal-moves player board))
             (scores (mapcar #'(lambda (move)
                 (funcall
                  eval-fn
                  player
                  (make-move move player
                         (copy-board board))))
                             moves))
             (best  (apply #'max scores)))
        (elt moves (position best scores)))))

(defun equal-weighted-squares (player board)
  "Sum of the weights of player's squares minus opponent's."
  (let ((opp (opponent player)))
    (loop for i in all-squares
          when (eql (bref board i) player)
          sum (aref *equal-weights* i)
          when (eql (bref board i) opp)
          sum (- (aref *equal-weights* i)))))

(defun dumb-weighted-squares (player board)
  "Sum of the weights of player's squares minus opponent's."
  (let ((opp (opponent player)))
    (loop for i in all-squares
          when (eql (bref board i) player)
          sum (aref *dumb-weights* i)
          when (eql (bref board i) opp)
          sum (- (aref *dumb-weights* i)))))

; When calculating opponent, opp use smae weights 
(defun weighted-squares-task3 (player board cur-player)
  "Sum of the weights of player's squares minus opponent's."
  (let ((opp (opponent player)))
    (if (equal player cur-player)
	(loop for i in all-squares
	   when (eql (bref board i) player) 
	   sum (aref (get-weights player cur-player) i)
	   ;do (format t "~%~a is playing, he thinks ~a use ~a" cur-player player (get-weight-name (get-weights player cur-player)))
	   when (eql (bref board i) opp)
	   sum (- (aref (get-weights player cur-player) i))
	   ;do (format t "~%~a is playing, he thinks ~a use ~a" cur-player opp (get-weight-name (get-weights player cur-player)))
	     )
	(loop for i in all-squares  
	   when (eql (bref board i) player) 
	   sum (aref (get-weights (opponent cur-player) cur-player) i)
	   ;do (format t "~%~a is playing, he think in ~a use ~a, opp think of him self" cur-player player (get-weight-name (get-weights (opponent cur-player) cur-player)))
	   when (eql (bref board i) opp)
	   sum (- (aref (get-weights (opponent cur-player) cur-player) i))
	   ;do (format t "~%~a is playing, he think in ~a use ~a, opp think of me" cur-player opp (get-weight-name (get-weights (opponent cur-player) cur-player)))
	     ))))

; When calculating opponent, will fetch opponent's strategy 
(defun weighted-squares-task4 (player board cur-player)
  "Sum of the weights of player's squares minus opponent's."
  (let ((opp (opponent player)))
    (if (equal player cur-player)
	(loop for i in all-squares ; last step is by playing guy
	   when (eql (bref board i) player) 
	   sum (aref (get-weights player cur-player) i)
	   when (eql (bref board i) opp)
	   sum (- (aref (get-weights player cur-player) i)))
	(loop for i in all-squares   ; last step is by opp
	   when (eql (bref board i) player) 
	   sum (aref (get-weights (opponent cur-player) cur-player) i)
	   when (eql (bref board i) opp)
	   sum (- (aref (get-weights (opponent cur-player) cur-player) i))
	     ))))


; cur-player is the current playing/thinking player
; player is what if this player is playing
(defun alpha-beta (player board achievable cutoff ply eval-fn cur-player) 
  "Find the best move, for PLAYER, according to EVAL-FN,
  searching PLY levels deep and backing up values,
  using cutoffs whenever possible."
  (if (= ply 0)
      (funcall eval-fn player board cur-player)
      (let ((moves (legal-moves player board)))
        (if (null moves)
            (if (any-legal-move? (opponent player) board)
                (- (alpha-beta (opponent player) board
                               (- cutoff) (- achievable)
                               (- ply 1) eval-fn cur-player))
                (final-value player board))
            (let ((best-move (first moves)))
              (loop for move in moves do
                (let* ((board2 (make-move move player
                                          (copy-board board)))
                       (val (- (alpha-beta
                                 (opponent player) board2
                                 (- cutoff) (- achievable)
                                 (- ply 1) eval-fn cur-player))))
                  (when (> val achievable)
                    (setf achievable val)
                    (setf best-move move)))
                until (>= achievable cutoff))
              (values achievable best-move))))))

(defun alpha-beta-searcher (depth eval-fn)
  "A strategy that searches to DEPTH and then uses EVAL-FN."
  #'(lambda (player board)
      (multiple-value-bind (value move)
          (alpha-beta player board losing-value winning-value
                      depth eval-fn player) ; cur-player is the player thinking
        (declare (ignore value))
        move)))


(defun minimax (player board ply eval-fn cur-player)
  "Find the best move, for PLAYER, according to EVAL-FN,
  searching PLY levels deep and backing up values."
  (if (= ply 0)
      (progn 
      ;(format t "~%minimax last step ~a is playing" player) 
      (funcall eval-fn player board cur-player) ; not depth will return a score
      )
      (let ((moves (legal-moves player board)))
        (if (null moves) ; if this have no move
            (if (any-legal-move? (opponent player) board)
                (- (minimax (opponent player) board ; opp have move
                            (- ply 1) eval-fn cur-player))
                (final-value player board)) ; no one have move, end of game
            (let ((best-move nil) ; normal case
                  (best-val nil))
              (dolist (move moves) ; loop throught all moves, compare outcome
                (let* ((board2 (make-move move player
                                          (copy-board board)))
                       (val (- (minimax ; negate the max outcome of opp.
                                 (opponent player) board2
                                 (- ply 1) eval-fn cur-player))))
                  (when (or (null best-val)
                            (> val best-val))
                    (setf best-val val)
                    (setf best-move move))))
              (values best-val best-move)))))) ;

(defun minimax-searcher (ply eval-fn)  ; strategy will be feed into get-move,
  "A strategy that searches PLY levels and then uses EVAL-FN."
  #'(lambda (player board) ;; called by get-move , this player is the cur player
      (multiple-value-bind (value move)
          (minimax player board ply eval-fn player) ; the call for minimax return tuple (v, m)
        (declare (ignore value))
        move)))
