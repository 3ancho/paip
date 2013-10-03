;;; HWK 2 Task 3
;;;
;;; Ruoran Wang & Changkyu Song
;;;
;;; This uses student_org.lisp which is the original version of student.lisp

(load "student_org.lisp")

(setf example3-1 '(what is the sum of 2 times 3 and 4 ?))

(setf example3-2 '(what is the product of 2 plus 3 and 4 ?))

(setf example3-3 '(the square footage is twice the perimeter |.| the perimeter is 20 |.| what is the square footage ?))


;;; The code bellow are overrding the original functions

(defvar *cont*    0)
(defvar *trial*   0)

(defun rule-based-translator-i 
       (input rules &key (matcher 'pat-match) 
        (rule-if #'first) (rule-then #'rest) (action #'sublis))
  "Find the possible rules that matches input,
  and apply the action to that rules."
  (let ((candidates 
         (remove nil 
           (mapcar 
            #'(lambda (rule)
                    (let ((result (funcall matcher (funcall rule-if rule) input)))
                         (if (not (eq result fail))
                             (funcall action result (funcall rule-then rule))
                         )
                    )
              )
            rules)
        )
       ))
       (cond ((null candidates)               (make-variable input)   )
             ((< *trial* (length candidates)) (nth *trial* candidates))
             (T                               (progn (setf *trial* (- *trial* (length candidates)) )
                                                     (make-variable input))                        )
       )
  )  
)

(defun translate-to-expression-i (words)
  "Translate an English phrase into an equation or expression."
      (rule-based-translator-i
        words *student-rules*
        :rule-if #'rule-pattern :rule-then #'rule-response
        :action #'(lambda (bindings response)
                    (sublis (mapcar #'translate-pair-i bindings)
                              response)))
      )

(defun translate-pair-i (pair)
  "Translate the value part of the pair into an equation or expression."
  (cons (binding-var pair)
        (translate-to-expression-i (binding-val pair))))

(defun ask-user-continue ()
	(format t "~&Would you like to continue (yes or no)? ")
	(read)
)

(defun student-i (words)
  "Solve certain Algebra Word Problems."
  (let ( (*cont* 0) (*trial* 0) )
       (while T          
          (solve-equations 
            (create-list-of-equations
              (translate-to-expression-i (remove-if #'noise-word-p words))
            ) 
          )
          (if (> *trial* 0) (return nil))
          (if (equal (ask-user-continue) 'yes)
              (progn (incf *cont*) (setf *trial* *cont*))
              (return nil)
          )
       )
  )
)
