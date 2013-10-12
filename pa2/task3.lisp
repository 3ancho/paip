(load "student_org.lisp")

(defvar *cont*    0)
(defvar *trial*   0)

;
; The original function of rule-based-translator used (some) function which stops calling if it takes non-nil value
; The function is modified to apply all the rules using (mapcar), and store in 'candidates' as a list
; It selects one element among the candidates, and reduces the value of *trial*, which can be interpreted as coins.
; Because *trial* value is reset by *cont*, which is the count of user's continue, the element selected by *trial* will be
; different whenever user continues the translation.
;
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

;
; translate-to-expresssion-i is modified from translate-to-expression
; (make-variable) part is moved to rule-based-tranlator for convenience
;
(defun translate-to-expression-i (words)
  "Translate an English phrase into an equation or expression."
      (rule-based-translator-i
        words *student-rules*
        :rule-if #'rule-pattern :rule-then #'rule-response
        :action #'(lambda (bindings response)
                    (sublis (mapcar #'translate-pair-i bindings)
                              response)))
      )
;
; modified by calling 'translate-to-expression-i'
;
(defun translate-pair-i (pair)
  "Translate the value part of the pair into an equation or expression."
  (cons (binding-var pair)
        (translate-to-expression-i (binding-val pair))))

;
; User interface function: it asks a question of yes/no, and return the value
;
(defun ask-user-continue ()
	(format t "~&Would you like to continue (yes or no)? ")
	(read)
)

;
; The main function of student-i
;
(defun student-i (words)
  "Solve certain Algebra Word Problems."
  (let ( (*cont* 0) (*trial* 0) )
       (while T          
          (solve-equations              ;solve the equations
            (create-list-of-equations   ;make a equation from
              (translate-to-expression-i (remove-if #'noise-word-p words))
            ) 
          )
          (if (> *trial* 0) (return nil))
          (if (equal (ask-user-continue) 'yes) ; ask question
              (progn (incf *cont*) (setf *trial* *cont*)) ; case of yes -> increase the *cont* value, and reset *trial*
              (return nil)                                ; case of no  -> stop
          )
       )
  )
)


;
; example cannot be solved
; If the statement contains a special word, such as product, time, ans so on, cannot be interpreted properly
; For example, The efficiency is the number of product divided by time and money
; could not be translated well because product and time is also used as operator usually.
;

(setf example3-1 '(what is the sum of 2 times 3 and 4 ?))
; The first example shows us the same result on the instruction
;
;CG-USER(1): (student-i example3-1)
;
;The equations to be solved are:
;   WHAT = ((2 * 3) + 4)
;
;The solution is:
;   WHAT = 10
;Would you like to continue (yes or no)? yes
;
;The equations to be solved are:
;   WHAT = (2 + 4)
;
;The solution is:
;   WHAT = 6
;Would you like to continue (yes or no)? yes
;
;The equations to be solved are:
;   WHAT = (SUM * 3)
;
;The solution is:
;Would you like to continue (yes or no)? yes
;
;The equations to be solved are:
;   WHAT = SUM
;
;The solution is:
;Would you like to continue (yes or no)? no
;NIL

(setf example3-2 '(what is the product of 2 plus 3 and 4 ?))

;
; In the example2, it gives us duplicated result, but the inner translation process is totally different
; we can check it by (trace) function
;
;CG-USER(2): (student-i example3-2)
;
;The equations to be solved are:
;   WHAT = (PRODUCT + 3)
;
;The solution is:
;Would you like to continue (yes or no)? yes
;
;The equations to be solved are:
;   WHAT = (PRODUCT + 3)
;
;The solution is:
;Would you like to continue (yes or no)? yes
;
;The equations to be solved are:
;   WHAT = (2 * 4)
;
;The solution is:
;   WHAT = 8
;Would you like to continue (yes or no)? yes
;
;The equations to be solved are:
;   WHAT = PRODUCT
;
;The solution is:
;Would you like to continue (yes or no)? yes
;
;The equations to be solved are:
;   WHAT = PRODUCT
;
;The solution is:
;Would you like to continue (yes or no)? yes
;
;The equations to be solved are:
;   WHAT + 3
;
;The solution is:
;   WHAT = 3
;NIL

(setf example3-3 '(the square footage is twice the perimeter |.| the perimeter is 20 |.| what is the square footage ?))
;
; Also, example 3 shows a similar result
;
;CG-USER(4): (student-i example3-3);
;
;The equations to be solved are:
;   (FOOTAGE * FOOTAGE) = (2 * PERIMETER)
;   PERIMETER = 20
;   WHAT = (FOOTAGE * FOOTAGE)
;
;The solution is:
;   PERIMETER = 20
;Would you like to continue (yes or no)? yes
;
;The equations to be solved are:
;   (FOOTAGE * FOOTAGE) = (2 * PERIMETER)
;   PERIMETER = 20
;   WHAT = SQUARE
;
;The solution is:
;   PERIMETER = 20
;Would you like to continue (yes or no)? yes
;
;The equations to be solved are:
;;   (FOOTAGE * FOOTAGE) = (2 * PERIMETER)
;   PERIMETER = 20
;   WHAT
;
;The solution is:
;   PERIMETER = 20
;Would you like to continue (yes or no)? yes;
;
;The equations to be solved are:
;   (FOOTAGE * FOOTAGE) = (2 * PERIMETER)
;   PERIMETER WHAT
;
;The solution is:
;Would you like to continue (yes or no)? no
;NIL

