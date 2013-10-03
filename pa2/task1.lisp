;;; HWK 2 Task 1
;;;
;;; Ruoran Wang & Changkyu Song
;;;

(load "student.lisp")

;;; We are using setq for testing purpose, so we don't need to re-type those inputs.
;;; We added || around puctuations
;;; We seperated number with % sign


(setq qa '(The price of a radio is 69.70 dollars |.| If this price is 15 % less than the marked price |,| find the market price))

;;;; Direct encoding
;
;radio_price = 69.70
;
;market_price * (1-15%) = radio_price
;
;market_price = 82

; CL-USER> (student qa)
; 
; The equations to be solved are:
;    PRICE = 69.7
;    PRICE = (MARKED * ((100 - 15) / 100))
;    TO-FIND = MARKET
; 
; The solution is:
;    MARKED = 81.99999
;    PRICE = 69.7
; NIL

; This result seems to be correct

(setq qb '(The number of soldiers the Russians have is one half of the number of guns they have |.| The number of guns they have is 7000 |.| What is number of soldiers they have ? ))

;;;; Direct encoding
; Russian_soldiers = Russian_guns / 2
;
; Russian_guns = 7000
;
; Russian_soldiers = 3500 

; CL-USER> (student qb)
; 
; The equations to be solved are:
;    SOLDIERS = (GUNS / 2)
;    GUNS = 7000
;    WHAT = SOLDIERS
; 
; The solution is:
;    WHAT = 3500
;    SOLDIERS = 3500
;    GUNS = 7000
; NIL

; This result seems to be correct

(setq qc '(If the number of customers Tom gets is twice the square of 20 % of the number of advertisements he runs |,| and the number of advertisements is 45 |,| and the profit Tom receives is 10 times the number of customers he gets |,| then what is the profit ?))
;;;; Direct encoding
;
; Customers = 2 * (20% * ads) * (20% ads)
;
; ads = 45
;
; profit = 10 * Customers
;
; customers = 2 * 0.2 * 45 * 0.2 * 45 = 162
; profit = 10 * 162 = 1620

; CL-USER> (student qc)
; 
; The equations to be solved are:
;    CUSTOMERS = (2 *
;                 (((20 / 100) * ADVERTISEMENTS) *
;                  ((20 / 100) * ADVERTISEMENTS)))
;    ADVERTISEMENTS = 45
;    PROFIT = (10 * CUSTOMERS)
;    WHAT = PROFIT
; 
; The solution is:
;    WHAT = 1620
;    PROFIT = 1620
;    CUSTOMERS = 162
;    ADVERTISEMENTS = 45
; NIL

; This result seems to be correct

(setq qd '(The average score is 73 |.| The maximum score is 97 |.| What is the square of the difference between the average and the maximum ?))
;;;; Direct encoding
;
; Ave = 73
; Max = 97
;
; (ave-max) * (ave-max) = 576 

; CL-USER> (student qd)
; 
; The equations to be solved are:
;    AVERAGE = 73
;    MAXIMUM = 97
;    WHAT = ((MAXIMUM - AVERAGE) * (MAXIMUM - AVERAGE))
; 
; The solution is:
;    WHAT = 576
;    MAXIMUM = 97
;    AVERAGE = 73
; NIL

; This result seems to be correct

(setq qe '(Tom is twice Mary's age |,| and Jane's age is half the difference between Mary and Tom |.| If Mary is 18 years old |,| how old is Jane ?))

;;; Direct encoding
; Mary * 2 = Tom
;
; Jane = (Tom - Mary) / 2
;
; Mary = 18
;
; Jane = ?
;
; Mary = 18
; Jane = 9
; Tom = 36

; CL-USER> (student qe)
; 
; The equations to be solved are:
;    TOM = (2 * MARY)
;    JANE = ((TOM - MARY) / 2)
;    MARY = 18
;    HOW = JANE
; 
; The solution is:
;    HOW = 9
;    JANE = 9
;    TOM = 36
;    MARY = 18
; NIL

; This result seems to be correct

(setq qf '(What is 4 + 5 * 14 / 7 ?))

; Direct encoding 4 + 5 * 14 / 7 = 14 

; CL-USER> (student qf)
; 
; The equations to be solved are:
;    WHAT = (4 + (5 * (14 / 7)))
; 
; The solution is:
;    WHAT = 14
; NIL

; This result seems to be correct

(setq qg '(x * b = c + d |.| b * c = x |.| x = b + b |.| b = 5 |.|))

; Direct encoding
; x * b = c + d
; b * c = x
; x = b + b
; b = 5
;
; x = 10
; c = 2
; b = 5
; d = 48

; CL-USER> (student qg)
; 
; The equations to be solved are:
;    (X * B) = (C + D)
;    (B * C) = X
;    X = (B + B)
;    B = 5
; 
; The solution is:
;    D = 48
;    C = 2
;    X = 10
;    B = 5
; NIL

; This result seems to be correct
