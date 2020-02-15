;----------------------------------------
;program : lanemark
;          draw lane mark
;          Yi suk jong
;          00/5/19
;----------------------------------------
(defun c:lanemark(
/ lmarkwidth lmarkthick lent spnt epnt spntx spnty epntx epnty dydx
  xpnt  ipnt  side  nlane  pitch  index
)
  
  (setq lmarkwidth 300
	lmarkthick 50)					
  
  (setq lent (entget (car (entsel "\nSelect line: "))))   ;Select line
  (setq spnt (cdr (assoc 10 lent))				;start point
	epnt (cdr (assoc 11 lent)))				;end point 
  (setq spntx 	(car spnt)
	spnty (cadr spnt)
	epntx (car epnt)
	epnty (cadr epnt))				;x,y of start point and end point
  (setq dydx (/ (- epnty spnty) (- epntx spntx)))       ;slop of line
  
  (setq xpnt (getpoint "\nPick X-position: "))
  (setq ipnt (inters spnt epnt xpnt (list (car xpnt) (- (cadr xpnt) 1)) nil))  ;intersection point

  (draw_lmark  ipnt dydx lmarkwidth lmarkthick) ;draw first lane mark
  
  (setq side (car (getpoint xpnt "\nPick side: ")))      ;get side point
  (setq nlane (getint "\nEnter Number: "))    ;get number of lane
  (setq pitch (getreal "\nEnter Width of lane: ")) ;get width of lane


  (if (> side (car xpnt)) (setq sign 1) (setq sign -1))

  (setq index 1)
  (repeat nlane
    (setq x (+ (car xpnt) (* pitch sign index)))
    (setq ipnt (inters spnt epnt (list x 0) (list x 1) nil))
    (draw_lmark ipnt dydx lmarkwidth lmarkthick)
    (setq index (1+ index))
  );repeat  

  
  
);defun

;--------------------------------
; function : draw_lmark
;            draw lane mark
;            Yi suk jong
;            00/5/19
;--------------------------------
;arguments:
;  slop : slop of line
;  ipnt : insert point
;  lmwidth : width of lane mark
;  lmthick : thickness of lane mark
;------------------------------------
(defun draw_lmark(ipnt slop lmwidth lmthick
/ slop ipnt lmwidth lmthick x1 y1 x2 y2 y3 y4 p1 p2 p3 p4 oldosmode)
  (setq x1 (-  (car ipnt) (* 0.5 lmwidth))
	y1 (- (cadr ipnt) (* (* 0.5 lmwidth) slop))
	x2 (+  (car ipnt) (* 0.5 lmwidth))
	y2 (+ (cadr ipnt) (* (* 0.5 lmwidth) slop))
	y3 (+ y1 lmthick)
	y4 (+ y2 lmthick))
  (setq p1 (list x1 y1)			;lower left
	p2 (list x2 y2)                 ;lower right
	p3 (list x1 y3)   		;upper left
	p4 (list x2 y4))
	
  (setq oldosmode (getvar "osmode"))
  (setvar "osmode" 0)
  (command "solid" p1 p2 p3 p4 "")
  (setvar "osmode" oldosmode)
	

);defun  
