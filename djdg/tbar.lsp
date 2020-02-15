;-------------------------
; Program: TBAR
;          Tie BAR
;          Yi Suk Jong
;          04/03/31
;------------------------
(defun C:TBAR(
	      / pnt1 pnt2 pnt3 dx dy lx ly ny1 irow y nx icol x
	      )
;  (setq ds (getvar "dimscale"))
  (setq pnt1 (getpoint "\nPick first point: "))
  (setq pnt2 (getcorner pnt1 "\nOther corner: "))
  (setq pnt3 (getcorner pnt1 "\nFirst Tie Bar: "))
  
  (setq dx (- (car pnt3) (car pnt1))
	dy (- (cadr pnt3) (cadr pnt1)))
  (setq lx (- (car pnt2) (car pnt1))
	ly (- (cadr pnt2) (cadr pnt1)))
  (setq ny1 (fix (/ ly dy)))
  (setq irow 0);row
  (repeat (1+ ny1)
    (setq y (+ (cadr pnt1) (* dy irow)))
    (setq nx (abs (fix (/ (- lx (* (rem irow 2) dx)) (* dx 2)))))
    (setq icol 0)
    (repeat (1+ nx)
      (setq x (+ (car pnt1) (* (rem irow 2) dx) (* dx icol 2)))
      (command "insert" (strcat (prefix) "blocks/tbar") (list x y) "1" "1" "0")
      (setq icol (1+ icol))
    );repeat	  
    (setq irow (1+ irow))
  );repeat
  
  
)  