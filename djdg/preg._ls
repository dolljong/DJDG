;-----
; preg : points for regoin
; make region by picking points
;----
(defun c:preg()
  (setq p1 (getpoint "pick first point: "))
  (setq p2 (getpoint p1 "Pick 2'nd point: "))
  (grdraw p1 p2 3 1)
  (setq p3 (getpoint "Pick 3'rd point: "))
  (if (= p3 nil)
    (progn
    )
    (progn 
      (sstq p4 
    )
  );if    
  
);defun  