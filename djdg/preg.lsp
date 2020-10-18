;-----
; preg : points for regoin
; make region by picking points
; dolljong 201018
;----
(defun c:preg( / p1 p2 p3 p4 ss)
  (setq p1 (getpoint "pick first point: "))
  (setq p2 (getpoint p1 "Pick 2'nd point: "))
  (grdraw p1 p2 3 1)
  (setq p3 (getpoint p2 "Pick 3'rd point: "))
  (grdraw p2 p3 3 1)
  (setq p4 (getpoint p3 "Pick 4'th point: "))
  (if (= p4 nil)
    (progn
      (grdraw p3 p1 3 1)
      (entmake_line p1 p2 3)
      (setq ss (ssadd (entlast) ss))
      (entmake_line p2 p3 3)
      (setq ss (ssadd (entlast) ss))
      (entmake_line p3 p1 3)
      (setq ss (ssadd (entlast) ss))
      (redraw)
      (command "REGION" ss "")
    )
    (progn
      (grdraw p3 p4 3 1)
      (grdraw p4 p1 3 1)
      (entmake_line p1 p2 3)
      (setq ss (ssadd (entlast) ss))
      (entmake_line p2 p3 3)
      (setq ss (ssadd (entlast) ss))
      (entmake_line p3 p4 3)
      (setq ss (ssadd (entlast) ss))
      (entmake_line p4 p1 3)
      (setq ss (ssadd (entlast) ss))      
      (redraw)
      (command "REGION" ss "")      
    )
  );if    
  
);defun

;-----
; entmake_line
; input : pt1 pt2 clr : point-1, point-2, radius, color
; dolljong 201011
;-----
(defun entmake_line (pt1 pt2 clr / blg lst ocs pt1 rad )

   (setq ocs (trans '(0 0 1) 1 0 t))
   (entmake
           (append
              '(   (000 . "LINE")
                   (100 . "AcDbEntity")
                   ;(100 . "AcDbPolyline")
               )
 	       (list (cons 10 pt1))
	       (list (cons 11 pt2))
	       (list (cons 210 ocs))
	       (list (cons 62 clr))
           )
    );entmake
);defun
