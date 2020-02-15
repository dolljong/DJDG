;---------------------------
; Program : TRIM2
;           Trim
;           Yi Suk Jong
;           04/03/30
;---------------------------
(defun c:TRIM2(
		/ ess pnt1 pnt2 plist tss nss ndo index entname ent intpnt trimep 
	       )
  (princ "\nSelect Cutting Endges..")
  (setq ess (ssget ))   ;edge selection set
  (setq pnt1 (getpoint "\n Pick Start point: "))
  (setq pnt2 (getpoint pnt1 "\n Pick End point: "))
  (setq plist (list pnt1 pnt2))
  (setq tss (ssget "F" plist))
  (setq nss (sslength tss))
  (setq ndo  (fix (/ nss 2.0)))
  (if (= (rem nss 2.0) 1) (setq ndo (1+ ndo)))

  (setq index 0)
  (push-os)
  (command "TRIM" ess "")
  (repeat ndo
    (setq entname (ssname tss index ))
    (setq ent (entget entname))
    (setq intpnt (inters pnt1 pnt2 (cdr (assoc 10 ent)) (cdr (assoc 11 ent))))
    (setq trimep (append (list entname) (list intpnt)));trim entity & point
    (command trimep)
    (setq index (+ 2 index))
  );repeat
  (command)
  (pop-os)
)  


;---------------------------
; Program : TRIMS
;           Trim Side
;           Yi Suk Jong
;           04/04/29
;---------------------------
(defun c:TRIMS(
	       / ente entn p2 p1 p2 ang ang90 ipnt angs plist tss nss
	         index entname ent pnt1 pnt2 vang1 vang2 
	       )
  (initget "2point")
  (setq sel (entsel "\nSelect Cutting Endge or [2point]: "))
  (if (= sel "2point")
    (progn
      (setq p1 (getpoint "\nPick start point: ")
	    p2 (getpoint p1 "\nPick end point: "))
      (setq p3 (getpoint "\nPick side point: "))         
    );if true
    (progn
      (setq ente (entget (car sel)))   ;edge selection
      (setq entn (cdr (assoc -1 ente)))   ;entity name of edge line
      (setq p3 (getpoint "\nPick side point: "))   
  
      (setq p1 (cdr (assoc 10 ente))
            p2 (cdr (assoc 11 ente)))
    );;if false
  );if  
  (setq ang (angle p1 p2))
  (setq ang90 (+ ang (* 0.5 pi)))
  (setq ipnt (inters p1 p2 p3 (polar p3 ang90 100) nil))
  (setq angs (angle ipnt p3))   ;angle of side point

  (setq plist (list p1 p2))
  (setq tss (ssget "F" plist))
  (setq nss (sslength tss))

  (setq index 0)

  (repeat nss
    (setq entname (ssname tss index ))
    (setq ent (entget entname))
    (setq pnt1 (cdr (assoc 10 ent))         ;start point
          pnt2 (cdr (assoc 11 ent)))        ;end point
    
    (if (inters p1 p2 pnt1 pnt2)
      (progn
	(setq intpnt (inters pnt1 pnt2 p1 p2))  ;intersection point
        (setq vang1 (v_angle p1 p2 pnt1)        ;angle to start point
	      vang2 (v_angle p1 p2 pnt2))       ;angle to end point
	(if (<= (abs (dang vang1 angs)) (abs (dang vang2 angs)))
          (setq old (assoc 10 ent)
		new (cons 10 intpnt))                      ;modify start point
          (setq old (assoc 11 ent)
		new (cons 11 intpnt))                      ;modify end point
	);if
	(setq ent (subst new old ent)) 
	(entmod ent)
      );progn
    );if  
    (setq index (1+ index))
  );repeat
);defun
