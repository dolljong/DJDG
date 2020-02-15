;-------------------------------------
; Program : HSTIFF
;           Draw Horizontal stiffner
;           Yi Suk Jong
;           04/06/14
;-------------------------------------
; 1-row : 0.2b , 2-row : 0.14b , 0.36b
(defun c:HSTIFF(
		/ p1 p2 dist p3 nrow b th ulb temp ang vang pnt11 pnt21 pnt12 pnt22 pnt32 pnt42
		)
  (setq p1 (getpoint "\nPick first point: "))
  (setq p2 (getpoint p1 "\nPick second point: "))
  (setq dist (distance p1 p2))  
  (princ "\nHeight of Web is")(princ dist)
  (setq p3 (getpoint "\nPick side: "))
  (setq nrow (getint "\nRow Number (1 or 2): "))
  (setq b (getint "\nWidth of rib(mm): "))
  (setq th (getint "\nThickness of rib(mm): "))
  (initget "Upper Lower Both")
  (setq ulb (getkword "\nSelect Upper/Lower/Both: "))
  (if (< (cadr p1) (cadr p2))
    (setq temp p1 p1 p2 p2 temp)
  );if  


  (setq ang (angle p1 p2))
  (setq vang (v_angle p1 p2 p3))
  (cond
    ((= nrow 1)
      (setq pnt11 (polar p1 ang (* 0.2 dist)))  ;upper when 1 row
      (setq pnt21 (polar p2 (+ ang pi) (* 0.2 dist)))
      (push-os)
      (cond
	((= ulb "Upper") (djdg_rect pnt11 vang b th 0))
	((= ulb "Lower") (djdg_rect pnt21 vang b th 0))
	((= ulb "Both") (djdg_rect pnt11 vang b th 0) (djdg_rect pnt21 vang b th 0))
      )	;cond
      (pop-os)
    );sub cond
    ((= nrow 2)
      (setq pnt12 (polar p1 ang (* 0.14 dist)))  ;upper when 1 row
      (setq pnt22 (polar p1 ang (* 0.36 dist)))
      (setq pnt32 (polar p2 (+ ang pi) (* 0.36 dist)))
      (setq pnt42 (polar p2 (+ ang pi) (* 0.14 dist)))
      (push-os)
      (cond
	((= ulb "Upper") (djdg_rect pnt12 vang b th 0)(djdg_rect pnt22 vang b th 0))
	((= ulb "Lower") (djdg_rect pnt32 vang b th 0)(djdg_rect pnt42 vang b th 0))
	((= ulb "Both") (djdg_rect pnt12 vang b th 0)(djdg_rect pnt22 vang b th 0)
	                (djdg_rect pnt32 vang b th 0)(djdg_rect pnt42 vang b th 0)
	);sub cond
      )	;cond
      (pop-os)
    );sub cond
  );cond  
); defun  HSTIFF

