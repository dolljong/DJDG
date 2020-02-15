;-------------------------------
; Program : Anch1
;           make inclined Anchorage center line
;           Yi Suk Jong
;           04/03/25
;-------------------------------
(defun c:anch1(
	       / sel anchpnt len r selpnt selent sp ep sdist edist endpnt ang ang90
	        ang180 ipnt1 ipnt anganch fpnt apnt ln2 lnsel1 lnsel)
  (setq sel (entsel "\nSelect tendon line: "))
  (setq anchpnt (getpoint "\nPick anchorage point: "))
  (setq len (getreal "\nLength of intersection: "))
  (setq r (getreal "\nRadius of Fillet : "))
  
  
  (setq selpnt (cadr sel))             ;select point
  (setq selent (entget (car sel)))    ;line entity
  (setq sp (cdr (assoc 10 selent))    ;start point
	ep (cdr (assoc 11 selent)))   ;end point
  (setq sdist (distance sp anchpnt)
	edist (distance ep anchpnt))
  (if (< sdist edist)
    (setq endpnt sp
	  ang (angle ep sp))
    (setq endpnt ep
	  ang (angle sp ep))
  );if
  (setq ang90 (+ ang (* pi 0.5)))   ;normal angle
  (setq ang180 (+ ang pi))
  (setq ipnt1 (inters sp ep anchpnt (polar anchpnt ang90 100) nil))
  (setq ipnt (polar ipnt1 ang180 len))   ;intersection point

;  (setq newln (cons 
;  (entmod (subst (cons 
  (setq anganch (angle ipnt anchpnt))
  

  (setq fpnt (polar ipnt ang180 10))   ; point on original line
  (setq apnt (polar ipnt anganch 10))  ;point on inclined line
  
  (push-os)(celtype "center")(cecolor "red") 
  (command "break" sel "f" endpnt ipnt)
;  (command "circle" endpnt "100")   ;for test
;  (command "circle" ipnt "100")     ;for test
  (command "line" anchpnt ipnt "")
  (setq ln2 (entlast))
  (setq lnsel1 (append (list (car sel)) (list fpnt)))
  (setq lnsel (append (list ln2) (list apnt))) 
  (command "fillet" "R" r)
  (command "fillet" lnsel1 lnsel)
  (pop-os)(pop-ltype)(popcolor)
  
  (princ)
);defun  