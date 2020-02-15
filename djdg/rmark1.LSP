;*************************************
; Program : RMARK1
;           Radius MARK
;           Yi Suk-Jong
;           2000/5/29
;*************************************
; Radius mark - linear line

(defun C:RMARK1(/
               txt      ln          ip      sp      ep      ang     wh4
               new_ang  ent_type    c_ent   cen_pnt
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  
  (setq oer *error* *error* seterr)

  (setq   ds   (getvar "DIMSCALE")
	th (* (getvar "DIMTXT") ds))  ;height of text 
	
  (initget "2Point")  ;use 2points for angle
  (setq arcsel (entsel "\n2Point or <Select Arc or Circle>: "))

  (if (= arcsel "2Point")
    (setq sp (getpoint "\nPick first point: ")       ;두점을 선택
          ep (getpoint sp "\nPick second point: ")
	  txt (getstring "\nEnter Text: "))          ;enter text
    (progn
      (setq ent (entget (car arcsel)))
      (setq entype (cdr (assoc 0 ent)))			;entity type

      (if (or (= entype "CIRCLE") (= entype "ARC"))
	(progn
          (setq radius (cdr (assoc 40 ent)))    ;radius of Arc
          (setq txt (strcat "R=" (rtos radius 2 3)))   ;text	  	  
	  (setq sp (cdr (assoc 10 ent)))     ;center point
;	  (setq ep (cadr arcsel))             ;select point
	  (setq ep (polar sp (angle sp (cadr arcsel)) radius))

	);progn then
	(progn
	  (alert "Entity must be Arc or Circle")
	  (exit)
	); progn else
      );if	
    );progn
  );if
  (setq ang (angle ep sp))           ;angle of base line

  

  
  (setq len (* (+ (strlen txt) 2) th))      ;length of base line
  (setq sp1 (polar ep ang len))

;  (command "LINE" sp1 ep "")
;  (command "INSERT" (strcat (prefix) "blocks/arw1") ep ds "" (rtod ang))

  (setq wh4 (which4 ang))                              ;몇사분면에 있는가?

  (cond                                                ;1~4사분면에 있을 때
     ((= wh4 1)
       (setq ang2 ang)
       (setq tang (+ ang (* pi 0.5)))
     )
     ((= wh4 2)
       (setq ang2 (- ang pi))
       (setq tang (- ang (* pi 0.5)))      
     )
     ((= wh4 3)
       (setq ang2 (- ang pi))
       (setq tang (- ang (* pi 0.5)))      
     )
     ((= wh4 4)
       (setq ang2 (- ang (* 2 pi)))
       (setq tang (+ ang (* pi 0.5)))      
     )
  );of cond

  (setq txtpnt (polar (mid-point sp1 ep) tang (* th 0.5)))  ;text point

  (setq oldosmode (getvar "OSMODE")) (setvar "OSMODE" 0)
  (setq oldcolor (getvar "CECOLOR")) (setvar "CECOLOR" "red")
  (command "LINE" sp1 ep "")
  (setvar "CECOLOR" oldcolor)  
  (command "INSERT" (strcat (prefix) "blocks/arw1") ep ds "" (rtod ang))
  (command "TEXT" "J" "C" txtpnt th (rtod ang2) txt "")
  (setvar "OSMODE" oldosmode)


);defun  
  
