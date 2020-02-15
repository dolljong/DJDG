;*********************************    
; Program : DA    
;           Dimension Aligned    
;           Jong-Suk Yi    
;           1995. 3. 9, 7/5    
;*********************************    
;-------

;----------------------------------------
;  PROGRAM : LDIM
;            Line DIMension
;            Yi Suk Jong
;            04/04/03
;----------------------------------------

(defun C:DA(/    
th ds sp dsel sent pnt1 pnt2 ppnt    
ep ang1 w4 dp ang2 a2sgn tsgn fst dmdst    
dxy dst txt txt1 divl divn txtlen    
dtxt1 dtxt2 dtxt1p dtxt2p next sp1 ep1    
)    
    
  (defun SETERR(s)    
    (if (/= s "Function cancelled")    
        (princ (strcat "\nError: " s))    
    ); of If    
    (setq *error* oer seterr nil)    
    (princ)    
  ); of SETERR    
    
  (setq oer *error* *error* seterr)                     ;내장에러루틴 가동    
    
(setq th (getvar "DIMTXT")                              ; text크기    
      dim_gap (getvar "DIMDLI"))                                    ; 치수선 간격    
    
(setq ds (getvar "DIMSCALE"))    
      
(setvar "BLIPMODE" 0)    
(setvar "CMDECHO" 0)      
      
;(push-env)    
    
(initget "Object")    
(setq sp (getpoint "\nPick first point/Object: "))    
(if (= sp "Object")    
  (progn    
    (setq dsel (entsel "\nSelect Dimension Entity: "))    
    (setq sent (entget (car dsel)))    
    (setq pnt1 (cdr (assoc 13 sent)))    
    (setq pnt2 (cdr (assoc 14 sent)))    
    (setq ppnt (cadr dsel))    
    (if (> (distance ppnt pnt1) (distance ppnt pnt2))    
      (setq sp pnt2) (setq sp pnt1))                        ;선택점과 가까운 쪽    
  ) ;of progn THEN    
) ;of IF(sp=Object)    
    
(setq ep (getpoint sp "\nPick second point: "))    
(setq ang1 (angle sp ep))    
(setq w4 (which4 ang1))    
(cond                                       ;텍스트의 각도 (골뱅이 옵션)    
  ((or (= w4 1) (= w4 4)) (setq tang ang1))    
  ((or (= w4 2) (= w4 3)) (setq tang (- ang1 pi)))    
) ;of cond    
    
(setq dp (getpoint "\nPick Dimension line side: "))    
(setq ang2 (angle sp dp))    
(if (minusp (dang ang1 ang2)) (setq a2sgn -1) (setq a2sgn 1))  ;부호구함    
(setq tsgn 1)    
(if (and (or (= w4 2) (= w4 3)) (= a2sgn 1)) (setq tsgn -1))    
(if (and (or (= w4 1) (= w4 4)) (= a2sgn -1)) (setq tsgn -1))    
    
(setq fst (getint "\nDimension line Level <1>: "))       ;치수선 레벨 입력    
(if (= fst nil) (setq fst 1))    
(setq dmdst (* ds (+ 20 (* dim_gap (- fst 1)))))    
    
(while (/= ep nil)    
    
  (setq dxy (polar ep (+ (* pi 0.5 a2sgn) ang1) dmdst))    
  (setq dst (distance sp ep))    
    
  (if (< dst 1000.0)    
    (setq txt (rtos dst 2 0))                          ;1000미만일 때    
;    (setq txt (rtos (* dst 0.001) 2 3))                ;1000이상일 때    
    (setq txt (rtos_dimdsep (* dst 0.001)  3))                ;1000이상일     
    ) ;of if(dst < 1000)    
    
  (princ "\nDimension text <")                        ;Dimension text표시    
  (princ txt)    
  (setq txt1 (getstring T ">: "))                     ;새로운 dimension text입력    
  (if (= (substr txt1 1 1) "@")                                    ;골뱅이 입력시    
    (progn    
;      (setq divl (getreal "\nDivision length: "))       ;나누는 길이 입력    
      (setq divl (atof (substr txt1 2 (1- (strlen txt1)))))    
      (setq divn (/ dst divl))    
      (setq divn (rtos divn 2 0))                       ;나눈 갯수계산    
      (if (< divl 1000.)                                ;    
        (setq divl (rtos divl 2 0))    
;        (setq divl (rtos (* divl 0.001) 2 3))) ;of if    
        (setq divl (rtos_dimdsep (* divl 0.001) 3))) ;of if	    
      (setq txtlen (* (+ (strlen txt) (strlen divn) (strlen divl) 2) th ds    
                   (cdr (assoc 41 (tblsearch "STYLE" (getvar "TEXTSTYLE"))))))    
      (if (>= txtlen dst)    
        (progn    
          (setq dtxt1 (strcat divn "@" divl))    
          (setq dtxt2 (strcat "=" txt))    
          (setq dtxt1p (polar (mid-point sp ep)    
                              (+ ang1 (* pi 0.5 a2sgn))    
                              (+ dmdst (* th ds tsgn))))    
          (setq dtxt2p (polar (mid-point sp ep)    
                              (+ ang1 (* pi 0.5 a2sgn))    
                              (- dmdst (* th ds tsgn))))    
          (setq oldosmode (getvar "OSMODE"))(setvar "OSMODE" 0)    
	  (command "TEXT" "M" dtxt1p (* ds th) (rtod tang) dtxt1)    
          (command "TEXT" "M" dtxt2p (* ds th) (rtod tang) dtxt2)    
          (command "DIM1" "ALI" sp ep dxy " ")               ;DIM명령 내림    
          (setvar "OSMODE" oldosmode)    
	) ;of progn THEN    
        (progn    
          (setq dtxt1 (strcat divn "@" divl "=" txt))    
          (setq oldosmode (getvar "OSMODE"))(setvar "OSMODE" 0)    
	  (command "DIM1" "ALI" sp ep dxy dtxt1)               ;DIM명령 내림    
          (setvar "OSMODE" oldosmode)    
	) ;of progn ELSE    
      ) ;of IF    
    ) ;of progn THEN    
    (progn    
      (if (= txt1 "") (setq txt1 txt))                    ;리턴입력시 옛 text 씀(    
      (setq oldosmode (getvar "OSMODE"))(setvar "OSMODE" 0)    
      (command "DIM1" "ALI" sp ep dxy txt1)               ;DIM명령 내림    
      (setvar "OSMODE" oldosmode)    
    ) ;of progn ELSE    
  ) ;of if(txt1=@)    
    
  (setq sp ep)                    ;끝점을 첫점으로    
  (initget "eXit Undo")    
  (setq next (getdist "\nDistance or RETURN to Pick point/eXit: ")) ;끝점까지 거리    
  (cond                                               ;점을 입력하려면 리턴입력    
    ((= next nil)    
      (setq ep (getpoint "\nPick point: "))                ;끝점을 입력    
      (setq sp1 (polar sp ang1 1.0))    
      (setq ep1 (polar ep (+ ang1 (* 0.5 pi)) 1.0))    
      (setq ep (inters sp sp1 ep ep1 nil))    
    ) ;cond(next=nil)    
    ((= next "eXit")                                         ;eXit입력시 ep=nil    
      (setq ep nil)    
    ) ;cond(next="eXit")    
    ((numberp next)                                          ;dx가 숫자인 경우    
      (setq ep (polar sp ang1 next))    
    ) ;cond(next=number)    
  ) ;of cond    
    
) ;of while    
    
;(pop-env)    
  (setq *error* oer seterr nil)    
(prin1)    
) ;defun    
    
;-------------------------------------------------------    
; function : rtos_dimdsep    
;            rtos (change . to dimdsep)    
;            Yi suk jong    
;            00/5/10    
;-------------------------------------------------------    
; argument    
;       real : real to be converted    
;  precision : decimail precision    
;-------------------------------------------------------    
(defun rtos_dimdsep(real precision / real precision txt txtlen count)      
  (setq txt (rtos real 2 precision)    
	txtlen (strlen txt)    
	count 1)      
      
  (while (and (/= (substr txt count 1) ".") (<= count (1+ txtlen)))    
    (setq count (1+ count))    
  );while    
  (if (> count txtlen)    
    txt    
    (strcat (substr txt 1 (1- count)) (getvar "DIMDSEP") (substr txt (1+ count) (- txtlen count)))    
  )    
);defun        


;----------------------------------------
;  PROGRAM : LDIM
;            Line DIMension
;            Yi Suk Jong
;            04/04/03
;----------------------------------------
(defun c:ldim( /
               ds isel selpnt lent sidepnt spnt epnt
	      ang ang90 intp dist angside defpnt1 defpnt2 dlpnt l txt1
	      )
  (setq ds (getvar "DIMSCALE"))  ;get dimscale
  (setq lsel (entsel "\nSelect a line: "))
  
  (setq selpnt (cadr lsel)
	lent (entget (car lsel))) ;line entity
  
  (setq sidepnt (getpoint selpnt "\nPick definition point: "))
		  
  (setq spnt (cdr (assoc 10 lent))   ;start point
	epnt (cdr (assoc 11 lent)))  ;end point
  (setq ang (angle spnt epnt))
  (setq ang90 (+ ang (* pi 0.5)))
  (setq intp (inters spnt epnt
		     sidepnt (polar sidepnt ang90 100) nil))
  (setq dist (distance sidepnt intp)) ;distance sidepoint ~ intersection point
  (setq angside (angle intp sidepnt))
  (setq defpnt1 (polar spnt angside dist)
	defpnt2 (polar epnt angside dist))
  (setq dlpnt (polar epnt angside (+ (* 20 ds) dist)))       ;dimension line point
  (setq l (distance spnt epnt))  ;distance of line
  (setq txt1 (rto_dimtxt l))
  (command "DIM1" "ALI" defpnt1 defpnt2 dlpnt txt1)               ;DIM
);


;----------------------------------------
; Funcion : rto_dimtxt(l)
;           real to dimtxt
;           Yi Suk Jong
;           04/04/03
;----------------------------------------
; argument : l (length)
; retun    : dim text
;  ex)  (rto_dimtxt 1000)  --> "1.000"
;       (rto_dimtxt 999)  --> "999"
(defun rto_dimtxt(l / l )
  (setvar "DIMZIN" 0)
  (if (< l 1000)
    (rtos l 2 0)
    (progn
      (rtos (* l 0.001) 2 3)
    );progn
  );if  
);  defun
