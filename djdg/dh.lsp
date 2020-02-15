;------------------program list
; program : DH
; program : DV
; program : DA
; program : LDIM
; program : ARWH
; program : DSS    
; program : DT   ; dimension text를 실제 길이에 맞게 수정해줌
; program : DSM  ; Dimension Stretch & Move
;------------------ function list
; function : rtos_dimdsep    	: rtos (change . to dimdsep)    
; Function: farest 		: seek farest 2 points --> djdgfun
; Funcion : rto_dimtxt(l) 	: real to dimtxt
; function : djdgf_dt		; 실제치수를 치수문자로 적용해줌(함수)
; function : djdg_stretchdim	; 치수를 strech시켜줌
; function : djdg_movedim (move dimension)	; 치수를 옮겨줌
; function : djdg_angdist			; 어떤 방향으로의 거리 (반대방향이면 -)


;*********************************        
; Program : DH        
;           Dimension Horizontal        
;           Jong-Suk Yi        
;           1995. 3. 7, 7/5        
;*********************************        
; g
(defun C:DH(/        
th ds sp dsel sent pnt1 pnt2 ppnt                       ;지역변수 정의        
next ep dp dty sgn fst dy dxy dx        
txt txt1 divl divn txtlen dtxt1 dtxt2        
dtxt1p dtxt2p        
)        
        
  (defun SETERR(s)        
    (if (/= s "Function cancelled")        
        (princ (strcat "\nError: " s))        
    ); of If        
    (setq *error* oer seterr nil)        
    (princ)        
  ); of SETERR        
        
  (setq oer *error* *error* seterr)        
        
(setq th (getvar "DIMTXT")                               ;글자 크기 지정        
      dim_gap (getvar "DIMDLI"))                                     ;치수선 단 사이 간격        
(setq ds (getvar "DIMSCALE"))                           ;scale factor        

;(push-env)                                              ;환경값 지정        
(setvar "BLIPMODE" 0)    
(setvar "CMDECHO" 0)
(setvar "dimzin" 0)  
      
(initget "Object")        
(setq sp (getpoint "\nPick first point/Object: "))      ;시작점 입력        
(if (= sp "Object")                                       ;기존 치수선 참조        
  (progn        
    (setq dsel (entsel "\nSelect Dimension Entity: "))    ;dimension entity선택        
    (setq sent (entget (car dsel)))        
    (setq pnt1 (cdr (assoc 13 sent)))        
    (setq pnt2 (cdr (assoc 14 sent)))        
    (setq ppnt (cadr dsel))                                 ;선택점        
    (if (> (distance ppnt pnt1) (distance ppnt pnt2))        
      (setq sp pnt2) (setq sp pnt1))                         ;선택점과 가까운 쪽        
  ) ;of progn THEN        
) ;of IF(sp=Object)        
        
(setq dp (getpoint "\nPick Dimension line side: ")) ;치수선의 위치(위,아래)        
(if (> (- (cadr dp) (cadr sp)) 0)        
  (setq sgn 1)        
  (setq sgn -1)        
) ;of if        
        
(setq fst (getint "\nLevel <1>: "))                  ;치수선 단계입력        
(if (= fst nil) (setq fst 1))                       ;리턴입력시 1단계로        
(setq dy (* ds (+ 20 (* dim_gap (- fst 1)))))             ;치수선 위치 계산        
        
(setq next (getdist sp "\nDistance or RETURN to Pick point: ")) ;끝점까지 거리        
(cond                                               ;점을 입력하려면 리턴입력        
  ((= next nil)        
    (setq ep (getpoint "\nPick point: "))                ;끝점을 입력        
    (setq ep (list (car ep) (cadr sp)))                  ;수정된 끝점        
  ) ;cond(next=nil)        
  ((numberp next)                                          ;dx가 숫자인 경우        
    (setq ep (list (+ (car sp) next) (cadr sp)))             ;ep 위치계산        
    (if (> next 0) (setq lsgn 1) (setq lsgn -1))           ;치수선 진행방향        
  ) ;cond(next=number)        
) ;of cond        
        
        
(while (/= ep nil)                                  ;ep가 nil이 아니면 계속 반복        
        
  (setq dxy (list (car ep) (+ (cadr ep) (* dy sgn)) 0.0))  ;치수선 위치        
        
  (setq dx (distance sp ep))                          ;거리 계산        
  
  (setq dx (* dx (getvar "dimlfac")))
  
  (if (< dx 1000.0)        
    (setq txt (rtos dx 2 0))                          ;1000미만일 때        
;    (setq txt (rtos (* dx 0.001) 2 3))                ;1000이상일 때        
    (setq txt (rtos_dimdsep (* dx 0.001) 3))                ;1000이상일 때        
  ) ;of if(dx < 1000)        
        
  (princ "\nDimension text <")                        ;Dimension text표시        
  (princ txt)        
  (setq txt1 (getstring T ">: "))                     ;새로운 dimension text입력        
  (if (= (substr txt1 1 1) "@")        
    (progn        
;      (setq divl (getint "\nDivision length: "))      ;나누는 길이 입력        
      (setq divl (atof (substr txt1 2 (1- (strlen txt1)))))
      (setq divl (* divl (getvar "dimlfac")))         ; dimlfac적용
      (setq divn (rtos (/ dx divl) 2 0))              ;나눈 갯수계산        
      (if (< divl 1000.)        
        (setq divl (rtos divl 2 0))                   ;1000미만일 때        
        (setq divl (rtos_dimdsep (* 0.001 divl)  3))) ;of if ;1000이상일 때        
      (setq txtlen (* (+ (strlen txt) (strlen divn) (strlen divl) 2) th ds  ;text전체길이        
                   (cdr (assoc 41 (tblsearch "STYLE" (getvar "TEXTSTYLE"))))))        
      (if (>= txtlen dx)                       ;치수보조선 내에 text 안들어가면        
        (progn        
          (setq dtxt1 (strcat divn "@" divl))       ;위 아래 두줄로 나눈다        
          (setq dtxt2 (strcat "=" txt))        
          (setq dtxt1p (mapcar '+ (mid-point sp ep)        
                                  (list 0.0 (+ (* dy sgn) (* ds th)) 0.0)))        
          (setq dtxt2p (mapcar '+ (mid-point sp ep)        
                                  (list 0.0 (- (* dy sgn) (* ds th)) 0.0)))        
          (setq oldosmode (getvar "OSMODE"))(setvar "OSMODE" 0)    
;         (command "TEXT" "M" dtxt1p (* th ds) "0" dtxt1)        
          (command "TEXT" "M" dtxt2p (* th ds) "0" dtxt2)        
;          (command "DIM1" "HOR" sp ep dxy " ")               ;DIM명령 내림        
          (command "DIM1" "HOR" sp ep dxy dtxt1)               ;DIM명령 내림        
	  (setvar "OSMODE" oldosmode)     
        ) ;of progn THEN        
        (progn                                 ;치수보조선 내에 text 들어가면        
          (setq dtxt1 (strcat divn "@" divl "=" txt))        
          (setq oldosmode (getvar "OSMODE"))(setvar "OSMODE" 0)    
	  (command "DIM1" "HOR" sp ep dxy dtxt1)               ;DIM명령 내림        
	  (setvar "OSMODE" oldosmode)     
	  ) ;of progn ELSE        
      ) ;of IF        
    ) ;of progn THEN        
    (progn        
      (if (= txt1 "") (setq txt1 txt))                  ;리턴입력시 옛 text를 씀        
      (setq oldosmode (getvar "OSMODE"))(setvar "OSMODE" 0)    
      (command "DIM1" "HOR" sp ep dxy txt1)             ;DIM명령 내림        
      (setvar "OSMODE" oldosmode)     
      ) ;of progn ELSE        
  ) ;of if(txt1=@)        
        
  (setq sp ep)                                          ;끝점을 첫점으로        
  (initget "eXit Undo")        
  (setq next (getdist "\nDistance or RETURN to Pick point/eXit: ")) ;끝점까지 거리        
  (cond                                               ;점을 입력하려면 리턴입력        
    ((= next nil)        
      (setq ep (getpoint "\nPick point: "))                ;끝점을 입력        
      (setq ep (list (car ep) (cadr sp)))                  ;수정된 끝점        
    ) ;cond(next=nil)        
    ((= next "eXit")                                       ;eXit입력시 ep=nil        
      (setq ep nil)        
    ) ;cond(next="eXit")        
    ((numberp next)                                             ;dx가 숫자인 경우        
      (setq ep (list (+ (car sp) (* next lsgn)) (cadr sp)))     ;ep 위치계산        
    ) ;cond(next=number)        
  ) ;of cond        
        
) ;of while        
        
;  (pop-env)        
  (setq *error* oer seterr nil)        
  (prin1)        
) ;defun


;*********************************    
; Program : DV    
;           Dimension Vertical    
;           Jong-Suk Yi    
;           1995. 3. 8, 7/5    
;*********************************    
; 수직치수선그리기

(defun C:DV(/    
th ds sp dsel sent pnt1 pnt2 ppnt    
next ep dp dtx sgn fst dx dxy dy    
txt txt1 divl divn txtlen dtxt1 dtxt2    
dtxt1p dtxt2p    
)    
  (defun SETERR(s)    
    (if (/= s "Function cancelled")    
        (princ (strcat "\nError: " s))    
    ); of If    
    (setq *error* oer seterr nil)    
    (princ)    
  ); of SETERR    
  (setq oer *error* *error* seterr)    
    
(setq th (getvar "DIMTXT")                          ;text크기 = dimtxt    
      dim_gap (getvar "DIMDLI"))                                 ;치수선 간격    
(setq ds (getvar "DIMSCALE"))                       ;scale factor    
    
(setvar "BLIPMODE" 0)    
(setvar "CMDECHO" 0)      
;(push-env)                                          ;환경변수값 대피    
    
(initget "Object")    
(setq sp (getpoint "\nPick first point/Object: "))  ;기존 치수선 참조=Object    
(if (= sp "Object")    
  (progn    
    (setq dsel (entsel "\nSelect Dimension Entity: "))   ;기존 치수선 선택    
    (setq sent (entget (car dsel)))                      ;치수선 entity    
    (setq pnt1 (cdr (assoc 13 sent)))                    ;보조선 시작점    
    (setq pnt2 (cdr (assoc 14 sent)))                    ;보조선 끝점    
    (setq ppnt (cadr dsel))                              ;선택시 pick point    
    (if (> (distance ppnt pnt1) (distance ppnt pnt2))    ;pick point에 가까운    
      (setq sp pnt2) (setq sp pnt1))                        ;쪽 점을 sp로    
  ) ;of progn THEN    
) ;of IF(sp=Object)    
    
(setq dp (getpoint "\nPick Dimension side: "))           ;치수선이 위치할 방향    
(if (> (- (car dp) (car sp)) 0)    
  (setq sgn 1)    
  (setq sgn -1)    
) ;of if    
    
(setq fst (getint "\nDimension line LEVEL <1>: "))       ;치수선 level입력    
(if (= fst nil) (setq fst 1))    
(setq dx (* ds (+ 20 (* dim_gap (- fst 1)))))    
    
(setq next (getdist sp "\nDistance or RETURN to Pick point: ")) ;끝점까지 거리    
(cond                                               ;점을 입력하려면 리턴입력    
  ((= next nil)    
    (setq ep (getpoint "\nPick point: "))                ;끝점을 입력    
    (setq ep (list (car sp) (cadr ep)))                  ;수정된 끝점    
  ) ;cond(next=nil)    
  ((numberp next)                                          ;dx가 숫자인 경우    
    (setq ep (list (car sp) (+ (cadr sp) next)))             ;ep 위치계산    
    (if (> next 0) (setq lsgn 1) (setq lsgn -1))            ;치수선 진행 방향    
  ) ;cond(next=number)    
) ;of cond    
    
    
(while (/= ep nil)                                  ;ep가 nil이 아닌동안 반복    
    
  (setq dxy (list (+ (car ep) (* dx sgn)) (car ep) 0.0))  ;치수선이 놓일 위치    
    
  (setq dy (distance sp ep))                          ;두 점의 거리    

  (setq dy (* dy (getvar "dimlfac")))
  
  (if (< dy 1000.0)    
    (setq txt (rtos dy 2 0))                          ;1000미만일 때    
;    (setq txt (rtos (* dy 0.001) 2 3))                ;1000이상일 때    
    (setq txt (rtos_dimdsep (* dy 0.001)  3))                ;1000이상일     
    ) ;of if(dy < 1000)    
    
  (princ "\nDimension text <")                        ;Dimension text표시    
  (princ txt)    
  (setq txt1 (getstring T ">: "))                     ;새로운 dimension text입력    
  (if (= (substr txt1 1 1) "@")    
    (progn    
;      (setq divl (getint "\nDivision length: "))      ;나누는 길이 입력    
      (setq divl (atof (substr txt1 2 (1- (strlen txt1)))))
      (setq divl (* divl (getvar "dimlfac")))         ;dimlfac 적용
      (setq divn (rtos (/ dy divl) 2 0))              ;나눈 갯수계산    
      (if (< divl 1000.)    
        (setq divl (rtos divl 2 0))                   ;나누는 길이가 1000미만시    
;        (setq divl (rtos (* divl 0.001) 2 3))) ;of if           ;나누는 길이가 1000이상시    
        (setq divl (rtos_dimdsep (* divl 0.001) 3))) ;of if           ;나누는 길이가 1000이상    
      (setq txtlen (* (+ (strlen txt) (strlen divn) (strlen divl) 2) th ds    
                   (cdr (assoc 41 (tblsearch "STYLE" (getvar "TEXTSTYLE"))))))    
      (if (>= txtlen dy)    
        (progn                                  ;text가 보조선 내에 안들어가면    
          (setq dtxt1 (strcat divn "@" divl))   ;두줄로 나눔    
          (setq dtxt2 (strcat "=" txt))    
          (setq dtxt1p (mapcar '+ (mid-point sp ep)    
                                  (list (- (* dx sgn) (* ds th)) 0.0 0.0)))    
          (setq dtxt2p (mapcar '+ (mid-point sp ep)    
                                  (list (+ (* dx sgn) (* ds th)) 0.0 0.0)))    
          (setq oldosmode (getvar "OSMODE"))(setvar "OSMODE" 0)     
;	  (command "TEXT" "M" dtxt1p (* th ds) "90" dtxt1)    
          (command "TEXT" "M" dtxt2p (* th ds) "90" dtxt2)    
          (command "DIM1" "VER" sp ep dxy dtxt1)               ;DIM명령 내림    
          (setvar "OSMODE" oldosmode)    
	) ;of progn THEN    
        (progn                                  ;text가 보조선 내에 들어가면    
          (setq dtxt1 (strcat divn "@" divl "=" txt))    
          (setq oldosmode (getvar "OSMODE"))(setvar "OSMODE" 0)     
	  (command "DIM1" "VER" sp ep dxy dtxt1)               ;DIM명령 내림    
          (setvar "OSMODE" oldosmode)    
	) ;of progn ELSE    
      ) ;of IF    
    ) ;of progn THEN    
    (progn    
      (if (= txt1 "") (setq txt1 txt))                    ;리턴입력시 옛 text를 씀    
      (setq oldosmode (getvar "OSMODE"))(setvar "OSMODE" 0)     
      (command "DIM1" "VER" sp ep dxy txt1)               ;DIM명령 내림    
      (setvar "OSMODE" oldosmode)    
    ) ;of progn ELSE    
  ) ;of if(txt1=@)    
    
  (setq sp ep)                    ;끝점을 첫점으로    
  (initget "eXit Undo")    
  (setq next (getdist "\nDistance or RETURN to Pick point/eXit: ")) ;끝점까지 거리    
  (cond                                               ;점을 입력하려면 리턴입력    
    ((= next nil)    
      (setq ep (getpoint "\nPick point: "))                ;끝점을 입력    
      (setq ep (list (car sp) (cadr ep)))                  ;수정된 끝점    
    ) ;cond(next=nil)    
    ((= next "eXit")                                         ;eXit입력시 ep=nil    
      (setq ep nil)    
    ) ;cond(next="eXit")    
    ((numberp next)                                          ;dx가 숫자인 경우    
      (setq ep (list (car sp) (+ (cadr ep) (* next lsgn))))  ;ep 위치계산    
    ) ;cond(next=number)    
  ) ;of cond    
    
) ;of while    
    
;(pop-env)    
  (setq *error* oer seterr nil)    
(prin1)    
) ;defun    


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
  
  (setq dst (distance sp ep))                          ;두점간의 거리

  (setq dst (* dst (getvar "dimlfac")))			; dimlfac적용

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
      (setq divl (* divl (getvar "dimlfac")))           ;dimlfac적용
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

  (setq l (* l (getvar "dimlfac")))  ;dimlfac 적용

;  (setq txt1 (rto_dimtxt l))
  (setq txt1 (rtos_dimdsep (* l 0.001)  3))                ;1000이상일     

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
(defun rto_dimtxt(l / l txt)
  (setvar "DIMZIN" 0)
  (if (< l 1000)
    (setq txt (rtos l 2 0))
    (progn
      (setq txt (rtos (* l 0.001) 2 3))
    );progn
  );if
  
  (setq txtlen (strlen txt))
  (setq count 1)      
      
  (while (and (/= (substr txt count 1) ".") (<= count (1+ txtlen)))    
    (setq count (1+ count))    
  );while    
  (if (> count txtlen)    
    txt    
    (strcat (substr txt 1 (1- count)) (getvar "DIMDSEP") (substr txt (1+ count) (- txtlen count)))    
  )    
  
);  defun



;*********************************        
; Program : ARWH
;           Draw ARroW Head
;           Jong-Suk Yi        
;           04/03/26
;*********************************        
(defun C:ARWH(/)
  (command "vbarun" (strcat (prefix) "djdg/djdg.dvb!arwh.arwh"))
);defun	      


;--------------------------------------
; program : DSS
;           Dimension Summation
;           Yi Suk Jong
;           00/5/20
;--------------------------------------
(defun c:DSS(
/ ds ssdim ndim index plist diment defpoints defpnt1 defpnt2 firstdim
  pnt14 pntdl ang disdl newdl crosspnt dist )
  
  (setq ds (getvar "dimscale")   ;get dimension scale
        dim_gap (getvar "DIMDLI")) ;get dimension gap 
    
  
  (setq ssdim (ssget '((0 . "DIMENSION"))))
  (setq ndim (sslength ssdim))
  (setq index 0
	plist nil)
  (repeat ndim
    (setq diment (entget (ssname  ssdim index)))
    (setq plist (append plist (list (cdr (assoc 13 diment)))))
    (setq plist (append plist (list (cdr (assoc 14 diment)))))
    (setq index (1+ index))
  );repeat

  (setq defpoints (farest plist))
  (setq defpnt1 (nth 0 defpoints)
	defpnt2 (nth 1 defpoints))

  (setq firstdim (entget (ssname ssdim 0)))   ;first dimension
  (setq pnt14 (cdr (assoc 14 firstdim))       ;definition point (assoc 14)
	pntdl (cdr (assoc 10 firstdim)))      ;dimension line point
  (setq ang (angle pnt14 pntdl))              ;angle definition point --> dimension line point
  (setq distdl (distance pnt14 pntdl))        ;distance definition point --> demension line point
  (setq newdl (polar defpnt2 ang (+ distdl (* ds dim_gap))));new dimension line point

  (setq crosspnt (inters defpnt2 newdl defpnt1 (polar defpnt1 (+ ang (* pi 0.5)) 1) nil)) ;cross point (defpoint2-newdl) and (defpoint1)
  (setq dist (distance defpnt1 crosspnt))    ;distance for dimension text

  (setq dist (* dist (getvar "dimlfac")))    ; dimlfac 적용
  
;  (if (>= dist 1000)
;    (setq dist (rtos (* dist 0.001) 2 3))
;    (setq dist (rtos  dist 2 0) )
;  )

  (if (< dist 1000.)        
    (setq dist (rtos dist 2 0))                   ;1000미만일 때        
    (setq dist (rtos_dimdsep (* 0.001 dist)  3))) ;of if ;1000이상일 때        
  
  (setq oldosmode (getvar "osmode")) (setvar "osmode" 0)
  (command "dim1" "al" defpnt1 defpnt2 newdl dist)
  (setvar "osmode" oldosmode)
  
);defun


;------------------------------------
; program : DT (update DimText)
;           선택된 dimension의 길이를 측정해서 text를 바꿔준다.
;           Yi Suk Jong
;           05/08/18
;------------------------------------
(defun c:dt( / ssdim ndim index sname )
  (setq ssdim (ssget '((0 . "DIMENSION"))))
  (setq ndim (sslength ssdim))
  (setq index 0)
  (repeat ndim
    (setq sname (ssname ssdim index))		;entity name
;    (setq diment (entget sname))		;entity 정보
;    (setq alen (cdr (assoc 42 diment)))   	;actual length(치수의 실제 길이)
;    (setq txt (rto_dimtxt alen))    		;치수문자.
;    (command "DIM1" "NEWTEXT" txt sname "")	; DIM 갱신
    (djdgf_dt sname)
    (setq index (1+ index))
  );repeat
);defun

;------------------------------------
; function : djdgf_dt
;            함수: dimension text를 현재 길이대로 수정
;            Yi Suk Jong
;            05/08/21
;------------------------------------
; arguments
;      sname : entity name
(defun djdgf_dt( sname / diment alen txt)
    (setq diment (entget sname))		;entity 정보
    (setq alen (cdr (assoc 42 diment)))   	;actual length(치수의 실제 길이)
    (setq txt (rto_dimtxt alen))    		;치수문자.
    (command "DIM1" "NEWTEXT" txt sname "")	; DIM 갱신
);defun       

;------------------------------------
; program : DSM (Dimension Strech & Move)
;           dimension text를 수정해서 그 숫자에 맞춰 dimension을 strech하고 Move한다.
;           Yi Suk Jong
;           05/08/18
;------------------------------------
(defun c:dsm(
	     / ssdim dimename tp diment def13 def14 olenstr newdtxt kw newlen delta dist13 dist14
	       ang bp stp stang nss i dent dst13 dst14 near nearent delta1 sgndst
	     )
  (princ "\n수정할 치수선들을 선택하세요: ")
  (setq ssdim (ssget '((0 . "DIMENSION"))))
  (setq dimename (car (entsel "\n길이를 수정할 치수선을 선택하세요: ")))  ;dimension entity name
  (initget "Middle")
  (setq tp (getpoint "\n늘리거나 줄어들 쪽을 찍으세요(M=중앙): ")) ;target point

  ;--- entity 정보 얻기
  (setq diment (entget dimename))   ;entity 정보
  ;--- definition point얻기
  (setq def13 (cdr (assoc 13 diment)))
  (setq def14 (cdr (assoc 14 diment)))
  ;--- 현재 길이 구하기
  (setq alen (cdr (assoc 42 diment)))  ;actual length of dimension
  ;--- 새치수 입력받기
  (setq olenstr (strcat "\n현재 치수:" (rto_dimtxt alen) " 새치수를 입력하세요: ")) ;old length string
  (setq newdtxt (getstring olenstr))
  ;인근치수선 처리방법 입력받기
  (initget "Stretch Move")
  (setq kw (getkword "\n인근치수선을 Stretch(S), 인근치수선을 Move(M): "))
  
  ;--- 현재 치수와 새치수 차이 계산하기
  ;(setq dimdsep (getvar "dimdsep")) 			;decimal sparator
  (setq newlen (djdg_dimtxtoreal newdtxt))		;새치수의 실제길이
  (setq delta (- newlen alen))				;새치수와 현재치수의 차이
  ;--- 선택된 치수선 늘리기
  (djdg_stretchdim dimename tp delta)			;해당치수선 늘리기
  (djdgf_dt dimename)
  ;--- 나머지 치수선 stretch 또는 move하기
  ;--- 기준점과 늘려지는 점 찾기
  (setq dist13 (distance def13 tp))
  (setq dist14 (distance def14 tp))
  (setq ang (cdr (assoc 50 diment)))
  (if (<= dist13 dist14)
    (setq bp def14		
	  stp def13
	  stang (+ ang pi))		;14 --> 13방향
    (setq bp def13		
	  stp def14
	  stang ang)			;13 --> 14방향
  );if  
  ;--- selection set에서 수정된 entity 빼기
  (if (= nil (setq remset (ssdel dimename ssdim))) (setq remset ssdim))
  (setq nss (sslength remset))  			;나머지 selection set 갯수
  (cond
    ((= kw "Stretch")					;stretch선택시 target point에서 제일가까운점을 stretch 
      (setq i 0)
      (setq ;near 0
	    nearent nil)
      (repeat nss
	(setq dent (entget (ssname remset i)))
        (setq dst13 (djdg_angdist bp (cdr (assoc 13 dent)) stang))  ;기준점에서의 거리, 앞쪽이면+ 뒷쪽이면 -
        (setq dst14 (djdg_angdist bp (cdr (assoc 14 dent)) stang))  ;기준점에서의 거리, 앞쪽이면+ 뒷쪽이면 -	        
;        (setq dst13 (distance tp (cdr (assoc 13 dent)))  ;기준점에서의 거리;
;	       dst14 (distance tp (cdr (assoc 14 dent))))
        (if (= nearent nil)
	  (progn						;최단기리가 아직 정의되지 않았을 때
 	    (if (> dst13 0) (setq near dst13 nearent dent))
	    (if (> dst14 0) (setq near dst14 nearent dent)) 
	  );progn
	  (progn
	    (if (and (>  dst13 0) (<= dst13 near))
	      (setq near dst13				;13이 양수이고 최소값보다 작으면 13을 최근거리로
		 nearent dent)
	    );if
	    (if (and (>  dst14 0) (<= dst14 near))
	      (setq near dst14				;14이 양수이고 최소값보다 작으면 14를 최근거리로
		 nearent dent)
	    );if  
	  );progn
	);if    
        (setq i (1+ i))		     
      ); repeat
      ;제일 가까운 entity에 대해서 stretch
      (if (/= nearent nil)			;가장가까운
	(progn
          (setq delta1 (* -1 delta))   		;delta부호 바꾸기
          (djdg_stretchdim (cdr (assoc -1 dent)) tp delta1)
          (djdgf_dt (cdr (assoc -1 dent)))
	);progn
      );if	
    );sub cond
    ((= kw "Move")				;move를 선택했을 때
      (setq i 0)
      (repeat nss
	(setq dent (entget (ssname remset i)))
        (setq sgndst (djdg_angdist bp (cdr (assoc 13 dent)) stang))  ;기준점에서의 거리 앞쪽이면+ 뒷쪽이면 -
	(if (> sgndst 0)				;dimension이 앞쪽에 있을 때만 옮기기
	  (djdg_movedim (ssname remset i) stang delta)  ;stretch 방향으로 delta만큼 옮기기.
	);if
        (setq i (1+ i))		     			
      ); repeat

     );sub cond 
	
  );cond  
  (princ "\n명령이 정상적으로 종료됨")(princ)
);defun

;---------------------------------
; function : djdg_stretchdim
;            주어진 길이만큼 dimension을 strech한다.
;             Yi Suk Jong
;             05/08/18
;---------------------------------
; argument
;     ename : strech될 entity name
;       pnt : 기준점 (nil인 경우엔 양쪽으로 늘리기)
;       dst : 늘리는 양 (pnt가 nil인 경우 각각 dst/2만큼 stretch)
;---------------------------------
(defun djdg_stretchdim(ename pnt dst
		       /   diment defp13 defp14 ang newdefp13 newdefp14 dst13 dst14 newdefp )
  (setq diment (entget ename))  ;치수선 정보 얻기
  (setq defp13 (cdr (assoc 13 diment)))  ;definition point 13
  (setq defp14 (cdr (assoc 14 diment)))  ;definition point 14
  (setq ang (cdr (assoc 50 diment)))     ;angle
  (if (= pnt nil)
    (progn
      (setq newdefp13 (polar defp13 (+ ang pi) (* 0.5 dst)))
      (setq newdefp14 (polar defp14 ang (* 0.5 dst)))
      (setq diment (subst (cons 13 newdefp13) (assoc 13 diment) diment))
      (entmod (subst (cons 14 newdefp14) (assoc 14 diment) diment))
    )  ;progn
    (progn
      (setq dst13 (distance defp13 pnt))
      (setq dst14 (distance defp14 pnt))
      (if (< dst13 dst14)
	(progn
	  (setq newdefp (polar defp13 (+ ang pi) dst))
	  (entmod (subst (cons 13 newdefp) (assoc 13 diment) diment)) ;text 갱신
	);progn
	(progn
	  (setq newdefp (polar defp14 ang dst))
	  (entmod (subst (cons 14 newdefp) (assoc 14 diment) diment)) ;text 갱신	  
	);progn
      );if	
    ); progn
  );if  
)  ;defun


;---------------------------------
; function : djdg_movedim (move dimension)
;            주어진 길이만큼 dimension을 move한다.
;             Yi Suk Jong
;             05/08/20
;---------------------------------
; arguments
;     ename : move될 entity name
;       ang : move될 각도 (radian)
;       dst : move될 양
;---------------------------------
(defun djdg_movedim(ename ang dst
		    / diment defp13 defp14 newdefp13 newdefp14 diment)
  (setq diment (entget ename))  ;치수선 정보 얻기
  (setq defp13 (cdr (assoc 13 diment)))  ;definition point 13
  (setq defp14 (cdr (assoc 14 diment)))  ;definition point 14

  (setq newdefp13 (polar defp13 ang dst))	;시점 definition point 바꾸기
  (setq newdefp14 (polar defp14 ang dst))   	;종점 definition point 바꾸기
  (setq diment (subst (cons 13 newdefp13) (assoc 13 diment) diment))
  (entmod (subst (cons 14 newdefp14) (assoc 14 diment) diment))
)  ;defun

