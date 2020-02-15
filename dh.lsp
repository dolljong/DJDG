;------------------program list
; program : DH
; program : DV
; program : DA
; program : LDIM
; program : ARWH
; program : DSS    
; program : DT   ; dimension text�� ���� ���̿� �°� ��������
; program : DSM  ; Dimension Stretch & Move
;------------------ function list
; function : rtos_dimdsep    	: rtos (change . to dimdsep)    
; Function: farest 		: seek farest 2 points --> djdgfun
; Funcion : rto_dimtxt(l) 	: real to dimtxt
; function : djdgf_dt		; ����ġ���� ġ�����ڷ� ��������(�Լ�)
; function : djdg_stretchdim	; ġ���� strech������
; function : djdg_movedim (move dimension)	; ġ���� �Ű���
; function : djdg_angdist			; � ���������� �Ÿ� (�ݴ�����̸� -)


;*********************************        
; Program : DH        
;           Dimension Horizontal        
;           Jong-Suk Yi        
;           1995. 3. 7, 7/5        
;*********************************        
; g
(defun C:DH(/        
th ds sp dsel sent pnt1 pnt2 ppnt                       ;�������� ����        
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
        
(setq th (getvar "DIMTXT")                               ;���� ũ�� ����        
      dim_gap (getvar "DIMDLI"))                                     ;ġ���� �� ���� ����        
(setq ds (getvar "DIMSCALE"))                           ;scale factor        

;(push-env)                                              ;ȯ�氪 ����        
(setvar "BLIPMODE" 0)    
(setvar "CMDECHO" 0)
(setvar "dimzin" 0)  
      
(initget "Object")        
(setq sp (getpoint "\nPick first point/Object: "))      ;������ �Է�        
(if (= sp "Object")                                       ;���� ġ���� ����        
  (progn        
    (setq dsel (entsel "\nSelect Dimension Entity: "))    ;dimension entity����        
    (setq sent (entget (car dsel)))        
    (setq pnt1 (cdr (assoc 13 sent)))        
    (setq pnt2 (cdr (assoc 14 sent)))        
    (setq ppnt (cadr dsel))                                 ;������        
    (if (> (distance ppnt pnt1) (distance ppnt pnt2))        
      (setq sp pnt2) (setq sp pnt1))                         ;�������� ����� ��        
  ) ;of progn THEN        
) ;of IF(sp=Object)        
        
(setq dp (getpoint "\nPick Dimension line side: ")) ;ġ������ ��ġ(��,�Ʒ�)        
(if (> (- (cadr dp) (cadr sp)) 0)        
  (setq sgn 1)        
  (setq sgn -1)        
) ;of if        
        
(setq fst (getint "\nLevel <1>: "))                  ;ġ���� �ܰ��Է�        
(if (= fst nil) (setq fst 1))                       ;�����Է½� 1�ܰ��        
(setq dy (* ds (+ 20 (* dim_gap (- fst 1)))))             ;ġ���� ��ġ ���        
        
(setq next (getdist sp "\nDistance or RETURN to Pick point: ")) ;�������� �Ÿ�        
(cond                                               ;���� �Է��Ϸ��� �����Է�        
  ((= next nil)        
    (setq ep (getpoint "\nPick point: "))                ;������ �Է�        
    (setq ep (list (car ep) (cadr sp)))                  ;������ ����        
  ) ;cond(next=nil)        
  ((numberp next)                                          ;dx�� ������ ���        
    (setq ep (list (+ (car sp) next) (cadr sp)))             ;ep ��ġ���        
    (if (> next 0) (setq lsgn 1) (setq lsgn -1))           ;ġ���� �������        
  ) ;cond(next=number)        
) ;of cond        
        
        
(while (/= ep nil)                                  ;ep�� nil�� �ƴϸ� ��� �ݺ�        
        
  (setq dxy (list (car ep) (+ (cadr ep) (* dy sgn)) 0.0))  ;ġ���� ��ġ        
        
  (setq dx (distance sp ep))                          ;�Ÿ� ���        
  
  (setq dx (* dx (getvar "dimlfac")))
  
  (if (< dx 1000.0)        
    (setq txt (rtos dx 2 0))                          ;1000�̸��� ��        
;    (setq txt (rtos (* dx 0.001) 2 3))                ;1000�̻��� ��        
    (setq txt (rtos_dimdsep (* dx 0.001) 3))                ;1000�̻��� ��        
  ) ;of if(dx < 1000)        
        
  (princ "\nDimension text <")                        ;Dimension textǥ��        
  (princ txt)        
  (setq txt1 (getstring T ">: "))                     ;���ο� dimension text�Է�        
  (if (= (substr txt1 1 1) "@")        
    (progn        
;      (setq divl (getint "\nDivision length: "))      ;������ ���� �Է�        
      (setq divl (atof (substr txt1 2 (1- (strlen txt1)))))
      (setq divl (* divl (getvar "dimlfac")))         ; dimlfac����
      (setq divn (rtos (/ dx divl) 2 0))              ;���� �������        
      (if (< divl 1000.)        
        (setq divl (rtos divl 2 0))                   ;1000�̸��� ��        
        (setq divl (rtos_dimdsep (* 0.001 divl)  3))) ;of if ;1000�̻��� ��        
      (setq txtlen (* (+ (strlen txt) (strlen divn) (strlen divl) 2) th ds  ;text��ü����        
                   (cdr (assoc 41 (tblsearch "STYLE" (getvar "TEXTSTYLE"))))))        
      (if (>= txtlen dx)                       ;ġ�������� ���� text �ȵ���        
        (progn        
          (setq dtxt1 (strcat divn "@" divl))       ;�� �Ʒ� ���ٷ� ������        
          (setq dtxt2 (strcat "=" txt))        
          (setq dtxt1p (mapcar '+ (mid-point sp ep)        
                                  (list 0.0 (+ (* dy sgn) (* ds th)) 0.0)))        
          (setq dtxt2p (mapcar '+ (mid-point sp ep)        
                                  (list 0.0 (- (* dy sgn) (* ds th)) 0.0)))        
          (setq oldosmode (getvar "OSMODE"))(setvar "OSMODE" 0)    
;         (command "TEXT" "M" dtxt1p (* th ds) "0" dtxt1)        
          (command "TEXT" "M" dtxt2p (* th ds) "0" dtxt2)        
;          (command "DIM1" "HOR" sp ep dxy " ")               ;DIM��� ����        
          (command "DIM1" "HOR" sp ep dxy dtxt1)               ;DIM��� ����        
	  (setvar "OSMODE" oldosmode)     
        ) ;of progn THEN        
        (progn                                 ;ġ�������� ���� text ����        
          (setq dtxt1 (strcat divn "@" divl "=" txt))        
          (setq oldosmode (getvar "OSMODE"))(setvar "OSMODE" 0)    
	  (command "DIM1" "HOR" sp ep dxy dtxt1)               ;DIM��� ����        
	  (setvar "OSMODE" oldosmode)     
	  ) ;of progn ELSE        
      ) ;of IF        
    ) ;of progn THEN        
    (progn        
      (if (= txt1 "") (setq txt1 txt))                  ;�����Է½� �� text�� ��        
      (setq oldosmode (getvar "OSMODE"))(setvar "OSMODE" 0)    
      (command "DIM1" "HOR" sp ep dxy txt1)             ;DIM��� ����        
      (setvar "OSMODE" oldosmode)     
      ) ;of progn ELSE        
  ) ;of if(txt1=@)        
        
  (setq sp ep)                                          ;������ ù������        
  (initget "eXit Undo")        
  (setq next (getdist "\nDistance or RETURN to Pick point/eXit: ")) ;�������� �Ÿ�        
  (cond                                               ;���� �Է��Ϸ��� �����Է�        
    ((= next nil)        
      (setq ep (getpoint "\nPick point: "))                ;������ �Է�        
      (setq ep (list (car ep) (cadr sp)))                  ;������ ����        
    ) ;cond(next=nil)        
    ((= next "eXit")                                       ;eXit�Է½� ep=nil        
      (setq ep nil)        
    ) ;cond(next="eXit")        
    ((numberp next)                                             ;dx�� ������ ���        
      (setq ep (list (+ (car sp) (* next lsgn)) (cadr sp)))     ;ep ��ġ���        
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
; ����ġ�����׸���

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
    
(setq th (getvar "DIMTXT")                          ;textũ�� = dimtxt    
      dim_gap (getvar "DIMDLI"))                                 ;ġ���� ����    
(setq ds (getvar "DIMSCALE"))                       ;scale factor    
    
(setvar "BLIPMODE" 0)    
(setvar "CMDECHO" 0)      
;(push-env)                                          ;ȯ�溯���� ����    
    
(initget "Object")    
(setq sp (getpoint "\nPick first point/Object: "))  ;���� ġ���� ����=Object    
(if (= sp "Object")    
  (progn    
    (setq dsel (entsel "\nSelect Dimension Entity: "))   ;���� ġ���� ����    
    (setq sent (entget (car dsel)))                      ;ġ���� entity    
    (setq pnt1 (cdr (assoc 13 sent)))                    ;������ ������    
    (setq pnt2 (cdr (assoc 14 sent)))                    ;������ ����    
    (setq ppnt (cadr dsel))                              ;���ý� pick point    
    (if (> (distance ppnt pnt1) (distance ppnt pnt2))    ;pick point�� �����    
      (setq sp pnt2) (setq sp pnt1))                        ;�� ���� sp��    
  ) ;of progn THEN    
) ;of IF(sp=Object)    
    
(setq dp (getpoint "\nPick Dimension side: "))           ;ġ������ ��ġ�� ����    
(if (> (- (car dp) (car sp)) 0)    
  (setq sgn 1)    
  (setq sgn -1)    
) ;of if    
    
(setq fst (getint "\nDimension line LEVEL <1>: "))       ;ġ���� level�Է�    
(if (= fst nil) (setq fst 1))    
(setq dx (* ds (+ 20 (* dim_gap (- fst 1)))))    
    
(setq next (getdist sp "\nDistance or RETURN to Pick point: ")) ;�������� �Ÿ�    
(cond                                               ;���� �Է��Ϸ��� �����Է�    
  ((= next nil)    
    (setq ep (getpoint "\nPick point: "))                ;������ �Է�    
    (setq ep (list (car sp) (cadr ep)))                  ;������ ����    
  ) ;cond(next=nil)    
  ((numberp next)                                          ;dx�� ������ ���    
    (setq ep (list (car sp) (+ (cadr sp) next)))             ;ep ��ġ���    
    (if (> next 0) (setq lsgn 1) (setq lsgn -1))            ;ġ���� ���� ����    
  ) ;cond(next=number)    
) ;of cond    
    
    
(while (/= ep nil)                                  ;ep�� nil�� �ƴѵ��� �ݺ�    
    
  (setq dxy (list (+ (car ep) (* dx sgn)) (car ep) 0.0))  ;ġ������ ���� ��ġ    
    
  (setq dy (distance sp ep))                          ;�� ���� �Ÿ�    

  (setq dy (* dy (getvar "dimlfac")))
  
  (if (< dy 1000.0)    
    (setq txt (rtos dy 2 0))                          ;1000�̸��� ��    
;    (setq txt (rtos (* dy 0.001) 2 3))                ;1000�̻��� ��    
    (setq txt (rtos_dimdsep (* dy 0.001)  3))                ;1000�̻���     
    ) ;of if(dy < 1000)    
    
  (princ "\nDimension text <")                        ;Dimension textǥ��    
  (princ txt)    
  (setq txt1 (getstring T ">: "))                     ;���ο� dimension text�Է�    
  (if (= (substr txt1 1 1) "@")    
    (progn    
;      (setq divl (getint "\nDivision length: "))      ;������ ���� �Է�    
      (setq divl (atof (substr txt1 2 (1- (strlen txt1)))))
      (setq divl (* divl (getvar "dimlfac")))         ;dimlfac ����
      (setq divn (rtos (/ dy divl) 2 0))              ;���� �������    
      (if (< divl 1000.)    
        (setq divl (rtos divl 2 0))                   ;������ ���̰� 1000�̸���    
;        (setq divl (rtos (* divl 0.001) 2 3))) ;of if           ;������ ���̰� 1000�̻��    
        (setq divl (rtos_dimdsep (* divl 0.001) 3))) ;of if           ;������ ���̰� 1000�̻�    
      (setq txtlen (* (+ (strlen txt) (strlen divn) (strlen divl) 2) th ds    
                   (cdr (assoc 41 (tblsearch "STYLE" (getvar "TEXTSTYLE"))))))    
      (if (>= txtlen dy)    
        (progn                                  ;text�� ������ ���� �ȵ���    
          (setq dtxt1 (strcat divn "@" divl))   ;���ٷ� ����    
          (setq dtxt2 (strcat "=" txt))    
          (setq dtxt1p (mapcar '+ (mid-point sp ep)    
                                  (list (- (* dx sgn) (* ds th)) 0.0 0.0)))    
          (setq dtxt2p (mapcar '+ (mid-point sp ep)    
                                  (list (+ (* dx sgn) (* ds th)) 0.0 0.0)))    
          (setq oldosmode (getvar "OSMODE"))(setvar "OSMODE" 0)     
;	  (command "TEXT" "M" dtxt1p (* th ds) "90" dtxt1)    
          (command "TEXT" "M" dtxt2p (* th ds) "90" dtxt2)    
          (command "DIM1" "VER" sp ep dxy dtxt1)               ;DIM��� ����    
          (setvar "OSMODE" oldosmode)    
	) ;of progn THEN    
        (progn                                  ;text�� ������ ���� ����    
          (setq dtxt1 (strcat divn "@" divl "=" txt))    
          (setq oldosmode (getvar "OSMODE"))(setvar "OSMODE" 0)     
	  (command "DIM1" "VER" sp ep dxy dtxt1)               ;DIM��� ����    
          (setvar "OSMODE" oldosmode)    
	) ;of progn ELSE    
      ) ;of IF    
    ) ;of progn THEN    
    (progn    
      (if (= txt1 "") (setq txt1 txt))                    ;�����Է½� �� text�� ��    
      (setq oldosmode (getvar "OSMODE"))(setvar "OSMODE" 0)     
      (command "DIM1" "VER" sp ep dxy txt1)               ;DIM��� ����    
      (setvar "OSMODE" oldosmode)    
    ) ;of progn ELSE    
  ) ;of if(txt1=@)    
    
  (setq sp ep)                    ;������ ù������    
  (initget "eXit Undo")    
  (setq next (getdist "\nDistance or RETURN to Pick point/eXit: ")) ;�������� �Ÿ�    
  (cond                                               ;���� �Է��Ϸ��� �����Է�    
    ((= next nil)    
      (setq ep (getpoint "\nPick point: "))                ;������ �Է�    
      (setq ep (list (car sp) (cadr ep)))                  ;������ ����    
    ) ;cond(next=nil)    
    ((= next "eXit")                                         ;eXit�Է½� ep=nil    
      (setq ep nil)    
    ) ;cond(next="eXit")    
    ((numberp next)                                          ;dx�� ������ ���    
      (setq ep (list (car sp) (+ (cadr ep) (* next lsgn))))  ;ep ��ġ���    
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
    
  (setq oer *error* *error* seterr)                     ;���忡����ƾ ����    
    
(setq th (getvar "DIMTXT")                              ; textũ��    
      dim_gap (getvar "DIMDLI"))                                    ; ġ���� ����    
    
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
      (setq sp pnt2) (setq sp pnt1))                        ;�������� ����� ��    
  ) ;of progn THEN    
) ;of IF(sp=Object)    
    
(setq ep (getpoint sp "\nPick second point: "))    
(setq ang1 (angle sp ep))    
(setq w4 (which4 ang1))    
(cond                                       ;�ؽ�Ʈ�� ���� (����� �ɼ�)    
  ((or (= w4 1) (= w4 4)) (setq tang ang1))    
  ((or (= w4 2) (= w4 3)) (setq tang (- ang1 pi)))    
) ;of cond    
    
(setq dp (getpoint "\nPick Dimension line side: "))    
(setq ang2 (angle sp dp))    
(if (minusp (dang ang1 ang2)) (setq a2sgn -1) (setq a2sgn 1))  ;��ȣ����    
(setq tsgn 1)    
(if (and (or (= w4 2) (= w4 3)) (= a2sgn 1)) (setq tsgn -1))    
(if (and (or (= w4 1) (= w4 4)) (= a2sgn -1)) (setq tsgn -1))    
    
(setq fst (getint "\nDimension line Level <1>: "))       ;ġ���� ���� �Է�    
(if (= fst nil) (setq fst 1))    
(setq dmdst (* ds (+ 20 (* dim_gap (- fst 1)))))    
    
(while (/= ep nil)    
    
  (setq dxy (polar ep (+ (* pi 0.5 a2sgn) ang1) dmdst))
  
  (setq dst (distance sp ep))                          ;�������� �Ÿ�

  (setq dst (* dst (getvar "dimlfac")))			; dimlfac����

  (if (< dst 1000.0)    
    (setq txt (rtos dst 2 0))                          ;1000�̸��� ��    
;    (setq txt (rtos (* dst 0.001) 2 3))                ;1000�̻��� ��    
    (setq txt (rtos_dimdsep (* dst 0.001)  3))                ;1000�̻���     
    ) ;of if(dst < 1000)    
    
  (princ "\nDimension text <")                        ;Dimension textǥ��    
  (princ txt)    
  (setq txt1 (getstring T ">: "))                     ;���ο� dimension text�Է�    
  (if (= (substr txt1 1 1) "@")                                    ;����� �Է½�    
    (progn    
;      (setq divl (getreal "\nDivision length: "))       ;������ ���� �Է�    
      (setq divl (atof (substr txt1 2 (1- (strlen txt1)))))    
      (setq divl (* divl (getvar "dimlfac")))           ;dimlfac����
      (setq divn (/ dst divl))    
      (setq divn (rtos divn 2 0))                       ;���� �������    
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
          (command "DIM1" "ALI" sp ep dxy " ")               ;DIM��� ����    
          (setvar "OSMODE" oldosmode)    
	) ;of progn THEN    
        (progn    
          (setq dtxt1 (strcat divn "@" divl "=" txt))    
          (setq oldosmode (getvar "OSMODE"))(setvar "OSMODE" 0)    
	  (command "DIM1" "ALI" sp ep dxy dtxt1)               ;DIM��� ����    
          (setvar "OSMODE" oldosmode)    
	) ;of progn ELSE    
      ) ;of IF    
    ) ;of progn THEN    
    (progn    
      (if (= txt1 "") (setq txt1 txt))                    ;�����Է½� �� text ��(    
      (setq oldosmode (getvar "OSMODE"))(setvar "OSMODE" 0)    
      (command "DIM1" "ALI" sp ep dxy txt1)               ;DIM��� ����    
      (setvar "OSMODE" oldosmode)    
    ) ;of progn ELSE    
  ) ;of if(txt1=@)    
    
  (setq sp ep)                    ;������ ù������    
  (initget "eXit Undo")    
  (setq next (getdist "\nDistance or RETURN to Pick point/eXit: ")) ;�������� �Ÿ�    
  (cond                                               ;���� �Է��Ϸ��� �����Է�    
    ((= next nil)    
      (setq ep (getpoint "\nPick point: "))                ;������ �Է�    
      (setq sp1 (polar sp ang1 1.0))    
      (setq ep1 (polar ep (+ ang1 (* 0.5 pi)) 1.0))    
      (setq ep (inters sp sp1 ep ep1 nil))    
    ) ;cond(next=nil)    
    ((= next "eXit")                                         ;eXit�Է½� ep=nil    
      (setq ep nil)    
    ) ;cond(next="eXit")    
    ((numberp next)                                          ;dx�� ������ ���    
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

  (setq l (* l (getvar "dimlfac")))  ;dimlfac ����

;  (setq txt1 (rto_dimtxt l))
  (setq txt1 (rtos_dimdsep (* l 0.001)  3))                ;1000�̻���     

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

  (setq dist (* dist (getvar "dimlfac")))    ; dimlfac ����
  
;  (if (>= dist 1000)
;    (setq dist (rtos (* dist 0.001) 2 3))
;    (setq dist (rtos  dist 2 0) )
;  )

  (if (< dist 1000.)        
    (setq dist (rtos dist 2 0))                   ;1000�̸��� ��        
    (setq dist (rtos_dimdsep (* 0.001 dist)  3))) ;of if ;1000�̻��� ��        
  
  (setq oldosmode (getvar "osmode")) (setvar "osmode" 0)
  (command "dim1" "al" defpnt1 defpnt2 newdl dist)
  (setvar "osmode" oldosmode)
  
);defun


;------------------------------------
; program : DT (update DimText)
;           ���õ� dimension�� ���̸� �����ؼ� text�� �ٲ��ش�.
;           Yi Suk Jong
;           05/08/18
;------------------------------------
(defun c:dt( / ssdim ndim index sname )
  (setq ssdim (ssget '((0 . "DIMENSION"))))
  (setq ndim (sslength ssdim))
  (setq index 0)
  (repeat ndim
    (setq sname (ssname ssdim index))		;entity name
;    (setq diment (entget sname))		;entity ����
;    (setq alen (cdr (assoc 42 diment)))   	;actual length(ġ���� ���� ����)
;    (setq txt (rto_dimtxt alen))    		;ġ������.
;    (command "DIM1" "NEWTEXT" txt sname "")	; DIM ����
    (djdgf_dt sname)
    (setq index (1+ index))
  );repeat
);defun

;------------------------------------
; function : djdgf_dt
;            �Լ�: dimension text�� ���� ���̴�� ����
;            Yi Suk Jong
;            05/08/21
;------------------------------------
; arguments
;      sname : entity name
(defun djdgf_dt( sname / diment alen txt)
    (setq diment (entget sname))		;entity ����
    (setq alen (cdr (assoc 42 diment)))   	;actual length(ġ���� ���� ����)
    (setq txt (rto_dimtxt alen))    		;ġ������.
    (command "DIM1" "NEWTEXT" txt sname "")	; DIM ����
);defun       

;------------------------------------
; program : DSM (Dimension Strech & Move)
;           dimension text�� �����ؼ� �� ���ڿ� ���� dimension�� strech�ϰ� Move�Ѵ�.
;           Yi Suk Jong
;           05/08/18
;------------------------------------
(defun c:dsm(
	     / ssdim dimename tp diment def13 def14 olenstr newdtxt kw newlen delta dist13 dist14
	       ang bp stp stang nss i dent dst13 dst14 near nearent delta1 sgndst
	     )
  (princ "\n������ ġ�������� �����ϼ���: ")
  (setq ssdim (ssget '((0 . "DIMENSION"))))
  (setq dimename (car (entsel "\n���̸� ������ ġ������ �����ϼ���: ")))  ;dimension entity name
  (initget "Middle")
  (setq tp (getpoint "\n�ø��ų� �پ�� ���� ��������(M=�߾�): ")) ;target point

  ;--- entity ���� ���
  (setq diment (entget dimename))   ;entity ����
  ;--- definition point���
  (setq def13 (cdr (assoc 13 diment)))
  (setq def14 (cdr (assoc 14 diment)))
  ;--- ���� ���� ���ϱ�
  (setq alen (cdr (assoc 42 diment)))  ;actual length of dimension
  ;--- ��ġ�� �Է¹ޱ�
  (setq olenstr (strcat "\n���� ġ��:" (rto_dimtxt alen) " ��ġ���� �Է��ϼ���: ")) ;old length string
  (setq newdtxt (getstring olenstr))
  ;�α�ġ���� ó����� �Է¹ޱ�
  (initget "Stretch Move")
  (setq kw (getkword "\n�α�ġ������ Stretch(S), �α�ġ������ Move(M): "))
  
  ;--- ���� ġ���� ��ġ�� ���� ����ϱ�
  ;(setq dimdsep (getvar "dimdsep")) 			;decimal sparator
  (setq newlen (djdg_dimtxtoreal newdtxt))		;��ġ���� ��������
  (setq delta (- newlen alen))				;��ġ���� ����ġ���� ����
  ;--- ���õ� ġ���� �ø���
  (djdg_stretchdim dimename tp delta)			;�ش�ġ���� �ø���
  (djdgf_dt dimename)
  ;--- ������ ġ���� stretch �Ǵ� move�ϱ�
  ;--- �������� �÷����� �� ã��
  (setq dist13 (distance def13 tp))
  (setq dist14 (distance def14 tp))
  (setq ang (cdr (assoc 50 diment)))
  (if (<= dist13 dist14)
    (setq bp def14		
	  stp def13
	  stang (+ ang pi))		;14 --> 13����
    (setq bp def13		
	  stp def14
	  stang ang)			;13 --> 14����
  );if  
  ;--- selection set���� ������ entity ����
  (if (= nil (setq remset (ssdel dimename ssdim))) (setq remset ssdim))
  (setq nss (sslength remset))  			;������ selection set ����
  (cond
    ((= kw "Stretch")					;stretch���ý� target point���� ���ϰ�������� stretch 
      (setq i 0)
      (setq ;near 0
	    nearent nil)
      (repeat nss
	(setq dent (entget (ssname remset i)))
        (setq dst13 (djdg_angdist bp (cdr (assoc 13 dent)) stang))  ;������������ �Ÿ�, �����̸�+ �����̸� -
        (setq dst14 (djdg_angdist bp (cdr (assoc 14 dent)) stang))  ;������������ �Ÿ�, �����̸�+ �����̸� -	        
;        (setq dst13 (distance tp (cdr (assoc 13 dent)))  ;������������ �Ÿ�;
;	       dst14 (distance tp (cdr (assoc 14 dent))))
        (if (= nearent nil)
	  (progn						;�ִܱ⸮�� ���� ���ǵ��� �ʾ��� ��
 	    (if (> dst13 0) (setq near dst13 nearent dent))
	    (if (> dst14 0) (setq near dst14 nearent dent)) 
	  );progn
	  (progn
	    (if (and (>  dst13 0) (<= dst13 near))
	      (setq near dst13				;13�� ����̰� �ּҰ����� ������ 13�� �ֱٰŸ���
		 nearent dent)
	    );if
	    (if (and (>  dst14 0) (<= dst14 near))
	      (setq near dst14				;14�� ����̰� �ּҰ����� ������ 14�� �ֱٰŸ���
		 nearent dent)
	    );if  
	  );progn
	);if    
        (setq i (1+ i))		     
      ); repeat
      ;���� ����� entity�� ���ؼ� stretch
      (if (/= nearent nil)			;���尡���
	(progn
          (setq delta1 (* -1 delta))   		;delta��ȣ �ٲٱ�
          (djdg_stretchdim (cdr (assoc -1 dent)) tp delta1)
          (djdgf_dt (cdr (assoc -1 dent)))
	);progn
      );if	
    );sub cond
    ((= kw "Move")				;move�� �������� ��
      (setq i 0)
      (repeat nss
	(setq dent (entget (ssname remset i)))
        (setq sgndst (djdg_angdist bp (cdr (assoc 13 dent)) stang))  ;������������ �Ÿ� �����̸�+ �����̸� -
	(if (> sgndst 0)				;dimension�� ���ʿ� ���� ���� �ű��
	  (djdg_movedim (ssname remset i) stang delta)  ;stretch �������� delta��ŭ �ű��.
	);if
        (setq i (1+ i))		     			
      ); repeat

     );sub cond 
	
  );cond  
  (princ "\n����� ���������� �����")(princ)
);defun

;---------------------------------
; function : djdg_stretchdim
;            �־��� ���̸�ŭ dimension�� strech�Ѵ�.
;             Yi Suk Jong
;             05/08/18
;---------------------------------
; argument
;     ename : strech�� entity name
;       pnt : ������ (nil�� ��쿣 �������� �ø���)
;       dst : �ø��� �� (pnt�� nil�� ��� ���� dst/2��ŭ stretch)
;---------------------------------
(defun djdg_stretchdim(ename pnt dst
		       /   diment defp13 defp14 ang newdefp13 newdefp14 dst13 dst14 newdefp )
  (setq diment (entget ename))  ;ġ���� ���� ���
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
	  (entmod (subst (cons 13 newdefp) (assoc 13 diment) diment)) ;text ����
	);progn
	(progn
	  (setq newdefp (polar defp14 ang dst))
	  (entmod (subst (cons 14 newdefp) (assoc 14 diment) diment)) ;text ����	  
	);progn
      );if	
    ); progn
  );if  
)  ;defun


;---------------------------------
; function : djdg_movedim (move dimension)
;            �־��� ���̸�ŭ dimension�� move�Ѵ�.
;             Yi Suk Jong
;             05/08/20
;---------------------------------
; arguments
;     ename : move�� entity name
;       ang : move�� ���� (radian)
;       dst : move�� ��
;---------------------------------
(defun djdg_movedim(ename ang dst
		    / diment defp13 defp14 newdefp13 newdefp14 diment)
  (setq diment (entget ename))  ;ġ���� ���� ���
  (setq defp13 (cdr (assoc 13 diment)))  ;definition point 13
  (setq defp14 (cdr (assoc 14 diment)))  ;definition point 14

  (setq newdefp13 (polar defp13 ang dst))	;���� definition point �ٲٱ�
  (setq newdefp14 (polar defp14 ang dst))   	;���� definition point �ٲٱ�
  (setq diment (subst (cons 13 newdefp13) (assoc 13 diment) diment))
  (entmod (subst (cons 14 newdefp14) (assoc 14 diment) diment))
)  ;defun

