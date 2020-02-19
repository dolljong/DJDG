;*******************************************    
; Program : DO    
;           Dimension Oblique    
;           Jong-Suk Yi    
;           96/4/16    
;*******************************************    
; Vertical DIM�� OBLIQUE�����ش�.    
; �������� - ���� DIM���� �ش�ȴ�.    
;          - OBLIQUE���� 30���� ������ �ִ�.    
;*******************************************    
    
(defun C:DO(/    
divl    divn    dp      ds      dsel    dtx     dtxt1   dtxt1p  dtxt2    
dtxt2p  dx      dxy     dy      ep      fst     lstdim  next    pnt1    
pnt2    ppnt    sent    sgn     sp      th      txt     txt1    txtlen    
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
      
;  (push-env)                                          ;ȯ�溯���� ����    
    
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
        (setq sp pnt2) (setq sp pnt1))                     ;�� ���� sp��    
    ) ;of progn THEN    
  ) ;of IF(sp=Object)    
    
  (setq dp (getpoint "\nPick Dimension side: "))          ;ġ������ ��ġ�� ����    
    
  (setq dtx (- (car dp) (car sp)))    
  (setq sgn (/ dtx (abs dtx)))                            ;���� ������ ��ȣ    
    
  (setq fst (getint "\nDimension line LEVEL <1>: "))      ;ġ���� level�Է�    
  (if (= fst nil) (setq fst 1))    
  (setq dx (* ds (+ 15 (* dim_gap (- fst 1)))))           ;�������� ġ�����ǰŸ�    
    
  (setq next (getdist sp "\nDistance or RETURN to Pick point: ")) ;�������� �Ÿ�    
  (cond                                                ;���� �Է��Ϸ��� �����Է�    
    ((= next nil)    
      (setq ep (getpoint "\nPick point: "))               ;������ �Է�    
      (setq ep (list (car sp) (cadr ep)))                 ;������ ����    
    ) ;cond(next=nil)    
    ((numberp next)                                       ;dx�� ������ ���    
      (setq ep (list (car sp) (+ (cadr sp) next)))        ;ep ��ġ���    
    ) ;cond(next=number)    
  ) ;of cond    
    
    
  (while (/= ep nil)                                  ;ep�� nil�� �ƴѵ��� �ݺ�    
    
    (setq dxy (list (+ (car ep) (* dx sgn)) (car ep) 0.0))  ;ġ������ ���� ��ġ    
    
    (setq dy (distance sp ep))                          ;�� ���� �Ÿ�    
    (if (< dy 1000.0)    
      (setq txt (rtos dy 2 0))                          ;1000�̸��� ��    
;      (setq txt (rtos (* dy 0.001) 2 3))                ;1000�̻��� ��    
      (setq txt (rtos_dimdsep (* dy 0.001) 3))                ;1000����      
    ) ;of if(dy < 1000)    
    
    (princ "\nDimension text <")                        ;Dimension textǥ��    
    (princ txt)    
    (setq txt1 (getstring T ">: "))                     ;���ο� dimension text�Է�    
    (if (= (substr txt1 1 1) "@")    
      (progn    
;        (setq divl (getint "\nDivision length: "))      ;������ ���� �Է�    
        (setq divl (atof (substr txt1 2 (1- (strlen txt1)))))    
        (setq divn (rtos (/ dy divl) 2 0))              ;���� �������    
        (if (< divl 1000.)    
          (setq divl (rtos divl 2 0))                   ;������ ���̰� 1000�̸���    
;          (setq divl (rtos (* divl 0.001) 2 3))) ;of if  ������ ���̰� 1000�̻��    
          (setq divl (rtos_dimdsep (* divl 0.001) 3))) ;of if  ������ ���̰� 1000�̻�    
	  (setq txtlen (* (+ (strlen txt) (strlen divn) (strlen divl) 2) th ds    
                     (cdr (assoc 41 (tblsearch "STYLE" (getvar "TEXTSTYLE"))))))    
        (if (>= txtlen dy)    
          (progn                                  ;text�� ������ ���� �ȵ���    
            (setq dtxt1 (strcat divn "@" divl))   ;���ٷ� ����    
            (setq dtxt2 (strcat "=" txt))    
            (setq dtxt1p (mapcar '+ (mid-point sp ep)    
                                    (list (- (* dx sgn) (* ds th))  ;x��ġ    
                                          (* dx (/ (sin (/ pi 6)) (cos (/ pi 6))))    
                                          0.0)))                     ;z��ġ    
            (setq dtxt2p (mapcar '+ (mid-point sp ep)    
                                    (list (+ (* dx sgn) (* ds th))  ;x��ġ    
                                          (* dx (/ (sin (/ pi 6)) (cos (/ pi 6))))    
                                          0.0)))                     ;z��ġ    
            (setq oldosmode (getvar "OSMODE")) (setvar "OSMODE" 0)    
	    (command "TEXT" "M" dtxt1p (* th ds) "90" dtxt1)    
            (command "TEXT" "M" dtxt2p (* th ds) "90" dtxt2)    
            (command "DIM1" "VER" sp ep dxy " ")              ;DIM��� ����    
            (setvar "OSMODE" oldosmode)    
	  ) ;of progn THEN    
          (progn                                  ;text�� ������ ���� ����    
            (setq dtxt1 (strcat divn "@" divl "=" txt))    
            (setq oldosmode (getvar "OSMODE")) (setvar "OSMODE" 0)    
	    (command "DIM1" "VER" sp ep dxy dtxt1)            ;DIM��� ����    
            (setvar "OSMODE" oldosmode)    
	  ) ;of progn ELSE    
        ) ;of IF    
      ) ;of progn THEN    
      (progn    
        (if (= txt1 "") (setq txt1 txt))                      ;�����Է½� �� text�� ��    
        (setq oldosmode (getvar "OSMODE")) (setvar "OSMODE" 0)    
	(command "DIM1" "VER" sp ep dxy txt1)             ;DIM��� ����    
        (setvar "OSMODE" oldosmode)    
      ) ;of progn ELSE    
    ) ;of if(txt1=@)    
    
    (setq lstdim (entlast))                               ;��� ������� dim����    
    (setq oldexo (getvar "DIMEXO"))    
    (setvar "DIMEXO" 3)    
    (command "DIM1" "OBL" lstdim "" (* sgn 30))           ;30����ŭ ������    
    (command "DIM1" "UPDATE" lstdim "")    
    (setvar "DIMEXO" oldexo)    
    
    (setq sp ep)                    ;������ ù������    
    (initget "eXit Undo")    
    (setq next (getdist "\nDistance or RETURN to Pick point/eXit: ")) ;�������� �Ÿ�    
    (cond                                               ;���� �Է��Ϸ��� �����Է�    
      ((= next nil)    
        (setq ep (getpoint "\nPick point: "))                 ;������ �Է�    
        (setq ep (list (car sp) (cadr ep)))                   ;������ ����    
      ) ;cond(next=nil)    
      ((= next "eXit")                                        ;eXit�Է½� ep=nil    
        (setq ep nil)    
      ) ;cond(next="eXit")    
      ((numberp next)                                         ;dx�� ������ ���    
        (setq ep (list (car sp) (+ (cadr ep) next)))          ;ep ��ġ���    
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
