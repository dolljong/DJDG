;*****************************************    
; Program : DC    
;           Dimension Circle    
;           By Jong-Suk Yi    
;           96/6/7    
;****************************************    
; ȣ�� ���̸� ǥ�����ݴϴ�.1
;****************************************    

;------------------------
; Program: DIMA
;          DIMension Arc
;          Yi Suk Jong
;          04/03/29
;------------------------


(defun C:DC(/    
ds elst cen rc sp sang ep eang seang    
cl txt dlst dcen rd dsp sep sgnpnt    
sgnr sgn dlevel mang dpnt wh4 tsgn    
tang txtpnt txtxy ltxt    
)    
    
  (defun SETERR(s)    
    (if (/= s "Function cancelled")    
        (princ (strcat "\nError: " s))    
    ); of If    
    (setq *error* oer seterr nil)    
    (princ)    
  ); of SETERR    
  (setq oer *error* *error* seterr)    
    
  (setvar "BLIPMODE" 0)    
  (setvar "CMDECHO" 0)    
;  (push-env)                                                ;ȯ�溯�� ����    
    
  (setq dim_gap (getvar "DIMDLI")                            ;ġ������ ���������� ������ �Ÿ�    
        ds (getvar "DIMSCALE")                                  ;�����ϰ�    
        th (getvar "DIMTXT"))                                                 ;textũ��    
    
  (setq elst (entget (car (entsel "\nSelect Arc or Circle: "))))  ;������ ����ȣ    
  (setq cen (cdr (assoc 10 elst))                               ;�߽�    
        rc  (cdr (assoc 40 elst)))                              ;������    
    
  (setq dlst (entget (car (entsel "Select baseline circle: ")))) ;ġ���� ���� Arc    
  (setq dcen (cdr (assoc 10 dlst))                    ;ġ���� ���� Arc�� �߽�    
        rd   (cdr (assoc 40 dlst)))                   ;ġ���� ���� Arc�� ������    
    
  (setq sgnpnt (getpoint "\nPick Side: "))            ;ġ������ ��ġ (��,�Ʒ�?)    
  (setq sgnr (distance cen sgnpnt))                   ;ġ���� ��ġ ��ȣ    
  (if (> sgnr rd)    
    (setq sgn 1)    
    (setq sgn -1)    
  ) ;of if    
    
; ġ������ ��ġ    
  (setq dlevel (getint "\nDim Line level <1>: "))    
  (if (= dlevel nil) (setq dlevel 1))    
    
  (setq sp (getpoint "Pick start point: "))                     ;�������Է�    
  (setq sang (angle cen sp))                                    ;�߽����� �������� �̷�� ��    
    
  (setq next (getdist sp "\nDistance or RETURN to Pick point: ")) ;�������� �Ÿ�    
  (cond                                               ;���� �Է��Ϸ��� �����Է�    
    ((= next nil)    
      (setq ep (getpoint "\nPick point: "))                     ;������ �Է�    
      (setq epang (angle cen ep))                               ;������ ����    
      (if (< epang sang) (setq lsgn 1) (setq lsgn -1))          ;ġ���� �������    
    ) ;cond(next=nil)    
    ((numberp next)                                             ;dx�� ������ ���    
       (setq ep (polar cen (- sang (/ next rc)) rc))             ;����    
       (if (> next 0) (setq lsgn 1) (setq lsgn -1))             ;ġ���� �������    
    ) ;cond(next=number)    
  ) ;of cond    
    
    
  (while (/= ep nil)    
    
    (setq eang (angle cen ep))                                    ;�߽����� ������ �̷�� ��    
    (setq seang (abs (dang sang eang)))                           ;���������� ���� ��    
    
    (setq cl (/ (* 2 pi rc seang) (* 2 pi)))                    ;Arc�� ����    
    (if (> cl 1000.0)    
;      (setq txt (rtos (* cl 0.001) 2 3))    
      (setq txt (rtos_dimdsep (* cl 0.001) 3))          
      (setq txt (rtos cl 2 0))    
    ) ;of if    
    
    (setq dsp (polar cen sang rd)                       ;ġ�������� ������    
          dep (polar cen eang rd))                      ;ġ�������� ����    
    
    (setq mang (/ (+ sang eang) 2.0))                   ;���۰��� ������ �߰���    
    (setq dpnt (polar cen mang (+ rd (* 20 ds sgn) (* (- dlevel 1) dim_gap ds sgn))))    
                                                            ;Dimension line�� ��ġ    
    
    (setq wh4 (which4 mang))    
    (if (or (= wh4 1) (= wh4 2))    
        (setq tsgn 1)    
        (setq tsgn -1)    
    ) ;of if    
    (setq tang (rtod (+ mang (* (/ PI 2.0) tsgn -1))))    
    (setq txtpnt (polar dpnt mang (* 1.1 th ds tsgn)))         ;text��ġ (X,Y,Z)    
    (setq txtxy (list (car txtpnt) (cadr txtpnt)))          ;text��ġ (X,Y)    
    (princ "\nText=<")    
    (princ txt)    
    (setq txt1 (getstring ">: "))    
    
  (if (= txt1 "@")    
    (progn    
      (setq divl (getint "\nDivision length: "))            ;������ ���� �Է�    
      (setq divn (rtos (/ cl divl) 2 0))                    ;���� �������    
      (if (< divl 1000.)    
        (setq divl (rtos divl 2 0))                         ;1000�̸��� ��    
;        (setq divl (rtos (* 0.001 divl) 2 3))               ;1000�̻��� ��    
        (setq divl (rtos_dimdsep (* 0.001 divl) 3))               ;1000�̻���     
	) ;of if    
      (setq txtlen (* (+ (strlen txt) (strlen divn) (strlen divl) 2) th ds  ;text��ü����    
                   (cdr (assoc 41 (tblsearch "STYLE" (getvar "TEXTSTYLE"))))))    
      (if (>= txtlen cl)                       ;ġ�������� ���� text �ȵ���    
        (progn    
          (setq dtxt1 (strcat divn "@" divl))       ;�� �Ʒ� ���ٷ� ������(ù��)    
          (setq dtxt2 (strcat "=" txt))             ;                     (��°��)    
          (setq dtxt1p (polar dpnt mang (* th ds tsgn)))    
          (setq dtxt2p (polar dpnt mang (* -1 th ds tsgn)))    
          (setq oldosmode (getvar "OSMODE"))(setvar "OSMODE" 0)    
	  (command "TEXT" "M" dtxt1p (* th ds) tang dtxt1)    
          (command "TEXT" "M" dtxt2p (* th ds) tang dtxt2)    
          (command "DIM1" "ANG" "" cen dsp dep "A" tang "T" " " dpnt txtxy)    
          (setvar "OSMODE" oldosmode)    
	) ;of progn    
        (progn                                 ;ġ�������� ���� text ����    
          (setq dtxt1 (strcat divn "@" divl "=" txt))    
          (setq oldosmode (getvar "OSMODE"))(setvar "OSMODE" 0)    
	  (command "DIM1" "ANG" "" cen dsp dep "A" tang "T" dtxt1 dpnt txtxy)    
          (setvar "OSMODE" oldosmode)    
	) ;of progn ELSE    
      ) ;of IF    
    ) ;of progn THEN    
    (progn    
      (if (= txt1 "") (setq txt1 txt))                  ;�����Է½� �� text�� ��    
        (setq oldosmode (getvar "OSMODE"))(setvar "OSMODE" 0)    
        (command "DIM1" "ANG" "" cen dsp dep "A" tang "T" txt1 dpnt txtxy)    
        (setvar "OSMODE" oldosmode)    
    ) ;of progn ELSE    
  ) ;of if(txt1=@)    
    
    (setq sp ep)                                            ;������ ù������    
    (setq sang (angle cen sp))                              ;�߽����� �������� �̷�� ��    
    
    (initget "eXit Undo")    
    (setq next (getdist "\nDistance or RETURN to Pick point/eXit: ")) ;�������� �Ÿ�    
    (cond                                                   ;���� �Է��Ϸ��� �����Է�    
      ((= next nil)    
        (setq ep (getpoint "\nPick point: "))               ;������ �Է�    
      ) ;cond(next=nil)    
      ((= next "eXit")                                      ;eXit�Է½� ep=nil    
        (setq ep nil)    
      ) ;cond(next="eXit")    
      ((numberp next)                                       ;dx�� ������ ���    
        (setq ep (polar cen (- sang (/ next rc lsgn)) rc))  ;����    
      ) ;cond(next=number)    
    ) ;of cond    
    
  ) ;of while    
    
  (pop-env)                                                 ;ȯ�溯�� ����    
  (setq *error* oer seterr nil)    
  (princ)    
    
) ;of defun    
    
    
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


;------------------------
; Program: DIMA
;          DIMension Arc
;          Yi Suk Jong
;          04/03/29
;------------------------
(defun c:DIMA()
  (setq dimgap (getvar "DIMDLI")
	ds     (getvar "DIMSCALE"))
  
  (setq aent (entget (car (entsel "\nSelect ARC: "))))
  (setq cpnt (cdr (assoc 10 aent)) ;center point
	r (cdr (assoc 40 aent))   ;radius
	sang (cdr (assoc 50 aent))     ; start ang
	eang (cdr (assoc 51 aent)))    ; end ang
  (setq pnt (getpoint cpnt "\nPick original point: "))
  (if (> sang eang)
    (setq eang (+ eang (* 2 pi)))
  );if
    
  (setq avang (* 0.5 (+ sang eang)))  ;average of angle

  (setq ro (distance cpnt pnt))   ; radius of original point
  (setq spnt (polar cpnt sang ro)
	epnt (polar cpnt eang ro))
  (setq cl (* r (abs (dang eang sang))))
  (if (< cl 1000)
    (setq lentxt (rtos cl 2 0))
    (setq lentxt (rtos cl 2 3))
  );if  
  (setq txtpnt (polar cpnt avang (+ ro (* 20 ds))))
  (push-os)
  (command "DIM1" "ANG" "" cpnt spnt epnt txtpnt lentxt "")
  (pop-os)
	
)


