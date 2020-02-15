;*************************
; Program : PILES
;           PILES
;           Suk-Jong Yi
;           04/05/03
;*************************
; ���ұ��� �׷��ش�.(using dialog Box)
;*************************

(defun C:PILES(
/ ipnt d l n p count na a yscl)

  (defun SETERR(s)                          ;���忡����ƾ ����
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)         ;���忡����ƾ ����

  (push-env)                                                ;ȯ�溯�� ����

  (initget "2point")
    (setq sel (entsel "\nSelect footing line or [2point]: "))
    (if (= sel "2point")
      (progn
        (setq p1 (getpoint "\nPick start point: ")
	      p2 (getpoint p1 "\nPick end point: "))
      );if true
      (progn
        (setq ente (entget (car sel)))   ;edge selection
        (setq p1 (cdr (assoc 10 ente))
              p2 (cdr (assoc 11 ente)))
      );;if false
    );if  

  (setq width (abs (* (- (car p2) (car p1)) 0.001)))

  (piles_dia)
  
  (setq ipnt (mid-point p1 p2))
  (setq d (atoi dia))                     ;���� ���� (mm)
  (setq l (getreal "\nPile Length(m): "))                   ;���ϱ���  (m)
  (setq n (1+ (atoi num)))                    ;���� ����
  (setq p (atof pitch))                    ;���� ����

  (setq count 1
        na nil)                                             ;slop list

  (repeat n                                                 ;pile������ŭ �ݺ�
;    (princ "\nSlop of Pile ") (princ count) (princ "/") (princ n)
;    (setq a (getreal "<0>: "))
    (setq a 0)    
;    (if (= a nil) (setq a 0))
    (setq na (append na (list a)))
    (setq count (1+ count))
  ) ;of defun

;  (setq yscl (getreal "\nY-scal<1.0>: "))                   ;y-scale (����鵵)
  (if (= yscl nil) (setq yscl 1))

  (f_spiles ipnt  d  l  na  p yscl)                         ;���ϱ� �׸��� �Լ�ȣ��

  (pop-env)                                             ;ȯ�溯�� ����
  (setq *error* oer seterr nil)

) ;of defun

;***************************************
; Function : F_SPILES
;            Function Steel PILES
;            Suk-Jong Yi
;            96/7/19
;***************************************
; ���ϱ� �׸��� (steel pile)
; �Ѿ���� ��
;   IP : Insert Point
;    D : Dia             (mm)
;    L : Length          (m)
;   NA : Nunber & Angle
;    P : Pitch           (m)
; YSCL : Y-SCaLe         (default=1.0)
;***************************************
(defun F_SPILES(IP D L NA P YSCL
/  p n l ix iy ix1 count pnt)

  (setq p (* p 1000)                                ;mm������ ȯ��
        n  (length na)                              ;pile����
        l (* l yscl)                                ;pile���� �������� ȯ��
        ix (car ip)
        iy (cadr ip)
        ix1 (- ix (/ (* (1- n) p) 2))               ;ù ���� ������ x
        count 0)


  (repeat n                                         ;���ϵ� �׸���
    (setq pnt (list (+ ix1 (* count p)) iy))
    (f_spile pnt d l (* (nth count na) yscl))       ;���� �׸��� �Լ� ȣ��
    (setq count (1+ count))
  ) ;of repeat
) ;of defun

;(defun F_RCDS(
;/)
;
;)

;***************************************
; Function : F_SPILE
;           Steel PILE
;           Suk-Jong Yi
;           96/7/18
;***************************************
; ���������� �׷��ش�. (�ܸ��� ǥ��)
; IP : Insert Point
; D  : DIA    (mm)
; L  : Length (m)
; S  : Slop
;***************************************
(defun F_SPILE(IP D L S
/   oldclt ds s-pnt pd rdo pt rdi pl tang ang tdsto tdsti e-pnt
    sl-ont0o sr-pnt0o sl-pnt0i sr-pnt0i sl-pnto sr-pnto sl-pnti sr-pnti
    el-pnto dr-pnto el-pnti er-pnti cl-pnt cr-pnt ll-pnt1 ll-pnt2 rl-pnt
    lient rient cclr plent
)

  (if (= (tblsearch "LTYPE" "CENTER") nil)         ;�e�� Hidden type ���a�e load
    (command "LINETYPE" "L" "CENTER" "ACAD" "")
  ) ;of if

  (setq oldclt (getvar "CELTYPE"))

  (setq ds (getvar "DIMSCALE"))

  (setq s-pnt IP)                                       ;������ �Է�
  (setq pd D)                                           ;���� ���� (����)
  (setq rdo (/ pd 2.0))                                 ;���� ������(����)
  (setq pl L)                                           ;���� ����
  (setq pl (* pl 1000))                                 ;mm ������ ȯ��
  (setq tang S)

  (if (= tang 0)                                            ;���� 0�� ���
    (setq ang (+ (dtor tang) (* pi (/ 3.0 2.0))))           ;radian���� ġȯ
    (setq ang (+ (* pi (/ 3.0 2.0)) (atan (/ 1.0 tang))))   ;���Ⱚ�� ������ ȯ��
  ) ;of IF

  (setq tdsto (abs (/ rdo (sin ang))))        ;���ϰ�翡 ���� ���� �ݰ��� ����

  (setq e-pnt (polar s-pnt ang pl))                   ;���� �߽� ����

  (setq sl-pnt0o (polar s-pnt (- ang (/ pi 2.0)) rdo)) ;������ ���� ����(����)����
  (setq sr-pnt0o (polar s-pnt (+ ang (/ pi 2.0)) rdo)) ;������ ���� ����(����)����
  (setq sl-pnto (list (- (car s-pnt) tdsto) (cadr s-pnt) 0.0))    ;�¿��� ����
  (setq sr-pnto (list (+ (car s-pnt) tdsto) (cadr s-pnt) 0.0))    ;����� ����
  (setq el-pnto (polar sl-pnt0o ang pl))                   ;���� �¿��� ����
  (setq er-pnto (polar sr-pnt0o ang pl))                   ;���� ����� ����

  (setq cl-pnt (list (/ (+ (car e-pnt) (car el-pnto)) 2.0)
                     (/ (+ (cadr e-pnt) (cadr el-pnto)) 2.0) 0.0))
  (setq cr-pnt (list (/ (+ (car e-pnt) (car er-pnto)) 2.0)
                     (/ (+ (cadr e-pnt) (cadr er-pnto)) 2.0) 0.0))

  (setq ll-pnt1 (polar cl-pnt (+ ang pi) (* rdo 0.25))) ;���ϳ��� ����ǥ�� �׸���
  (setq ll-pnt2 (polar cl-pnt ang (* rdo 0.25)))        ;���� ���� ���ϱ�
  (setq rl-pnt (polar cr-pnt ang (* rdo 0.25)))

  (setvar "CELTYPE" "BYLAYER")          ;����Ÿ���� �Ǽ�����
  (command "LINE" sl-pnto el-pnto "")   ;���� �ٱ����� ������ �ٱ��� �׸���
  (command "LINE" sr-pnto er-pnto "")
  (setq cclr (getvar "CECOLOR"))
  (setvar "CECOLOR" "RED")
  (setvar "CELTYPE" "CENTER")           ;���� Ÿ���� �����⼱����
  (command "LINE" s-pnt e-pnt "")       ;�߽ɼ� �׸���
  (setvar "CECOLOR" cclr)

  (setvar "CELTYPE" oldclt)             ;�������� Ÿ������ ���ͽ�Ű��

  (command "PLINE" er-pnto "A" "S" rl-pnt e-pnt       ;���ϳ����� ����ǥ��
                               "S" ll-pnt1 el-pnto    ;���������� ��ũ��ɻ��
                               "S" ll-pnt2 e-pnt "")
  (setq plent (list (entlast) ll-pnt1))

  (setvar "CECOLOR" "GREEN")                            ;���� �������
  (setq rr (/ rdo 2.0))                                 ;ù Round Radious
  (while (>= rr (* 0.25 DS))                            ;�ֿ��� round�� 0.25���� ū ���ȸ�
    (setq tdsti (abs (/ (- rdo rr) (sin ang))))                 ;���ϰ�翡 ���� ���ϴ� ���� ������
    (setq sl-pnt0i (polar s-pnt (- ang (/ pi 2.0)) (- rdo rr))) ;���� ����(����)����
    (setq sr-pnt0i (polar s-pnt (+ ang (/ pi 2.0)) (- rdo rr))) ;���� ����(����)����
    (setq sl-pnti (list (- (car s-pnt) tdsti) (cadr s-pnt) 0.0))    ;���� ���� ����
    (setq sr-pnti (list (+ (car s-pnt) tdsti) (cadr s-pnt) 0.0))    ;���� ���� ����
    (setq el-pnti (polar sl-pnt0i ang (- pl (* rdo 0.3))))     ;���� ���� ����
    (setq er-pnti (polar sr-pnt0i ang (- pl (* rdo 0.3))))     ;���� ���� ����

    (command "LINE" sl-pnti el-pnti "")                 ;���� �ȼ��� ������ �ȼ� �׸���
    (setq lient (list (entlast) el-pnti))
    (command "LINE" sr-pnti er-pnti "")
    (setq rient (list (entlast) er-pnti))
    (command "EXTEND" plent "" lient rient "")
    (setq rr (/ rr 2.0))                                ;���� �Ÿ��� ������
  ) ;of WHILE

  (setvar "CECOLOR" "WHITE")                            ;���� �������

) ;of defun

;-----------------------------------------
; function : piles_DIA
;            get data using dialog box
;            Yi Suk Jong
;            04/05/03
;-----------------------------------------
(defun PILES_DIA (/
        dcl_id
  )

  (defun setdia( / )
    (setq dia (get_tile "dia"))
    (setq minpitch (strcat "m  Min: " (rtos (* 2.5 (atof dia) 0.001) 2 3) " m"))    
    (setq minedge (strcat "m  Min: " (rtos (* 1.25 (atof dia) 0.001) 2 3) " m"))
    (set_tile "pitchtxt"  minpitch)
    (set_tile "edgetxt"  minedge)
  );defun setdia

  (defun pile_check( / )
    (setq num (get_tile "num")
	  pitch (get_tile "pitch")
	  edge (get_tile "edge"))
    (setq w (+ (* (atof num) (atof pitch)) (* 2.0 (atof edge))))
    (setq widthtxt (rtos width 2 3)
	  wtxt (rtos w 2 3))
    (setq resulttxt (strcat wtxt (if (< w width) "<" (if (> w width) ">" "=")) widthtxt))
    (set_tile "result" (strcat "Width: " resulttxt))
    (if (/= w width) (alert resulttxt))
  );defun pile_check
  
  (setq dcl_id (load_dialog "DJDG.DCL"))                  ;dialogȣ��
  (if (not (new_dialog "PILES" dcl_id)) (exit))


;-------------------
; �ʱⰪ����
;-------------------

  (if (= dia nil) (setq dia "508"))
  (setq minpitch (strcat "m  Min: " (rtos (* 2.5 (atof dia) 0.001) 2 3) " m"))
  (setq minedge (strcat "m  Min: " (rtos (* 1.25 (atof dia) 0.001) 2 3) " m"))
  (if (= scthick nil) (setq scthick "12"))
  (if (= sctype nil) (setq sctype 1))
  (if (= #piletype nil) (setq #piletype 1))
    
;---------------------------
; dialog box �ʱ�ȭ
;---------------------------
  (set_tile "widthf" (strcat "Width of Footing: " (rtos width 2 3) "m"))
  (set_tile "dia" dia)
  (set_tile "pitchtxt"  minpitch)
  (set_tile "edgetxt"  minedge)
  (if (= #piletype 1)
    (set_tile "steeltype" "1")	
    (set_tile "conctype" "1")
  );if
  
  (action_tile "steeltype" "(setq #piletype 1)")   ;steel pile
  (action_tile "conctype" "(setq #piletype 2)")    ;conc pile 
  (action_tile "dia" "(setdia)")        
  (action_tile "num" "(setq num $VALUE)")      
  (action_tile "pitch" "(setq pitch $VALUE)")
  (action_tile "edge"   "(setq edge $VALUE)")

  (action_tile "check" "(pile_check)")
  
  (action_tile "accept"  "(done_dialog)")
  (action_tile "cancel"  "(exit)")
    
;   (mode_tile "dia" 2)

  (start_dialog)

  (unload_dialog dcl_id)

) ;of defun SPLICE_DIALOG
