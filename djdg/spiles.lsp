;*************************
; Program : SPILES
;           Steel PILES
;           Suk-Jong Yi
;           96/7/18
;*************************
; ���� ���ұ��� �׷��ش�.
;*************************

(defun C:SPILES(
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

  (setq ipnt (getpoint "\nPick Center of PILE group: "))    ;���ϱ��� ��� ��
  (setq d (getreal "\nPile Dia(mm): "))                     ;���� ���� (mm)
  (setq l (getreal "\nPile Length(m): "))                   ;���ϱ���  (m)
  (setq n (getint "\nNumber of pile: "))                    ;���� ����
  (setq p (getreal "\nPitch of Pile(m): "))                    ;���� ����

  (setq count 1
        na nil)                                             ;slop list

  (repeat n                                                 ;pile������ŭ �ݺ�
    (princ "\nSlop of Pile ") (princ count) (princ "/") (princ n)
    (setq a (getreal "<0>: "))
    (if (= a nil) (setq a 0))
    (setq na (append na (list a)))
    (setq count (1+ count))
  ) ;of defun

  (setq yscl (getreal "\nY-scal<1.0>: "))                   ;y-scale (����鵵)
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
  (setvar "CECOLOR" "1")
  (setvar "CELTYPE" "CENTER")           ;���� Ÿ���� �����⼱����
  (command "LINE" s-pnt e-pnt "")       ;�߽ɼ� �׸���
  (setvar "CECOLOR" cclr)

  (setvar "CELTYPE" oldclt)             ;�������� Ÿ������ ���ͽ�Ű��

  (command "PLINE" er-pnto "A" "S" rl-pnt e-pnt       ;���ϳ����� ����ǥ��
                               "S" ll-pnt1 el-pnto    ;���������� ��ũ��ɻ��
                               "S" ll-pnt2 e-pnt "")
  (setq plent (list (entlast) ll-pnt1))

  (setvar "CECOLOR" "3")                            ;���� �������
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

  (setvar "CECOLOR" "7")                            ;���� �������

) ;of defun

