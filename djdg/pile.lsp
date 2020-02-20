;**************************************
; Program : PILE
;           PILE drawing
;           Suk-Jong Yi
;           1995. 3. 18
;**************************************

(defun C:PILE(/
oldclt ds s-pnt pd rdo pt rdi pl tang ang tdsto tdsti e-pnt
sl-ont0o sr-pnt0o sl-pnt0i sr-pnt0i sl-pnto sr-pnto sl-pnti sr-pnti
el-pnto dr-pnto el-pnti er-pnti cl-pnt cr-pnt ll-pnt1 ll-pnt2 rl-pnt
lient rient cclr plent
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)
(push-env)                                      ;ȯ�溯�� ����

(setq oldclt (getvar "CELTYPE"))

(setq ds (getvar "DIMSCALE"))

(setq s-pnt (getpoint "\nPick start point: "))   ;������ �Է�
(setq pd (getdist "\nPile diameter: "))         ;���� ���� (����)
(setq rdo (/ pd 2.0))                            ;���� ������(����)
(setq pt (getdist "\Pile thickness: "))         ;���� �β�
(setq rdi (- (/ pd 2.0) pt))                     ;���� ������(����)
(setq pl (getdist s-pnt "\nPile length: "))
(initget "Tilt Angle")
(setq tang (getreal "\nTilt/<Angle>: "))       ;1:?�� �Է��Ϸ��� "T"�� �Է�
(if (= tang "Tilt")
  (progn
    (setq tang (getreal "\nTilt ratio(Holizontal:Vertival=1:?): "))
    (setq ang (+ (* pi (/ 3.0 2.0)) (atan (/ 1.0 tang))))   ;���Ⱚ�� ������ ȯ��
  ) ;of progn
  (setq ang (+ tang (* pi (/ 3.0 2.0))))
) ;of if

(setq tdsto (abs (/ rdo (sin ang))))        ;���ϰ�翡 ���� ���� �ݰ��� ����
(setq tdsti (abs (/ rdi (sin ang))))

(setq e-pnt (polar s-pnt ang pl))                   ;���� �߽� ����

(setq sl-pnt0o (polar s-pnt (- ang (/ pi 2.0)) rdo)) ;������ ���� ����(����)����
(setq sr-pnt0o (polar s-pnt (+ ang (/ pi 2.0)) rdo)) ;������ ���� ����(����)����
(setq sl-pnt0i (polar s-pnt (- ang (/ pi 2.0)) rdi)) ;������ ���� ����(����)����
(setq sr-pnt0i (polar s-pnt (+ ang (/ pi 2.0)) rdi)) ;������ ���� ����(����)����

(setq sl-pnto (list (- (car s-pnt) tdsto) (cadr s-pnt) 0.0))    ;�¿��� ����
(setq sr-pnto (list (+ (car s-pnt) tdsto) (cadr s-pnt) 0.0))    ;����� ����
(setq sl-pnti (list (- (car s-pnt) tdsti) (cadr s-pnt) 0.0))    ;�³��� ����
(setq sr-pnti (list (+ (car s-pnt) tdsti) (cadr s-pnt) 0.0))    ;�쳻�� ����

(setq el-pnto (polar sl-pnt0o ang pl))                   ;���� �¿��� ����
(setq er-pnto (polar sr-pnt0o ang pl))                   ;���� ����� ����
(setq el-pnti (polar sl-pnt0i ang (- pl (* rdo 0.25))))     ;���� �³��� ����
(setq er-pnti (polar sr-pnt0i ang (- pl (* rdo 0.25))))     ;���� �쳻�� ����

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
(setvar "CELTYPE" "HIDDEN")           ;���� Ÿ���� ��������
(command "LINE" sl-pnti el-pnti "")   ;���� �ȼ��� ������ �ȼ� �׸���
(setq lient (list (entlast) el-pnti))
(command "LINE" sr-pnti er-pnti "")
(setq rient (list (entlast) er-pnti))
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

(command "EXTEND" plent "" lient rient "")

(pop-env)                                       ;ȯ�溯�� ����
  (setq *error* oer seterr nil)

) ;of defun
