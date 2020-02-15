;**********************************
; Porgram : BRSE
;           BRidge Start End
;           Jong-Suk Yi
;           98/5/4
;**********************************

(defun C:BRSE(/
ds ip ph dh hs txt1 txt2 l1 l2 txtl1 txtl2 pnt1 pnt2 pnt3 mpnt
oldc txtgap txtang txth txtpnt1 txtpnt2
)

  (defun SETERR(s)                                  ;���忡����ƾ ����
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)                 ;���忡����ƾ ����

  (push-env)                                          ;���� ȯ�溯�� ����

  (setq ds (getvar "DIMSCALE")                  ; dimscale
        th (getvar "DIMTXT"))                   ; textũ�� = dimtxt


  (setq ip (getpoint "\nPick insert point: "))                ;������ �Է�
  (setq ph (getpoint ip "\nPick Holizontal side: "))          ;������� �Է�

  (setq dh (- (car ph) (car ip)))                             ;������
  (setq hs (/ (abs dh) dh))                                   ;�����ȣ

  (if (= hs -1)
    (progn
      (setq txt1 (getstring "\nEnter TEXT <��������>: "))      ;����ؽ�Ʈ �Է�
      (if (= txt1 "") (setq txt1 "��������"))
    ) ;of progn
    (progn
      (setq txt1 (getstring "\nEnter TEXT <��������>: "))      ;�ؽ�Ʈ �Է�
      (if (= txt1 "") (setq txt1 "��������"))
    ) ;of progn
  ) ;of if

  (setq txt2 (getstring "\nEnter STA.: "))                     ;�ϴ��ؽ�Ʈ�Է�
  (if (= txt2 "")
    (setq txt2 "")
    (setq txt2 (strcat "STA. " txt2))
  ) ;of if

  (setq l1 5                        ;   insert point~������
        l2 10)                      ;   ������~������

  (setq l1 (* l1 ds)
        l2 (* l2 ds))               ; ������ ����

  (setq txtl1 (* th ds (+ (strlen txt1) 2)))  ;�α��ڸ� ���� ���ڰ� �����ϴ� ����
  (setq txtl2 (* th ds (+ (strlen txt2) 2)))  ;�α��ڸ� ���� ���ڰ� �����ϴ� ����

  (if (> txtl1 txtl2) (setq txtl txtl1) (setq txtl txtl2))     ;��ū���� ���α��̷�

  (setq pnt1 (list (car ip) (+ (cadr ip) l1))                  ;������
        pnt2 (list (car ip) (+ (cadr pnt1) l2))                ;�߰���
        pnt3 (polar pnt2 (+ (/ pi 2) (* (/ pi -3) hs)) txtl)   ;����
        mpnt (mid-point pnt2 pnt3))                            ;���̽����� �߰���

  (setq oldc (getvar "CECOLOR"))                               ;����� ����
  (setvar "CECOLOR" "RED")                                     ;���� ����������
  (command "PLINE" pnt1 pnt2 pnt3 "")                          ;�������� �׸���
  (setvar "CECOLOR" oldc)                                      ;������ ��������

  (setq txtgap (* 2 ds))                                       ;���̽�����~�ؽ�Ʈ

  (setq txtang (/ pi 6 hs))                                    ;�ؽ�Ʈ����(rad)
  (setq txth (* th ds))                                       ;�ؽ�Ʈ ����
  (setq txtpnt1 (polar mpnt (+ txtang (/ pi 2)) txtgap)        ;����ؽ�Ʈ ��ġ
        txtpnt2 (polar mpnt (- txtang (/ pi 2)) txtgap))       ;�ϴ��ؽ�Ʈ ��ġ

  (setq txtang (rtod txtang))                                  ;�ؽ�Ʈ����(deg)

  (command "TEXT" "J" "C" txtpnt1 txth txtang txt1)    ;�ؽ�Ʈ ��
  (command "TEXT" "J" "TC" txtpnt2 txth txtang txt2)    ;�ؽ�Ʈ ��

  (pop-env)                                             ;ȯ�溯�� ����

  (setq *error* oer seterr nil)                         ;���忡����ƾ ��

(princ)

) ;of defun