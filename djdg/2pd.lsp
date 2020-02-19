;************************************
; Program : 2PD
;           2 Point Distance
;           By Suk-Jong Yi
;           1995/5/23
;************************************
; ���� ������ �Ÿ��� ���鿡 ǥ������
;************************************

(defun C:2PD(/
              th    ta    pref    pnt1   pnt2    txt    tpnt
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (setvar "CMDECHO" 0)
;  (setq th (getvar "TEXTSIZE"))
  (setq th (getvar "DIMTXT"))                 ;text�� ũ��� ġ���� ũ���
  (setq pref (getstring "\nPrefix: "))        ;�Ӹ��� �Է�

  (while (setq pnt1 (getpoint "\nPick first point: "))  ;ù�� �Է�
    (setq pnt2 (getpoint pnt1 "\nPick second point: "))      ;��°�� �Է�

    (setq ang (angle pnt1 pnt2))                        ;������ �̷�� ��

    (setq wh4 (which4 ang))                             ;���и鿡 �ִ°�?

    (cond                                               ;1~4��и鿡 ���� ��
       ((= wh4 1)
         (setq ta ang)
       )
       ((= wh4 2)
         (setq ta (- ang pi))
       )
       ((= wh4 3)
         (setq ta (- ang pi))
       )
       ((= wh4 4)
         (setq ta (- ang (* 2 pi)))
       )
    );of cond

    (setq dst (distance pnt1 pnt2))                    ;������ �Ÿ� ���ϱ�
    (if (< dst 1000.0)
      (setq txt (rtos dst 2 (getvar 0)))                          ;1000�̸��� ��
      (setq txt (rtos (* dst 0.001) 2 (getvar "LUPREC")))                ;1000�̻��� ��
    ) ;of if(dst < 1000)

    (if (/= pref nil)
      (setq txt1 (strcat pref txt))                     ;�۸Ӹ� ���̱�
      (setq txt1 txt)                                   ;�۸Ӹ� ���� ��
    ) ;of if

    (princ "\nTEXT: ") (princ txt1)                      ;���� �ؽ�Ʈ �����ֱ�

    (initget "Prefix")
    (setq tpnt (getpoint "\nChange [P]refix/<Pick point>: "))       ;������ �Է� ����
    (if (= tpnt "Prefix")                               ;�۸Ӹ� �ٲٰ� ���� ��
      (progn
        (setq pref (getstring "\nNew Prefix: "))            ;�Ӹ��� �Է�
        (setq txt1 (strcat pref txt))                       ;�۸Ӹ� ���ϱ�
        (princ "\nTEXT: ") (princ txt1)                     ;���� �ؽ�Ʈ �����ֱ�
        (setq tpnt (getpoint "\nPick point: "))             ;������ �Է� �ޱ�
      ) ;of progn
    ) ;of if

    (command "TEXT" "C" tpnt th (rtod ta) txt1)                 ;�ؽ�Ʈ ��
  ) ;of while
  (setq *error* oer seterr nil)
) ;of defun
