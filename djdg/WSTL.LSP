;************************************
; Program : WSTL
;           Write STeel Length
;           By Suk-Jong Yi
;           1998/11/19
;************************************
; ������ ������ ���̸� ���ش�
; wlen.lsp�� ���� �����ϰ� �Ǿ�������
; wstl�� ���õ� ���翡 ���ؼ� ���̸� �����ش�.
;************************************

(defun C:WSTL(/
              th    ta    pnt1   pnt2    txt    tpnt
)

  (defun SETERR(s)                          ;���忡����ƾ
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)         ;���忡����ƾ ����

  (setvar "CMDECHO" 0)
  (setq th (getvar "TEXTSIZE"))

  (princ "\nPrefix<")(princ pref)(princ ">: ")
  (setq npref (getstring))                                  ;�Ӹ��� �Է�

  (if (/= npref "") (setq pref npref))                      ;Enter�Է½� ����������

  (setq pref (strcase pref))                                ;�빮�ڷ� ��ȯ

;  (while (setq pnt1 (getpoint "\nPick first point: "))  ;ù�� �Է�
;    (setq pnt2 (getpoint pnt1 "\nPick second point: "))      ;��°�� �Է�

  (while (setq lent (entget (car (entsel "\nSelect Line Entity: "))))  ;line����

    (setq pnt1 (cdr (assoc 10 lent))
          pnt2 (cdr (assoc 11 lent)))

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

    (setq dst (distance pnt1 pnt2))                    	;������ �Ÿ� ���ϱ�
    (if (< dst 1000.0)
      (setq txt (rtos dst 2 0))                         ;1000�̸��� ��
      (progn						;100mm������ �ڸ���
        (if (< (rem dst 100) 50)
          (setq dst (- dst (rem dst 100)))
          (setq dst (- dst (rem dst 100) -100))
        );if

        (setq txt (rtos (* dst 0.001) 2 3))                ;1000�̻��� ��

      );progn
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
        (princ "\n")(princ "Old Prefix : ")(princ pref)
        (setq pref (getstring "\nNew Prefix: "))            ;�Ӹ��� �Է�
        (setq txt1 (strcat pref txt))                       ;�۸Ӹ� ���ϱ�
        (princ "\nTEXT: ") (princ txt1)                     ;���� �ؽ�Ʈ �����ֱ�
        (setq tpnt (getpoint "\nPick point: "))             ;������ �Է� �ޱ�
      ) ;of progn
    ) ;of if

    (command "TEXT" "C" tpnt th (rtod ta) txt1)                 ;�ؽ�Ʈ ��
  ) ;of while

  (setq *error* oer seterr nil)                             ;���忡����ƾ ����

) ;of defun