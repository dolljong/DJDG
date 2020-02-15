;************************************
; Program : 2PD
;           2 Point Distance
;           By Suk-Jong Yi
;           1995/5/23
;************************************
; ���� �a���� �១�i ���e�A �a��Ё��
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
  (setq th (getvar "DIMTXT"))                 ;text�� �a���e á���� �a����
  (setq pref (getstring "\nPrefix: "))        ;�១�i ���b

  (while (setq pnt1 (getpoint "\nPick first point: "))  ;���� ���b
    (setq pnt2 (getpoint pnt1 "\nPick second point: "))      ;�i���� ���b

    (setq ang (angle pnt1 pnt2))                        ;���� �����e �b

    (setq wh4 (which4 ang))                             ;�y�a���e�A ���e�a?

    (cond                                               ;1~4�a���e�A ���i ��
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

    (setq dst (distance pnt1 pnt2))                    ;���� �១ ���a��
    (if (< dst 1000.0)
      (setq txt (rtos dst 2 (getvar 0)))                          ;1000���e�� ��
      (setq txt (rtos (* dst 0.001) 2 (getvar "LUPREC")))                ;1000���w�� ��
    ) ;of if(dst < 1000)

    (if (/= pref nil)
      (setq txt1 (strcat pref txt))                     ;�i�១ ������
      (setq txt1 txt)                                   ;�i�១ ���i ��
    ) ;of if

    (princ "\nTEXT: ") (princ txt1)                      ;�e�� �B�a�a ���a����

    (initget "Prefix")
    (setq tpnt (getpoint "\nChange [P]refix/<Pick point>: "))       ;�s���� ���b �h�q
    (if (= tpnt "Prefix")                               ;�i�១ �a���� ���i ��
      (progn
        (setq pref (getstring "\nNew Prefix: "))            ;�១�i ���b
        (setq txt1 (strcat pref txt))                       ;�i�១ ���a��
        (princ "\nTEXT: ") (princ txt1)                     ;�e�� �B�a�a ���a����
        (setq tpnt (getpoint "\nPick point: "))             ;�s���� ���b �h��
      ) ;of progn
    ) ;of if

    (command "TEXT" "C" tpnt th (rtod ta) txt1)                 ;�B�a�a �q
  ) ;of while
  (setq *error* oer seterr nil)
) ;of defun
