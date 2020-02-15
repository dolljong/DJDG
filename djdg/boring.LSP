;**********************************
; Program : BORING
;           insert BORING mark
;           By Suk-Jong Yi
;           1998/2/3
;*************************************

(defun C:BORING(/ p1)

  (defun SETERR(s)                                          ;���忡����ƾ ����
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)                                                ;ȯ�溯�� ����

  (setq p1 (getpoint "\nPick insert point: "))

  (command "INSERT" (strcat (prefix) "BLOCKS/BORING")         ;block �̸�
                    p1                                      ;insert��á
                    (getvar "DIMSCALE") "" "0.0")           ;scale, angle

  (pop-env)                                                 ;ȯ�溯�� ����

  (setq *error* oer seterr nil)                             ;������ƾ ����

  (prin1)

) ;of defun
