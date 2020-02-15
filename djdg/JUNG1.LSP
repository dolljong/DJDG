;**********************************
; Program : JUNG1
;           �߾Ӻи��� �׸���
;           By Suk-Jong Yi
;           1998/5/7
;*************************************

(defun C:JUNG1(/ p1)

  (defun SETERR(s)                                          ;���忡����ƾ ����
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)                         ;���忡����ƾ ����

  (push-env)                                                ;ȯ�溯�� ����

  (setq p1 (getpoint "\nPick insert point: "))              ;������ �Է¹���

  (command "INSERT" (strcat (prefix) "BLOCKS/jung1")        ;block �̸�
                    p1                                      ;insert��
                    "" "" "0.0")                            ;scale, angle

  (pop-env)                                                 ;ȯ�溯�� ����

  (setq *error* oer seterr nil)                             ;������ƾ ����

  (prin1)

) ;of defun
