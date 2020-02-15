;**********************************
; program : jung2
;           insert jungbundae
;           By Yi Seok Jong (dolljong)
;           2000/7/28
;--------------------------------------

(defun C:JUNG2(/ p1)

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

  (command "INSERT" (strcat (prefix) "BLOCKS/jung2")        ;block �̸�
                    p1                                      ;insert��
                    "" "" "0.0")                            ;scale, angle

  (pop-env)                                                 ;ȯ�溯�� ����

  (setq *error* oer seterr nil)                             ;������ƾ ����

  (prin1)

) ;of defun
