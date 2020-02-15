;**************************************************
; Program : solidbk
;           move solid back
;           Yi Suk-Jong
;           99/4/2
;**************************************************
; �ָ��常 �����Ͽ� draworder�������� back���� �����ش�
;
;**************************************************

(defun C:solidbk( / scl ans ssc nc llist count cent cc rc fn ipnt ans34)

  (defun SETERR(s)					;���忡����ƾ ����
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)			;���忡����ƾ �⵿

  (push-env)                                ;ȯ�溯�� ����

  (initget "All Select")
  (setq kw (getkword "\nSelect entity/<All>"))
  (if (= kw "All")
    (setq ssc (ssget x '((-4 . "<OR")           ;��� solid�� solid hatch�� ����
                           (0 . "SOLID")
                           (-4 . "<AND")
                             (0 . "HATCH")
                             (2 . "SOLID")
                           (-4 . "AND>")
                         (-4 . "OR>"))))
    (setq ssc (ssget '((-4 . "<OR")           ;solid�� solid hatch�� ����
                         (0 . "SOLID")
                         (-4 . "<AND")
                           (0 . "HATCH")
                           (2 . "SOLID")
                         (-4 . "AND>")
                       (-4 . "OR>"))))
  );if
  (setq nc (sslength ssc))                  ;solid���� ����
  (if ssc
    (command "DRAWORDER" ssc "" "")              ;draworder
    (princ "\nSolid not found")
  );if

) ;of defun

