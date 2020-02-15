;**************************************************
; Program : solidbk
;           move solid back
;           Yi Suk-Jong
;           99/4/2
;**************************************************
; 솔리드만 선택하여 draworder명령으로 back으로 보내준다
;
;**************************************************

(defun C:solidbk( / scl ans ssc nc llist count cent cc rc fn ipnt ans34)

  (defun SETERR(s)					;내장에러루틴 정의
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)			;내장에러루틴 기동

  (push-env)                                ;환경변수 대피

  (initget "All Select")
  (setq kw (getkword "\nSelect entity/<All>"))
  (if (= kw "All")
    (setq ssc (ssget x '((-4 . "<OR")           ;모든 solid나 solid hatch만 선택
                           (0 . "SOLID")
                           (-4 . "<AND")
                             (0 . "HATCH")
                             (2 . "SOLID")
                           (-4 . "AND>")
                         (-4 . "OR>"))))
    (setq ssc (ssget '((-4 . "<OR")           ;solid나 solid hatch만 선택
                         (0 . "SOLID")
                         (-4 . "<AND")
                           (0 . "HATCH")
                           (2 . "SOLID")
                         (-4 . "AND>")
                       (-4 . "OR>"))))
  );if
  (setq nc (sslength ssc))                  ;solid들의 갯수
  (if ssc
    (command "DRAWORDER" ssc "" "")              ;draworder
    (princ "\nSolid not found")
  );if

) ;of defun


