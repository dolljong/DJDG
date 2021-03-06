;***********************************
; Program : WFLOW
;           Water FLOW
;           Yi Suk-Jong
;           98/5/7
;***********************************

(defun C:WFLOW(/)

  (defun SETERR(s)                                  ;내장에러루틴 정의
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)                 ;내장에러루틴 가동

  (push-env)                                        ;환경변수 대피
  (setq ds (getvar "DIMSCALE"))                     ;스케일값

  (setq ip (getpoint "\nInsert point :"))           ;삽입점
  (setq sp (getpoint ip "\nDirection: "))           ;방향입력
  (setq ang (angle ip sp))                          ;각도

  (command "INSERT" (strcat (prefix) "blocks/wflow") ip
                    ds ds (rtod ang))               ;block삽입

  (pop-env)                                         ;환경변수 복귀

  (setq *error* oer seterr nil)                     ;에러루틴 복귀

  (princ)

) ;of defun
