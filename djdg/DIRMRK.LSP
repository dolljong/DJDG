;***********************************
; Program : DIRMRK
;           DIRection MaRKing
;           Yi Suk-Jong
;           96/6/17
;***********************************

(defun C:DIRMRK(/)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)                                        ;환경변수 대피
  (setq ds (getvar "DIMSCALE"))                     ;스케일값

  (setq ip (getpoint "\nInsert point :"))           ;삽입점
  (setq sp (getpoint ip "\nDirection: "))              ;방향입력
  (setq ang (angle ip sp))                          ;각도

  (command "INSERT" (strcat (prefix) "blocks/dirmrk") ip
                    ds ds (rtod ang))               ;block삽입

  (pop-env)
  (setq *error* oer seterr nil)
  (princ)

) ;of defun
