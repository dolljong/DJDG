;***********************************
; Program : WFLOW
;           Water FLOW
;           Yi Suk-Jong
;           98/5/7
;***********************************

(defun C:WFLOW(/)

  (defun SETERR(s)                                  ;¸wµAœážË¥ ¸÷·
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)                 ;¸wµAœážË¥ ˆa•·

  (push-env)                                        ;ÑÅ‰w¥e® ”Ï¡
  (setq ds (getvar "DIMSCALE"))                     ;¯aÅA·©ˆt

  (setq ip (getpoint "\nInsert point :"))           ;¬s·³¸ñ
  (setq sp (getpoint ip "\nDirection: "))           ;¤wÐ··³b
  (setq ang (angle ip sp))                          ;ˆb•¡

  (command "INSERT" (strcat (prefix) "blocks/wflow") ip
                    ds ds (rtod ang))               ;block¬s·³

  (pop-env)                                         ;ÑÅ‰w¥e® ¥¢Šá

  (setq *error* oer seterr nil)                     ;µAœážË¥ ¥¢Šá

  (princ)

) ;of defun
