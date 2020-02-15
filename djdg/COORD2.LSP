;*********************************************
; Program : COORD2
;           Coord Marking
;           Yi Suk-Jong
;           96/5/28
;*********************************************
; ÁÂÇ¥°ªÀ» µµ¸é¿¡ Ç¥½Ã
; x=12.345
; y=24.456
;*********************************************
(defun c:coord2()

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (setq ts (* (getvar "DIMTXT") (GETVAR "DIMSCALE")))       ;dimension textÅ©±â

  (setq pt1 (getpoint "Pick First Point:"))
  (setq pt2 (getpoint pt1 "Pick Second Point:"))
  (setq px (car pt1))
  (setq py (cadr pt1))
  ;****** real to string
  (setq pxt (strcat "X=" (rtos px 2 (getvar "luprec"))))    ;xÁÂÇ¥°ª
  (setq pyt (strcat "Y=" (rtos py 2 (getvar "luprec"))))    ;yÁÂÇ¥°ª
  (command "dim1" "leader" pt1 pt2 "" pxt)
  (setq txtpnt (cdr (assoc 10 (entget (entlast)))))         ;text»ðÀÔÁ¡
  (setq txtpnt1 (list (car txtpnt)
                      (- (cadr txtpnt) (* 1.5 ts)) 0.0))            ;yÁÂÇ¥ »ðÀÔÁ¡
  (if (< (car pt2) (car pt1)) 
    (setq algn "MR")
    (setq algn "ML")
  )
  (command "TEXT" algn txtpnt1 ts "0.0" pyt)                ;yÁÂÇ¥°ª ¾²±â

  (setq *error* oer seterr nil)
  (princ)

);;; End of program COORD

