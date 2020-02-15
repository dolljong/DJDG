;*************************************
;  Program : ARD
;            ARray Distance
;            By Jong-Suk Yi
;            1995. 2. 27
;*************************************
; º´á»¥ Šˆe·i arrayÐe”a.
;*************************************

(defun c:ARD(/
ffd spnt epnt ang dst ffdst num
ent ffpnt n entpnt entpp ffpnt
)
  (defun SETERR(s)                              ;¸w µAœážË¥ ¸÷·
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)             ;¸wµAœážË¥ ˆa•·

  (setvar "CMDECHO" 0)

  (setq ent (ssget))                               ;array µEË¡Ë¡ ¬åÈ‚
  (setq spnt (getpoint "\nPick start point: "))     ;¯¡¸b¸ñ ·³b
  (setq epnt (getpoint spnt "\nPick end point: "))  ;{¸ñ ·³b

  (setq dx (- (car epnt) (car spnt)))               ;xÀa
  (setq dy (- (cadr epnt) (cadr spnt)))             ;yÀa

  (if (> (abs dx) (abs dy))                         ;Çe½¢ ˆáŸ¡ Îa¯¡
    (progn (princ "\nDistance = ") (princ dx))
    (progn (princ "\nDistance = ") (princ dy))
  ) ;of if

  (setq arl (getdist "\nArray distance: "))          ;arrayˆe‰b

  (if (< dx 0) (setq dxsgn -1) (setq dxsgn 1))      ;x ¦Ñ¡
  (if (< dy 0) (setq dysgn -1) (setq dysgn 1))      ;y ¦Ñ¡

  (setq arxn (1+ (fix (abs (/ dx arl))))                 ;xˆ•®
        aryn (1+ (fix (abs (/ dy arl)))))                ;yˆ•®

  (if (> arxn aryn)                                 ;ˆb‰b »¡¸÷
    (setq ardst (* arl dxsgn)
          aryn 1
          remain (rem dx arl))
    (setq ardst (* arl dysgn)
          arxn 1
          remain (rem dy arl))
  ) ;of if

  (command "ARRAY" ent "" "R" aryn arxn ardst)      ;array®Ð—

  (princ "\nRemainder = ") (princ remain)           ;a á»¡ Îa¯¡

  (setvar "CMDECHO" 1)
  (setq *error* oer seterr nil)
  (princ)
);; of defun ARD
