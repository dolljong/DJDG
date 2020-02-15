;*************************************
; Program : CDS
;           Change Dimension Scale
;           By Yi Suk-Jong
;           1998/3/10
;*************************************
; Ã¡®¬å· scale·i ¤aŽ´áº‘

(defun C:CDS (/
                ss_ent     ans     old_tvp     old_tad    new_tvp
)

  ;;
  ;; ¸w error routine
  ;;
  (defun SETERR(s)                                      ;¸w µAœážË¥ ¸÷·
  ;If an error (CTRL-C) occurs when this command is active.
    (if (/= s "Function cancelled")
      (if (= s "quit / exit abort")
        (princ "\n*cancel*")
        (princ (strcat "\nError " s))
      ) ;of if
    ); of If
  ;Restore previous error handler
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

;  (setq oer *error* *error* seterr)         ;¸wµAœážË¥ˆa•·

  (push-env)                                ;ÑÅ‰w¥e® ”Ï¡

  (setq ss_filter (list '(0 . "DIMENSION")))   ;filter»¡¸÷
  (setq ss_ent (ssget ss_filter))               ;DIM entity¬åÈ‚
  (setq dsc (getreal "\nEnter Scale: "))

;  (setq old_tvp (getvar "DIMTVP")           ; µ• DIM¥e® ”Ï¡
;        old_tad (getvar "DIMTAD"))
  (command "DIMEDIT" "H" ss_ent "")         ;¬åÈ‚–E dimension·i home·a¡

  (setq ndim (sslength ss_ent))             ;dimension ˆ•®
  (setq count 0)

  (repeat ndim                              ;¤e¥¢ ®Ð—
    (setq dim_ent (entget (ssname ss_ent count)))  ;dim¸÷¥¡
    (setq dline_point (cdr (assoc 10 dim_ent)))   ;Ã¡®¬å¸ñ
    (setq dend_point (cdr (assoc 14 dim_ent)))    ;Ã¡®¬å»¡¸÷¸ñ(definition point)
    (setq dim_angle (angle dend_point dline_point))     ;ˆb•¡ŠÐa‹¡
    (setq dim_dist (distance dend_point dline_point))   ;ˆáŸ¡ŠÐa‹¡
    (setq new_dline_point
          (polar dend_point dim_angle (* dim_dist dsc)));¬ Ã¡®¬å¸ñ ŠÐa‹¡
    (setq ass10 (assoc 10 dim_ent))                     ;assoc 10
    (setq newass10 (cons (car ass10) new_dline_point))  ;¬¸ñ
    (setq new_ent (subst newass10 ass10 dim_ent))       ;
    (entmod new_ent)                                    ;®¸÷Ða‹¡
    (setq count (1+ count))                 ;”a·qÃ¡®¬å·a¡
  ) ;of repeat

  (pop-env)                                ;ÑÅ‰w¥e® ¥¢Šá

  (princ)

;  (setq *error* oer seterr nil)

) ;of defun
