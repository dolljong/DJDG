;*************************************
; Program : DUD
;           Dimension Up Down
;           By Suk-Jong Yi
;           1995/8/14
;*************************************
; Ã¡®¬å· Ã¡®Ÿi ¶á/´aœ¡ ¹¡¸÷

(defun C:DUD (/
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

  (setq oer *error* *error* seterr)

  (push-env)                               ;ÑÅ‰w¥e® ”Ï¡

  (setq ss_ent (ssget))                    ;DIM entity¬åÈ‚

  (setq old_tvp (getvar "DIMTVP")        ; µ• DIM¥e® ”Ï¡
        old_tad (getvar "DIMTAD"))

  (while (/= ss_ent nil)                   ;¤e¥¢ ®Ð—
    (initget "Up Down")                    ;
    (setq ans (getkword "\nUp/<Down>: "))    ; Up/Down ¬åÈ‚
    (if (= ans nil) (setq ans "Down"))      ; Ÿ¡Èå ·³b¯¡ Down·a¡ ˆeº
    (cond
      ((= ans "Up") (setq new_tvp 1))      ; UP/DownµA ˜aœa DIMTVPˆt ¬é¸÷
      ((= ans "Down") (setq new_tvp -1))
    ) ;of cond

    (setvar "DIMTAD" 0)                    ; ¬ DIM¥e® ¬é¸÷
    (setvar "DIMTVP" new_tvp)
    (command "DIM1" "UPDATE" ss_ent "")    ; UPDATE¡ww ®Ð—
    (setvar "DIMTVP" old_tvp)              ; µ• DIM¥e® ¥¢Šá
    (setvar "DIMTAD" old_tad)
    (setq ss_ent (ssget))                  ; ”a·q selection set ¬åÈ‚
  ) ;of while

  (pop-env)                                ;ÑÅ‰w¥e® ¥¢Šá
  (princ)

  (setq *error* oer seterr nil)

) ;of defun
