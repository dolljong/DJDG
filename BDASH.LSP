;***********************************
; Program : BDASH
;           Between DASHed
;           Suk-Jong Yi
;           1995/11/5
;***********************************

(defun C:BDASH(/
               sel  fpnt  spnt  oldlt
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)

  (setq sel (entsel "\nSelect Line: "))
  (setq fpnt (getpoint "\nFirst point: "))
  (setq spnt (getpoint "\nSecond point: "))

  (command "BREAK" sel "F" fpnt spnt)

  (if (= (tblsearch "LTYPE" "HIDDEN") nil)         ; e·© Hidden type ´ô·a¡e load
    (command "LINETYPE" "L" "HIDDEN" "ACAD" "")
  ) ;of if

  (setq oldlt (getvar "CELTYPE"))
  (setvar "CELTYPE" "HIDDEN")
  (command "LINE" fpnt spnt "")
  (setvar "CELTYPE" oldlt)

  (pop-env)
  (setq *error* oer seterr nil)

) ;of defun
