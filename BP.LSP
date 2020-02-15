;***********************************************
; Program : BB
;           Break
;           Yi Suk-Jong
;           1996/3/4
;***********************************************

(defun C:BP(/
            ent     pt
)
  (push-env)
  (setq ent (entsel "\nSelect Entity: "))
  (setq pt (getpoint "\nPick break point: "))
  (command "BREAK" ent "F" pt pt)
  (pop-env)
  (princ)
) ;of defun
