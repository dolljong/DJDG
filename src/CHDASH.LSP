;********************************************
; command : chdash
; Change a line to Dashed Line
;           By Yi Suk Jong
;                   1994,5
;********************************************

(defun c:chdash(/ ss count e oldlt newlt)
  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)
  (setq ss (ssget))
  (setq count 0)
  (while
    (setq e (ssname ss count))
    (setq ent (entget e))
    (setq oldlt (assoc 6 ent))
    (if (= oldlt nil)
       (setq ent (append ent (list (cons 6 "dashed"))))
       (progn
          (setq newlt (cons 6 "DASHED"))
          (setq ent (subst newlt oldlt ent))
       ) ; progn
    ) ; if
    (entmod ent)
    (setq count (+ count 1))
  ) ; while
  (setq *error* oer seterr nil)
  (princ)
)
