;**************************************************
; command : chcen
; change a line type to center line
;               By  Yi Suk Jong
;                        94/5, 95/4/15
;**************************************************

(defun c:chcen(/ ss count e oldlt newlt)
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
    (setq oldc (assoc 62 ent))
    (if (= oldlt nil)
       (setq ent (append ent (list (cons 6 "center"))))
       (progn
          (setq newlt (cons 6 "center"))
          (setq ent (subst newlt oldlt ent))
       ) ; progn
    ) ; if
    (if (= oldc nil)
       (setq ent (append ent (list (cons 62 1))))
       (progn
          (setq newc (cons 62 1))
          (setq ent (subst newc oldc ent))
       ) ; progn
    ) ; if
    (entmod ent)
    (setq count (+ count 1))
  ) ; while
  (setq *error* oer seterr nil)
  (princ)
)
