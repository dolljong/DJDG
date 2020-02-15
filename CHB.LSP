;**************************************************
; CHange a line type to reBar
;               By  Yi Suk Jong
;                        95/4/14
;**************************************************
;entity를 철근의 색과 line type으로 바꿔준다. (노란색, bylayer)
;**************************************************

(defun C:CHB(/
            ss      count   e   oldlt   newlt   oldc    newc
)
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
;라인타입바꾸기
    (if (= oldlt nil)
       (setq ent (append ent (list (cons 6 "CONTINUOUS"))))
       (progn
          (setq newlt (cons 6 "CONTINUOUS"))
          (setq ent (subst newlt oldlt ent))
       ) ; progn
    ) ; if
;색깔 바꾸기
    (if (= oldc nil)
       (setq ent (append ent (list (cons 62 2))))
       (progn
          (setq newc (cons 62 2))
          (setq ent (subst newc oldc ent))
       ) ; progn
    ) ; if
    (entmod ent)
    (setq count (+ count 1))
  ) ; while
  (setq *error* oer seterr nil)
  (princ)
)
;*******************************
