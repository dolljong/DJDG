;*******************************************
; Program : STYH
;           STYle Height change
;           Suk-Jong Yi
;           1995/10/26
;*******************************************

(defun C:STYH(/

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

  (setq tent (entget (car (entsel "\nSelect Text: "))))
  (setq f_list (list (assoc 0 tent) (assoc 7 tent) (assoc 40 tent)))
  (princ f_list)
  (setq ss (ssget "X" f_list))
  (setq n_ss (sslength ss))

  (setq index 0)                      ;선택된 entity 강조
  (repeat n_ss
    (redraw (ssname ss index) 3)
    (setq index (1+ index))
  ) ;of repeat

  (princ "\n")                        ; 선택된 갯수 출력
  (princ n_ss)
  (princ " Text Found")

  (setq old_h (cdr (assoc 40 tent)))        ;새로운 height 입력
  (princ "\nOld Height = ")
  (princ old_h)
  (setq new_h (getreal "\nNew height: "))

  (setq index 0)
  (repeat n_ss
    (setq entl (entget (ssname ss index)))
    (setq new_ent (subst (cons 40 new_h) (assoc 40 entl) entl))
    (entmod new_ent)
    (redraw (ssname ss index) 4)            ;강조된 entity 원상복구
    (setq index (1+ index))
  ) ;of repeat
  (pop-env)
  (setq *error* oer seterr nil)
  (princ)
) ;of defun STYH

