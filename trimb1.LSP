;*******************************************
; Program : TRIMB1
;           TRIM B 1
;           By Suk-Jong Yi
;           1995/7/13
;*******************************************
; 두 직선이나 호 사이의 물체를 TRIM

(defun C:TRIMB1(/
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

(setq ent1 (entget (car (entsel "\nSelect first entity: "))))
(setq ent2 (entget (car (entsel "\nSelect second entity: "))))

(setq ent1_type (cdr (assoc 0 ent1))
      ent2_type (cdr (assoc 0 ent2)))

(cond
  ((and (= ent1_type "LINE") (= ent2_type "LINE"))
    (setq ent1s (cdr (assoc 10 ent1))
          ent1e (cdr (assoc 11 ent1))
          ent2s (cdr (assoc 10 ent2))
          ent2e (cdr (assoc 11 ent2)))
    (setq dstss (distance ent1s ent2s)
          dstse (distance ent1s ent2e))

    (if (< dstse dstss)
      (progn ;THEN
        (setq temp ent2s)
        (setq ent2s ent2e)
        (setq ent2e temp)
      ) ;of progn
    ) ;of IF
    (setq ms (mid-point ent1s ent2s)
          me (mid-point ent1e ent2e))
    (command "TRIM"
             (list (cdr (assoc -1 ent1)) ent1s)
             (list (cdr (assoc -1 ent2)) ent2s) ""
             "F" ms me "" "")
  ) ;of cond(line-line)
) ;of COND

(pop-env)

  (setq *error* oer seterr nil)
(princ)
) ;of defun

