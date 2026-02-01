;*******************************
; Program : TRIMB3
;           TRIMB 3
;           By Suk-Jong Yi
;           1995/7/25
;*******************************
; 두선의 끝을 일정 길이만큼 줄이고 두 끝을 이어준다

(defun C:TRIMB3( / off_dst   offdst_old  pnt1      pnt2      ss_ent    index
                   spnt      epnt        crs_pnt   dst_spnt  dst_epnt
                   new-pnt   ent         new_pnt1
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (if (= off_dst nil)
    (setq offdst_old 0.0)
    (setq offdst_old off_dst))
  (princ "\nOffset distance <")
  (princ offdst_old)
  (setq off_dst (getreal ">: "))
  (if (= off_dst nil) (setq off_dst offdst_old))

  (setq pnt1 (getpoint "\nSelect first point: "))
  (setq pnt2 (getpoint pnt1 "\nSelect second point: "))

  (while (and (/= pnt1 nil) (/= pnt2 nil))
    (setq ss_ent (ssget "F" (list pnt1 pnt2)))

    (setq index 0)
    (repeat 2
      (setq ent (entget (ssname ss_ent index)))
      (redraw (ssname ss_ent index))
      (setq spnt (cdr (assoc 10 ent)))
      (setq epnt (cdr (assoc 11 ent)))
      (setq crs_pnt (inters pnt1 pnt2 spnt epnt))
      (setq dst_spnt (distance spnt crs_pnt))
      (setq dst_epnt (distance epnt crs_pnt))
      (if (<= dst_spnt dst_epnt)
        (progn
          (setq new_pnt (polar epnt (angle epnt spnt)
                                    (- (distance epnt spnt) off_dst)))
          (setq ent (subst (cons 10 new_pnt) (assoc 10 ent) ent))
          (entmod ent)
        ) ;of THEN
        (progn
          (setq new_pnt (polar spnt (angle spnt epnt)
                                    (- (distance spnt epnt) off_dst)))
          (setq ent (subst (cons 11 new_pnt) (assoc 11 ent) ent))
          (entmod ent)
        ) ;of ELSE
      ) ;of IF
      (if (= index 0) (setq new_pnt1 new_pnt))
      (setq index (1+ index))
    ) ;of repeat

    (command "LINE" new_pnt new_pnt1 "")
    (setq pnt1 (getpoint "\nSelect first point: "))
    (setq pnt2 (getpoint pnt1 "\nSelect second point: "))

  ) ;of while
  (setq *error* oer seterr nil)
  (princ)
) ;of defun
