;************************************
; function : DANG
;            Delta ANGle
;            Suk-Jong Yi
;            1995. 3. 9
;************************************

(defun DANG(ang1 ang2 /     ; ang1, ang2 : radian
wang1 wang2 ang
)
(setq wang1 (+ (fix (/ ang1 (dtor 90.0))) 1))
(setq wang2 (+ (fix (/ ang2 (dtor 90.0))) 1))
(princ "\n ang1: ")
(princ wang1)
(princ "\n ang2: ")
(princ wang2)
(if (and (= wang1 1) (= wang2 4))
  (setq ang (* (+ ang1 (- (dtor 360.0) ang2)) -1))
  (if (and (= wang1 4) (= wang2 1))
    (setq ang (+ (- (dtor 360.0) ang1) ang2))
    (setq ang (- ang2 ang1))
  ) ;of if
) ;of if
) ;of defun

(defun c:kk(/ a1 a2)
(setq a1 (angle (setq p1 (getpoint "\nPick 1: ")) (getpoint p1 "Pick 2: ")))
(setq a2 (angle p1 (getpoint p1 "Pick 3: ")))
(princ (rtod a1))
(princ "\n")
(princ (rtod a2))
(princ "\n")
(princ (rtod (dang a1 a2)))
)

