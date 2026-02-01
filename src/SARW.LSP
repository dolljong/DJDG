;******************************************
;    Program : SARW
;              Section ARrow insertion
;              By Suk-Jong Yi
;              1995. 3. 2.
;******************************************

(defun C:SARW(/
sp ep ang sdp sang
)
  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

(setvar "CMDECHO" 0)
(setq ds (getvar "DIMSCALE"))
(setq sp (getpoint "\nPick Start point: "))
(setq ep (getpoint sp "\nPick End point: "))
(setq ang (angle sp ep))

(setq sdp (getpoint "\nPick Side: "))
(setq sdang (angle sp sdp))

(setq txt (strcase (getstring "\nEnter String: ")))
(setq psc ds)
(setq nsc (* -1 ds))
(setq asgn (/ (dang ang sdang) (abs (dang ang sdang))))

(setq xy1 (polar sp (+ ang (dtor 180.0)) (* 5.0 ds)))
(setq xy2 (polar ep ang (* 5.0 ds)))
(setq txy1 (polar xy1 (+ ang (* (dtor 90.0) asgn)) (* 15.0 ds)))
(setq txy2 (polar xy2 (+ ang (* (dtor 90.0) asgn)) (* 15.0 ds)))

(if (>= asgn 0.0)
(setq ys psc)
(setq ys nsc)
)
(setq sang (- (rtod ang) 360.0))
(command "INSERT" (strcat (prefix) "BLOCKS/SECARW") sp psc ys sang)
(command "INSERT" (strcat (prefix) "BLOCKS/SECARW") ep nsc ys sang)
(command "TEXT" "J" "M" txy1 (* ds 4.0) "0" txt)
(command "TEXT" "J" "M" txy2 (* ds 4.0) "0" txt)
  (setq *error* oer seterr nil)
(princ)
)
