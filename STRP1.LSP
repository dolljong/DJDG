;**************************************
; Program : STRP1.LSP
;           STiRruP - 1
;           Yi Suk-Jong
;           1995/12/22
;***************************************

(defun C:STRP1(/
            gap     tick    ds      pnt1    pnt2    ang     pnt11
            pnt22   pnt111  pnt222

)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (setq  gap 1.5
        tick 2.0)

  (push-env)

  (setq ds (getvar "DIMSCALE"))

  (setq pnt1 (getpoint "\nPick First Point: "))
  (setq pnt2 (getpoint pnt1 "\nPick Second point: "))

  (setq ang (angle pnt1 pnt2))

  (setq pnt11 (polar pnt1 (+ ang (/ pi -4.0)) (* gap ds 0.5)))
  (setq pnt22 (polar pnt2 (+ pi ang (/ pi 4.0)) (* gap ds 0.5)))

  (setq pnt111 (polar pnt11 (+ pi ang (/ pi -4.0)) (* gap ds)))
  (setq pnt222 (polar pnt22 (+ ang (/ pi 4.0)) (* gap ds)))

  (command "PLINE" pnt111 pnt11 pnt22 pnt222 "")

  (pop-env)
  (setq *error* oer seterr nil)

) ;of defun
