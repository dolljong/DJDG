;****************************************
; Program : WAVECUT
;           WAVE CUT
;           By Suk-Jong Yi
;           95/7/8
;****************************************

(defun C:WAVE(/
            ds       gap     cp      sp      ep      dst     ang     ang90_p
            ang90_n  spu     spl     epu     epl     epu     epl     cpu
            cpl      oldc    pl1     sspl    r       cen1    cen2    ang1
            ang2     tp1     tp2     tp3     tp4     tp5     tp6     tp7
            tp8      tp9     tp10    tp11
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

  (setq ds (getvar "DIMSCALE"))
  (setq gap (* 0.5 ds 1.0))

  (initget "2")
  (setq cp (getpoint "\n<Pick center point>/2point: "))
  (if (= cp "2")
    (progn ;THEN
      (setq sp (getpoint "\nPick start point: "))
      (setq ep (getpoint sp "\nPick end point: "))
      (setq cp (mid-point sp ep))
      (setq dst (/ (distance sp ep) 2.0))
      (setq ang (angle sp ep))
    ) ;of THEN
    (progn ;ELSE
      (setq ep (getpoint cp "\nPick end point: "))
      (setq ang (angle cp ep))
      (setq dst (distance cp ep))
      (setq sp (polar cp (+ ang pi) dst))
    ) ;of ELSE
  ) ;of IF
  (setq ang90_p (+ ang (/ pi 2.0)))
  (setq ang90_n (- ang (/ pi 2.0)))
  (setq spu (polar sp ang90_p gap))
  (setq spl (polar sp ang90_n gap))
  (setq epu (polar ep ang90_p gap))
  (setq epl (polar ep ang90_n gap))
  (setq cpu (polar cp ang90_p gap))
  (setq cpl (polar cp ang90_n gap))

  (setq oldc (getvar "CECOLOR"))
  (setvar "CECOLOR" "3")
  (command "PLINE" spu "A" "A" "-90" cpu epu "")
  (setq pl1 (ssget "L"))
  (command "PLINE" spl "A" "A" "-90" cpl epl "")
  (setvar "CECOLOR" oldc)

  (setq sspl (ssadd (entlast) pl1))
  (setq r (* (sin (/ pi 4.0)) dst))
  (setq cen1 (polar sp (- ang (* pi 0.25)) r))
  (setq cen2 (polar cp (+ ang (* pi 0.25)) r))
  (setq ang1 (angle cen1 sp)
        ang2 (angle cen2 cp))
  (setq tp1 (polar cen1 ang1 r)
        tp2 (polar cen1 (- ang1 (* (/ pi 10) 1)) r)
        tp3 (polar cen1 (- ang1 (* (/ pi 10) 2)) r)
        tp4 (polar cen1 (- ang1 (* (/ pi 10) 3)) r)
        tp5 (polar cen1 (- ang1 (* (/ pi 10) 4)) r)
        tp6 (polar cen1 (- ang1 (* (/ pi 10) 5)) r)
        tp7 (polar cen2 (+ ang2 (* (/ pi 10) 1)) r)
        tp8 (polar cen2 (+ ang2 (* (/ pi 10) 2)) r)
        tp9 (polar cen2 (+ ang2 (* (/ pi 10) 3)) r)
        tp10 (polar cen2 (+ ang2 (* (/ pi 10) 4)) r)
        tp11 (polar cen2 (+ ang2 (* (/ pi 10) 5)) r))

  (command "TRIM" sspl "" "F"
           tp1 tp2 tp3 tp4 tp5 tp6 tp7 tp8 tp9 tp10 tp11 "" "")

  (pop-env)
  (setq *error* oer seterr nil)
  (princ)

) ;of defun
