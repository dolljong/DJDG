;************************************
; Program : THUN
;           THENder marking
;           Suk-Jong Yi
;           1995. 3. 14
;************************************

(defun C:THUN(/
        ds      oldosnp     oldcmde     sp      ep      ang     sp1     ep1
        sp2     ep2         mp          slpnts  slpnte  spnts0  spnte0  spnts1
        spnte1  spnts2      spnte2      ents1   ents2   ents3   tps     ente1
        ente2   ente3       tpe
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (setq ds (getvar "DIMSCALE"))
  (setq oldosnp (getvar "OSMODE"))
  (setq oldcmde (getvar "CMDECHO"))
  (setq oldclr (getvar "CECOLOR"))

  (setvar "BLIPMODE" 0)
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)


  (setq sp (getpoint "\nPick start point: "))
  (setq ep (getpoint sp "\nPick end point: "))

  (setq ang (angle sp ep))
  (setq sp1 (polar sp (+ ang (dtor 90.0)) (* 0.5 ds)))
  (setq ep1 (polar ep (+ ang (dtor 90.0)) (* 0.5 ds)))
  (setq sp2 (polar sp (- ang (dtor 90.0)) (* 0.5 ds)))
  (setq ep2 (polar ep (- ang (dtor 90.0)) (* 0.5 ds)))

  (setq mp (polar sp ang (/ (distance sp ep) 2.0)))

  (setq slpnts (polar mp (+ ang (dtor 135.0)) (* 6.0 ds)))
  (setq slpnte (polar mp (+ ang (dtor 315.0)) (* 6.0 ds)))
  (setq spnts0 (polar slpnts (+ ang (dtor 305.0)) (* 30.0 ds)))
  (setq spnte0 (polar slpnte (+ ang (dtor 125.0)) (* 30.0 ds)))

  (setq spnts1 (inters sp2 ep2 spnts0 slpnts))
  (setq spnte1 (inters sp1 ep1 spnte0 slpnte))

  (setq spnts2 (inters sp1 ep1 spnts0 slpnts))
  (setq spnte2 (inters sp2 ep2 spnte0 slpnte))

  (setvar "CECOLOR" "3")
  (command "LINE" slpnts slpnte "")

  (command "LINE" sp1 spnts2 "")
  (setq ents1 (list (entlast) sp1))
  (command "LINE" sp2 spnts1 "")
  (setq ents2 (list (entlast) sp2))
  (command "LINE" slpnts spnts1 "")
  (setq ents3 (list (entlast) slpnts))
  (setq tps (polar sp2 ang (- (distance sp2 spnts1) (* 0.1 ds))))

  (command "LINE" ep1 spnte1 "")
  (setq ente1 (list (entlast) ep1))
  (command "LINE" ep2 spnte2 "")
  (setq ente2 (list (entlast) ep2))
  (command "LINE" slpnte spnte1 "")
  (setvar "CECOLOR" oldclr)
  (setq ente3 (list (entlast) slpnte))
  (setq tpe (polar ep1 (+ ang pi) (- (distance ep1 spnte1) (* 0.1 ds))))


  (command "TRIM" ents1 ents2 ents3 "" "F" tps sp "" "")
  (command "TRIM" ente1 ente2 ente3 "" "F" tpe ep "" "")

  (setvar "OSMODE" oldosnp)
  (setvar "CMDECHO" oldcmde)
  (setq *error* oer seterr nil)
  (princ)

) ; of defun

