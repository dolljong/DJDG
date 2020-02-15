;**********************************
;*  MEASA
;*       Measure Arc
;*            Suk-Jong Yi
;*            1995. 2. 3.,3. 22.
;**********************************

(defun C:MEASA(/
;ent cen ra spnt sang eang anges             ;지역변수 정의
;larc mleng dvnum dtang count ang dpnt
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)
;*** Select entity
(setq ent (entget (car (entsel "\nSelect Arc or Circle: "))))

;*** Get center & radius
(setq cen (cdr (assoc 10 ent)))
(setq ra (cdr (assoc 40 ent)))

;*** Get start point & end point
(setq spnt (getpoint "\nPick start point: "))
(setq epnt (getpoint "\nPick end point: "))
(setq dpnt (getpoint spnt "\nPick Direction point: "))

;*** Get measure length
(setq mleng (getdist "\nMeasure length: "))

;*** Get start angle & end angle
(setq sang (angle cen spnt))
(setq eang (angle cen epnt))
(setq dpang (angle cen dpnt))
(setq sdsgn (/ (abs (dang sang dpang)) (dang sang dpang))) ;방향을 구함

(setq pang (+ (- (* 2 pi) sang) eang)) ;시작점~끝점의 양의 방향각
(if (> pang (* 2.0 pi)) (setq pang (- pang (* 2.0 pi))))
(setq nang (- pang (* 2 pi)))          ;시작점~끝점의 음의 방향각

(cond
  ((> sdsgn 0.0) (setq deltaa pang))
  ((< sdsgn 0.0) (setq deltaa nang))
) ;of cond

(setq larc (* ra deltaa))


;*** Get number of divide
(setq dvnum (abs (fix (/ larc mleng))))


;*** Calc delta ang
;(setq dtang (* (/ mleng ra) sdsgn))
(setq dtang (* (/ mleng ra) sdsgn))
;(princ "dtang: ")
;(princ dtang)

;*** Repeat divide while count<=dvnum-1
(setq count 1)
(while (<= count dvnum)
   (progn
;      (setq ang (+ sang (* count dtang)))
      (setq ang (+ sang (* count dtang)))
;      (princ "\n  Count: ")
;      (princ count)
;      (princ "  Angle : ")
;      (princ ang)
      (setq ppnt (polar cen ang ra))
;      (princ "  Coord. : ")
;      (princ ppnt)
      (command "point" ppnt)
      (setq count (+ count 1))
   ) ;of progn
   nil
) ;of while
  (setq *error* oer seterr nil)
) ;of defun

