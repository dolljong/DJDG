;**********************************
;*  DIVIDE
;*       Divide Arc
;*            Suk-Jong Yi
;*            1995. 2. 2.,3. 22
;**********************************

(defun C:DIVA(/
            ent     cen     r       spnt    epnt    sang    eang    ;지역변수 정의
            dvnum   dtang   count   ang     dpnt
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
(setq ent (entget (car (entsel "\nSelect Arc or Circle"))))

;*** Get center & radius
(setq cen (cdr (assoc 10 ent)))
(setq r (cdr (assoc 40 ent)))

;*** Get start point & end point
(setq spnt (getpoint "\nPick start point: "))
(setq epnt (getpoint "\nPick end point: "))
(setq dpnt (getpoint spnt "\nPick Direction Pint: "))

;*** Get start angle & end angle
(setq sang (angle cen spnt))
(setq eang (angle cen epnt))
(setq dpang (angle cen dpnt))   ;방향 포인트의 각

;*** Get divide number
(setq dvnum (getint "\nNumber of divide: "))

(setq pang (+ (- (* 2 pi) sang) eang)) ;시작점~끝점의 양의 방향각
(if (> pang (* 2.0 pi)) (setq pang (- pang (* 2.0 pi))))
(setq nang (- pang (* 2 pi)))             ;시작점~끝점의 음의 방향각
(setq sdsgn (/ (abs (dang sang dpang)) (dang sang dpang))) ;방향을 구함

(cond
  ((> sdsgn 0.0) (setq deltaa pang))
  ((< sdsgn 0.0) (setq deltaa nang))
)

;*** Calc delta ang ;시작점과 끝점의 각 차이를 나눌 수로 나눔
(setq dtang (/ deltaa dvnum))

;*** Repeat divide while count<=dvnum-1
(setq count 1)
(while (< count dvnum)
   (progn
      (setq ang (+ sang (* count dtang)))
      (setq dpnt (polar cen ang r))
      (command "point" dpnt)
      (setq count (+ count 1))
   ) ;of progn
   nil
) ;of while
  (setq *error* oer seterr nil)
) ;of defun

