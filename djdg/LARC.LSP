;******************************************
; Porgram : LARC
;           Get length of arc or circle
;           By Yi suk jong
;           1995. 1. 20, 3. 22.
;******************************************

(defun c:LARC(/
ent elist cen ra sp                     ;지역변수 정의
ep sang eang seang cl
)
  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

(setq ent (entsel "Select Arc or Circle: "))
(setq elist (entget (car ent)))
(setq cen (cdr (assoc 10 elist)))
(setq ra (cdr (assoc 40 elist)))
(setq sp (getpoint "Pick start point: "))
(setq ep (getpoint "Pick End point: "))
(setq sang (angle cen sp))
(setq eang (angle cen ep))
(setq seang (abs (dang sang eang)))

(setq cl (/ (* 2 Pi ra seang) (* 2 pi)))
(princ "\nLength of ARC : ")
(princ cl)
  (setq *error* oer seterr nil)
(princ)

);;; End if program LARC

