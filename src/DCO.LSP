;*****************************************
; Program : DC
;           Dimension Circle
;           By Jong-Suk Yi
;           1995/5/2
;****************************************

(defun C:DC(/
;ds elst cen rc sp sang ep eang seang
;cl txt dlst dcen rd dsp sep sgnpnt
;sgnr sgn dlevel mang dpnt wh4 tsgn
;tang txtpnt txtxy ltxt
)

  (push-env)                                                ;환경변수 대피

  (setq dim_gap 10.0                            ;치수선이 구조물에서 떨어진 거리
        ds (getvar "DIMSCALE")
        th (getvar "DIMTXT"))

  ;(setvar "cmdecho" 1)

  (setq elst (entget (car (entsel "Select Arc or Circle: "))))  ;길이의 기준호
  (setq cen (cdr (assoc 10 elst))                               ;중심
        rc  (cdr (assoc 40 elst)))                              ;반지름
  (setq sp (getpoint "Pick start point: "))                     ;시작점입력
  (setq sang (angle cen sp))
  (setq ep (getpoint "Pick end point: "))
  (setq eang (angle cen ep))
  (setq seang (abs (dang sang eang)))

  (princ (dang sang eang))
  (setq cl (/ (* 2 pi rc seang) (* 2 pi)))                    ;Arc의 길이
  (if (> cl 1000.0)
    (setq txt (rtos (* cl 0.001) 2 3))
    (setq txt (rtos cl 2 0))
  ) ;of if

  (setq dlst (entget (car (entsel "Select baseline circle: ")))) ;치수선 기준 Arc
  (setq dcen (cdr (assoc 10 dlst))                    ;치수선 기준 Arc의 중심
        rd   (cdr (assoc 40 dlst)))                   ;치수선 기준 Arc의 반지름
  (setq dsp (polar cen sang rd)                       ;치수보조선 시작점
        dep (polar cen eang rd))                      ;치수보조선 끝점

  (setq sgnpnt (getpoint "\nPick Side: "))            ;치수선의 위치 (위,아래?)
  (setq sgnr (distance cen sgnpnt))                   ;치수선 위치 부호
  (if (> sgnr rd)
    (setq sgn 1)
    (setq sgn -1)
  ) ;of if

  ; 치수선의 위치
  (setq dlevel (getint "\nDim Line level <1>: "))
  (if (= dlevel nil) (setq dlevel 1))
  (setq mang (/ (+ sang eang) 2.0))                   ;시작각과 끝각의 중간각
  ;(setq mang (- sang (/ (dang sang eang) 2.0)))                   ;시작각과 끝각의 중간각
  (setq dpnt (polar cen mang (+ rd (* 20 ds sgn) (* (- dlevel 1) dim_gap ds sgn))))
                                                      ;Dimension line의 위치

  (setq wh4 (which4 mang))
  ;(setq wh4 (which4 (angle dsp dep)))
  (if (or (= wh4 1) (= wh4 2))
      (setq tsgn 1)
      (setq tsgn -1)
  ) ;of if
  (setq tang (rtod (+ mang (* (/ PI 2.0) tsgn -1))))
  (setq txtpnt (polar dpnt mang (* th ds tsgn)))      ;text위치 (X,Y,Z)
  (setq txtxy (list (car txtpnt) (cadr txtpnt)))      ;text위치 (X,Y)
  (princ "\nText=<")
  (princ txt)
  (setq ltxt (getstring ">: "))
  (if (= ltxt "")
    (setq ltxt txt)
  ) ;of if

  (command "DIM1" "ANG" "" cen dsp dep "A" tang "T" ltxt dpnt txtxy)

  (pop-env)
  (princ)

) ;of defun
