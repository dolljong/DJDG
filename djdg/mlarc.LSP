;******************************************
; Porgram : MLARC
;           Multi Get length of arc or circle
;           By Yi suk jong
;           1998/2/4
;******************************************
; 연속으로 호의 길이를 구해줌

(defun c:MLARC(/
ent elist cen ra sp                     ;지역변수 정의
ep sang eang seang cl
)
  (defun SETERR(s)                                  ;내장 에러루틴 정의
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (setq ent (entsel "Select Arc or Circle: "))      ;원,호 선택
  (setq elist (entget (car ent)))
  (setq cen (cdr (assoc 10 elist)))                 ;중심점
  (setq ra (cdr (assoc 40 elist)))                  ;반지름

  (setq sp (getpoint "Pick start point: "))         ;시작점
  (setq sang (angle cen sp))                        ;시작각

  (setq ep (getpoint "Pick End point: "))           ;첫끝점

  (while (/= ep nil)
    (setq eang (angle cen ep))                      ;끝각
    (setq seang (abs (dang sang eang)))             ;각차

    (setq cl (/ (* 2 Pi ra seang) (* 2 pi)))        ;호길이
    (princ "\nLength of ARC : ")
    (princ cl)
    (setq ep (getpoint "Pick End point: "))         ;끝점
  ) ;of while

  (setq *error* oer seterr nil)                     ;에러 루틴 복귀
  (princ)

);;; End if program MLARC

