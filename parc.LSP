;************************************************
; Program : PARC
;           Point on the ARC
;           By Yi suk jong
;           1995. 1. 24, 3. 22, 5/27
;************************************************
; 호위의 한점에서 일정길이(호의 길이)떨어진 점의 좌표를 리턴
;

(defun C:PARC(/
ent elist cen ra sp sang                ;지역변수 정의
len rang ps spang rrang rxy
)

(setq ent (entsel "Select Arc or Circle: "))
(setq elist (entget (car ent)))
(setq cen (cdr (assoc 10 elist)))
(setq ra (cdr (assoc 40 elist)))
(setq sp (getpoint "Pick start point: "))
(setq sang (angle cen sp))
(setq len (getdist "Enter Arc Lenth: "))
(setq rang (/ len ra))
(setq ps (getpoint sp "Pick Which side: "))
(setq spang (angle cen ps))
(setq deltaa (dang sang spang))
(setq absang (/ deltaa (abs deltaa)))
(setq rrang (+ sang (* absang rang)))

(setq rxy (polar cen rrang ra))

) ;;; End of program ARCL
