;****************************************
; Program : Cutpipe
;           Cutpipe
;           By Suk-Jong Yi
;           00/4/25
;****************************************
;파이프를 절단한 모양으로 잘라준다.
;----------------------------------------

(defun C:cutpipe(
   /
   p1 p2 p3 len ang1 ang2 a2sgn p40 p5 p60 pi2 len4 p4 p6 p7
   arc1 cen1 cen11 r sang1 arc2 cen2 cen22 sang2 angarc index
)

  (defun SETERR(s)                          ;내장에러루틴 정의
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  
  (setq oer *error* *error* seterr)         ;내장에러루틴 가동

  (push-env)                                ;환경변수 대피

  (setq p1 (getpoint  "\nPick First point: ")          ;절단 시작점
        p2 (getpoint p1 "\nPick Second point: ")       ;절단 끝점
        p3 (getpoint "\nPick Side: "))                 ;방향점

  (setq len (distance p1 p2)                           ;절단길이
        ang1 (angle p1 p2)                             ;절단선 각도
        ang2 (angle p1 p3))                            ;절단방향 각도
	 
  (if (minusp (dang ang1 ang2)) (setq a2sgn -1) (setq a2sgn 1))  ;절단선각도와 절단방향각도의 관계

  (setq p40 (polar p1 ang1 (* len 0.25))               ;절단길이의 1/4위치
        p5 (polar p1 ang1 (* len 0.5))                 ;절단길이의 1/2위치
        p60 (polar p1 ang1 (* len 0.75)))              ;절단길의의 3/4위치

  (setq pi2 (/ pi 2.0)                                 ;90도
        len4 (* len 0.1))                              ;절단길이의 1/10

  (setq p4 (polar p40 (+ ang1 (* a2sgn pi2)) len4)     ;호를 그리기위한 보조점
	p6 (polar p60 (- ang1 (* a2sgn pi2)) len4)
	p7 (polar p60 (+ ang1 (* a2sgn pi2)) len4))

  (command "arc" p1 p4 p5)
  (setq arc1 (entlast)                                 ;첫호의 entity name
        cen1 (cdr (assoc 10 (entget  arc1)))           ;첫호의 중심점
        cen11 (polar cen1 (+ ang1 (* a2sgn pi2)) len4) ;첫호 trim line의 center
        r (cdr (assoc 40 (entget (entlast))))          ;반지름
        sang1 (angle cen1 p1))                         ;시작각도
  
  (command "arc" p5 p6 p2)
  (setq arc2 (entlast)                                 ;둘째호
        cen2 (cdr (assoc 10 (entget  arc2)))
	cen22 (polar cen2 (+ ang1 (* a2sgn pi2)) len4)
	sang2 (angle cen2 p5))
  
;  (setq cen2 (cdr (assoc 10 (entget  (entlast)))))
  ;(command "pline" p1 "a" "s" p4 p5 "s" p6 p2 "")

;  (command "pline")
      
  (setq angarc (* 2 (asin (/ len 4.0 r))))              ;호의 각도
  
  (command "TRIM" arc1 arc2 "" "F")                     ;trim fance
;  (command (polar cen11 (+ sang1 (* a2sgn (/ angarc 12) index)) r))
  (setq index -1)  
  (repeat 7
    (command (polar cen11 (- sang1 (* a2sgn (/ angarc 6) index)) r))
    (setq index (1+ index))
  );repeat

  (setq index 1)                        ;호를 6등분 한 선들을 찍어줌
  (repeat 7
    (command (polar cen22 (+ sang2 (* a2sgn (/ angarc 6) index)) r))
    (setq index (1+ index))
  );repeat
  
  (command "" "")
  
  (command "arc" p2 p7 p5)
  
) ;of defun

;------------------------------------------
; function : asin
;            arc sin
;            Yi Suk Jong
;            2000/4/25
;------------------------------------------
; asin(a) : arcsin값 return(radian)
;------------------------------------------
(defun asin(a / a)
  (cond
    ((= a 1) (/ pi 2.0))
    ((= a -1) (/ pi -2.0))
    (T (atan (/ a (sqrt (- 1 (* a a))))))
  );cond  
);defun  
