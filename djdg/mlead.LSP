;******************************
; Program : MLEAD
;           Multi LEAD
;           Yi Suk-jong
;           1995/12/7
;******************************

(defun C:MLEAD()

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (setq ds (getvar "DIMSCALE"))
  (setq blk (strcat (prefix) "blocks/arw1"))

  (princ "\nSelect LINES: ")
  (setq ss (ssget))                               ;line선택
  (setq spnt (getpoint "\nPick First Point: "))   ;시작점
  (setq epnt (getpoint spnt "\nPick End Point: "))     ;끝점
  (setq ang (/ (* (angle spnt epnt) 180) pi))          ;leader선의 각도
  (setq wh4 (which4 (angle spnt epnt)))
  (if (or (= wh4 1) (= wh4 4))                 ;leader선각이 몇사분면에
    (setq ysym ds)
    (setq ysym (* -1 ds))
  ) ;of if

  (setq nss (sslength ss))                        ;선택된 line의 갯수

  (setq index 0)

  (repeat nss
    (setq ent (entget (ssname ss index)))
    (setq entype (cdr (assoc 0 ent)))
    (cond
      ((= entype "LINE")
         (setq sp (cdr (assoc 10 ent)))
         (setq ep (cdr (assoc 11 ent)))
         (setq crspnt (inters spnt epnt sp ep))
      ) ;of cond LINE
      ((or (= entype "CIRCLE") (= entype "ARC"))
         (setq crspnt (cross ent spnt epnt))
      ) ;of cond CIRCLE
     ) ;of cond

     (command "INSERT" blk crspnt ds ysym ang)
     (setq index (1+ index))
   ) ;of repeat
   (setq oldc (getvar "CECOLOR"))
   (setvar "CECOLOR" "1")
   (command "LINE" spnt epnt "")
   (setvar "CECOLOR" oldc)
  (setq *error* oer seterr nil)
   (princ)
) ;of defun


;****************************************
; Function : CROSS
;            CROSS point of arc & line
;            By Suk-Jong Yi
;            1995/6/26
;****************************************

;함수: 호와 직선의 교차점 찾기
;     인수: ARC entity list, 직선의 첫점 , 직선의 끝점

(defun CROSS(aent sp ep /
aent sp ep
a b r sa ea x1 x2 y1 y2 c d
a1 b1 c1 x1 x2 y1 y2 ang1 ang2
)

(push-env)
(setq a (car (cdr (assoc 10 aent))))      ; ARC entity의 중심점 x좌표
(setq b (cadr (cdr (assoc 10 aent))))     ; ARC entity의 중심점 y좌표
(setq r (cdr (assoc 40 aent)))            ; ARC entity의 반지름
(setq sa (cdr (assoc 50 aent)))           ; ARC entity의 시작 각도
(setq ea (cdr (assoc 51 aent)))           ; ARC entity의 끝 각도

(setq x1 (car sp))                        ; LINE entity의 시작점 x좌표
(setq x2 (car ep))                        ; LINE entity의 끝점 x좌표
(setq y1 (cadr sp))                       ; LINE entity의 시작점 y좌표
(setq y2 (cadr ep))                       ; LINE entity의 끝점 y좌표
(setq c (/ (- y1 y2) (- x1 x2)))          ; y=cx+d에서 c
(setq d (- y2 (* c x2)))                  ; y=cx+d에서 d

(setq a1 (+ 1 (* c c)))                   ; x에 대한 이차방정식의 a
(setq b1 (+ (* 2 d c) (* -2 a) (* -2 b c)))   ;x에 대한 이차방정식의 b
(setq c1 (+ (* a a) (* b b) (* d d) (* -2 b d) (* -1 r r)))  ;이차 방정식의 c
(setq x1 (/ (+ (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1)))  ;근 1
(setq x2 (/ (- (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1)))  ;근 2
(setq y1 (+ (* c x1) d))                  ;근 1일 때 y값
(setq y2 (+ (* c x2) d))                  ;근 2일 때 y값

(setq ang1 (angle (list a b 0.0) (list x1 y1 0.0)))   ;교점1의 절대각(원점에서)
(setq ang2 (angle (list a b 0.0) (list x2 y2 0.0)))   ;교점2의 절대각(원점에서)

(if (inang sa ea ang1)
  (list x1 y1 0.0)         ;교점1이 호의 시작각과 끝각 상이에 있으면 교점 돌려줌
  (if (inang sa ea ang2)   ;교점2가 호의 시작각과 끝각 상이에 있으면 교점 돌려줌
    (list x2 y2 0.0)
    nil                    ;교점 1과 2가 모두 각 범위를 벗어날 경우 nil돌려줌
  ) ;of if
) ;of if
)

;************************************************************
; Function : INANG
;            a angle is IN the range of ANGle-1 and angle-2 ?
;            By Suk-Jong Yi
;            1995/6/26
;*************************************************************

;어떤 각이 주어진 두각(ang1, ang2) 사이에 있는가?
; 두각 사이에 있는 경우 두각의 차이를 돌려주고
; 두각 사이에 없는 경우는 nil을 돌려준다.

(defun inang(a1 a2 a3 /             ;인수 정의
a1 a2 a3                            ;지역변수 정의
)
(if (> a1 a2) (setq a2 (+ (* 2.0 pi) a2)))   ;첫각이 두번째 각보다 크면 +360도
(if (and (>= a3 a1) (<= a3 a2)) (- a2 a1)    ;주어진 각이 두각사이에 있으면
                                nil)         ; 두각의 차이를 돌려줌
)                                            ; 두각 사이에 없으면 nil돌려줌


