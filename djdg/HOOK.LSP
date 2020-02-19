;***************************************
; Program ; HOOK
;           HOOK rebar
;           Suk-Jong Yi
;           96/5/23
;***************************************
; 갈고리 철근을 그려준다.(90도, 180도)
;***************************************

(defun C:HOOK(/)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)                                                    ;환경변수 대피

  (setq r (getreal "\nRadius: "))                               ;반지름
  (setq l (getreal "\nLength: "))                               ;갈고리길이

  (initget "90 180")
  (setq ans (getkword "\n90 180 <90>: "))                       ;90도 180도 갈고리선택
  (if (= ans nil) (setq ans 90))                                ;return입력시 90도로

  (setq lent (entget (car (entsel "\nSelect LINE: "))))         ;line선택
  (setq spnt (cdr (assoc 10 lent))
        epnt (cdr (assoc 11 lent))                              ;시작점과 끝점
        newc (cdr (assoc 62 lent)))                             ;시작점과 끝점

;  (setq angse (angle spnt epnt))                                ;시작점과 끝점이 이루는 각

  (setq pnt (getpoint "\Pick end point: "))                     ;갈고리 끝점 입력

  (setq dstsp (distance spnt pnt)                               ;시작점에서 거리
        dstep (distance epnt pnt)                               ;끝점에서 거리
        dstse (distance spnt epnt))                             ;시작점에서 끝점까지 거리

  (if (<= dstsp dstep)
    (setq p1 spnt                                               ;가까운쪽 점을 p1
          p2 epnt)                                              ;먼쪽 점을 p2
    (setq p1 epnt
          p2 spnt))

  (setq ang (angle p2 p1))                                      ;시작점~끝점 각
  (setq tmp (polar pnt (+ (/ pi 2.0) ang) 1))                ;라인과 끝점의 교차점 찾기 위한 임시 점
  (setq pnt (inters p2 p1 tmp pnt nil))                     ;갈고리 끝점과 라인과의 교차점

  (setq sdpnt (getpoint "\nPick side point: "))                 ;갈고리 방향
  (setq tmp (polar sdpnt (+ (/ pi 2.0) ang) 1))                 ;라인과 side점의 교차점 찾기 위한 점
  (setq insp (inters p2 p1 tmp sdpnt nil))                      ;side point와 라인과의 교차점
  (setq ang1 (angle insp sdpnt))                                ;교차점과 side point가 이루는 각
  (setq epnt1 (polar p2 ang (- (distance p2 pnt) r)))           ;호의 시작점
  (setq cen (polar epnt1 ang1 r))                               ;갈고리 원의 중심
;  (if (= ans 90)
;    (command "ARC" "C" cen epnt1 "A" (rtod (dang ang ang1)))        ;호 그리기
;    (command "ARC" "C" cen epnt1 "A" (rtod (* 2 (dang ang ang1))))  ;호 그리기
;  ) ;of if

  (setq lent (subst (cons 10 p2) (assoc 10 lent) lent))         ;시작점 line정보
  (setq lent (subst (cons 11 epnt1) (assoc 11 lent) lent))      ;끝점 정보 바꾸기
  (entmod lent)                                                 ;line바꾸기

  (if (= ans 90)
    (setq lpnt1 (polar pnt ang1 r)
          lpnt2 (polar lpnt1 ang1 l))                           ;90도 갈고리 직선부
    (setq lpnt1 (polar epnt1 ang1 (* 2 r))                      ;180도 갈고리 직선시점
          lpnt2 (polar lpnt1 (- ang pi) l))                     ;180도 갈고리 직선종점
  ) ;of if

  (setq oldc (getvar "CECOLOR"))
  (command "COLOR" newc)

  (if (= ans 90)
    (command "ARC" "C" cen epnt1 "A" (rtod (dang ang ang1)))        ;호 그리기
    (command "ARC" "C" cen epnt1 "A" (rtod (* 2 (dang ang ang1))))  ;호 그리기
  ) ;of if
  (command "LINE" lpnt1 lpnt2 "")                               ;line그리기

  (setvar "CECOLOR" oldc)

  (pop-env)                                                     ;환경변수 복귀
  (setq *error* oer seterr nil)
  (princ)

);of defun
