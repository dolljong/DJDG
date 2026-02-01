;**********************************
; Porgram : BRSE
;           BRidge Start End
;           Jong-Suk Yi
;           98/5/4
;**********************************

(defun C:BRSE(/
ds ip ph dh hs txt1 txt2 l1 l2 txtl1 txtl2 pnt1 pnt2 pnt3 mpnt
oldc txtgap txtang txth txtpnt1 txtpnt2
)

  (defun SETERR(s)                                  ;내장에러루틴 정의
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)                 ;내장에러루틴 가동

  (push-env)                                          ;현재 환경변수 대피

  (setq ds (getvar "DIMSCALE")                  ; dimscale
        th (getvar "DIMTXT"))                   ; text크기 = dimtxt


  (setq ip (getpoint "\nPick insert point: "))                ;삽입점 입력
  (setq ph (getpoint ip "\nPick Holizontal side: "))          ;수평방향 입력

  (setq dh (- (car ph) (car ip)))                             ;수평차
  (setq hs (/ (abs dh) dh))                                   ;수평부호

  (if (= hs -1)
    (progn
      (setq txt1 (getstring "\nEnter TEXT <교량시점>: "))      ;상단텍스트 입력
      (if (= txt1 "") (setq txt1 "교량시점"))
    ) ;of progn
    (progn
      (setq txt1 (getstring "\nEnter TEXT <교량종점>: "))      ;텍스트 입력
      (if (= txt1 "") (setq txt1 "교량종점"))
    ) ;of progn
  ) ;of if

  (setq txt2 (getstring "\nEnter STA.: "))                     ;하단텍스트입력
  (if (= txt2 "")
    (setq txt2 "")
    (setq txt2 (strcat "STA. " txt2))
  ) ;of if

  (setq l1 5                        ;   insert point~시작점
        l2 10)                      ;   시작점~꺽인점

  (setq l1 (* l1 ds)
        l2 (* l2 ds))               ; 스케일 조정

  (setq txtl1 (* th ds (+ (strlen txt1) 2)))  ;두글자를 더한 글자가 차지하는 길이
  (setq txtl2 (* th ds (+ (strlen txt2) 2)))  ;두글자를 더한 글자가 차지하는 길이

  (if (> txtl1 txtl2) (setq txtl txtl1) (setq txtl txtl2))     ;더큰쪽을 라인길이로

  (setq pnt1 (list (car ip) (+ (cadr ip) l1))                  ;시작점
        pnt2 (list (car ip) (+ (cadr pnt1) l2))                ;중간점
        pnt3 (polar pnt2 (+ (/ pi 2) (* (/ pi -3) hs)) txtl)   ;끝점
        mpnt (mid-point pnt2 pnt3))                            ;베이스라인 중간점

  (setq oldc (getvar "CECOLOR"))                               ;현재색 대피
  (setvar "CECOLOR" "1")                                     ;색을 빨간색으로
  (command "PLINE" pnt1 pnt2 pnt3 "")                          ;폴리라인 그리기
  (setvar "CECOLOR" oldc)                                      ;색깔을 옛색으로

  (setq txtgap (* 2 ds))                                       ;베이스라인~텍스트

  (setq txtang (/ pi 6 hs))                                    ;텍스트각도(rad)
  (setq txth (* th ds))                                       ;텍스트 높이
  (setq txtpnt1 (polar mpnt (+ txtang (/ pi 2)) txtgap)        ;상단텍스트 위치
        txtpnt2 (polar mpnt (- txtang (/ pi 2)) txtgap))       ;하단텍스트 위치

  (setq txtang (rtod txtang))                                  ;텍스트각도(deg)

  (command "TEXT" "J" "C" txtpnt1 txth txtang txt1)    ;텍스트 씀
  (command "TEXT" "J" "TC" txtpnt2 txth txtang txt2)    ;텍스트 씀

  (pop-env)                                             ;환경변수 복귀

  (setq *error* oer seterr nil)                         ;내장에러루틴 끝

(princ)

) ;of defun
