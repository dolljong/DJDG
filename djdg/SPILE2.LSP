;***************************************
; Program : SPILE2
;           Steel PILE - 2
;           Suk-Jong Yi
;           96/5/8
;***************************************
; 강관파일을 그려준다. (외면을 표현)
;***************************************
(defun C:SPILE2(/
    oldclt ds s-pnt pd rdo pt rdi pl tang ang tdsto tdsti e-pnt
    sl-ont0o sr-pnt0o sl-pnt0i sr-pnt0i sl-pnto sr-pnto sl-pnti sr-pnti
    el-pnto dr-pnto el-pnti er-pnti cl-pnt cr-pnt ll-pnt1 ll-pnt2 rl-pnt
    lient rient cclr plent
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)                                          ;환경변수 대피

  (setq oldclt (getvar "CELTYPE"))

  (setq ds (getvar "DIMSCALE"))

  (setq s-pnt (getpoint "\nPick start point: "))      ;지작점 입력
  (setq pd (getdist "\nPile diameter (mm): "))             ;파일 지름 (외측)
  (setq rdo (/ pd 2.0))                               ;파일 반지름(외측)
;  (setq pt (getdist "\Pile thickness (mm): "))             ;파일 두께
;  (setq rdi (- (/ pd 2.0) pt))                        ;파일 반지름(외측)
  (setq pl (getdist s-pnt "\nPile length (m): "))     ;파일 길이
  (setq pl (* pl 1000))                               ;mm 단위로 환산
  (initget "Tilt Angle")
  (setq tang (getreal "\nAngle/<1:?>: "))       ;1:?로 입력하려면 "T"를 입력
  (if (= tang "Angle")
    (progn
      (setq tang (getreal "\nAngle (Degree): "))      ;각도 입력
      (setq ang (+ (dtor tang) (* pi (/ 3.0 2.0))))   ;radian으로 치환
      (setq pl (/ pl (cos tang)))                     ;경사파일길이
    ) ;of PROGN
    (if (= tang 0)
      (setq ang (* pi (/ 3.0 2.0)))                          ;radian으로 치환
      (setq ang (+ (* pi (/ 3.0 2.0)) (atan (/ 1.0 tang)))   ;기울기값을 각도로 환산
            pl  (/ pl (cos (atan (/ 1.0 tang)))))            ;경사파일길이
    ) ;of IF
  ) ;of if

  (setq tdsto (abs (/ rdo (sin ang))))        ;파일경사에 따른 파일 반경의 길이

  (setq e-pnt (polar s-pnt ang pl))                   ;파일 중심 끝점

  (setq sl-pnt0o (polar s-pnt (- ang (/ pi 2.0)) rdo)) ;파일의 외측 좌측(가상)시점
  (setq sr-pnt0o (polar s-pnt (+ ang (/ pi 2.0)) rdo)) ;파일의 외측 우측(가상)시점
  (setq sl-pnto (list (- (car s-pnt) tdsto) (cadr s-pnt) 0.0))    ;좌외측 시점
  (setq sr-pnto (list (+ (car s-pnt) tdsto) (cadr s-pnt) 0.0))    ;우외측 시점
  (setq el-pnto (polar sl-pnt0o ang pl))                   ;파일 좌외측 끝점
  (setq er-pnto (polar sr-pnt0o ang pl))                   ;파일 우외측 끝점

  (setq cl-pnt (list (/ (+ (car e-pnt) (car el-pnto)) 2.0)
                     (/ (+ (cadr e-pnt) (cadr el-pnto)) 2.0) 0.0))
  (setq cr-pnt (list (/ (+ (car e-pnt) (car er-pnto)) 2.0)
                     (/ (+ (cadr e-pnt) (cadr er-pnto)) 2.0) 0.0))

  (setq ll-pnt1 (polar cl-pnt (+ ang pi) (* rdo 0.25))) ;파일끝의 절단표시 그리기
  (setq ll-pnt2 (polar cl-pnt ang (* rdo 0.25)))        ;위한 점들 구하기
  (setq rl-pnt (polar cr-pnt ang (* rdo 0.25)))

  (setvar "CELTYPE" "BYLAYER")          ;라인타입을 실선으로
  (command "LINE" sl-pnto el-pnto "")   ;왼쪽 바깥선과 오른쪽 바깥선 그리기
  (command "LINE" sr-pnto er-pnto "")
  (setq cclr (getvar "CECOLOR"))
  (setvar "CECOLOR" "1")
  (setvar "CELTYPE" "CENTER")           ;라인 타입을 일점쇄선으로
  (command "LINE" s-pnt e-pnt "")       ;중심선 그리기
  (setvar "CECOLOR" cclr)

  (setvar "CELTYPE" oldclt)             ;원래라인 타입으로 복귀시키기

  (command "PLINE" er-pnto "A" "S" rl-pnt e-pnt       ;파일끝단의 절단표시
                               "S" ll-pnt1 el-pnto    ;폴리라인의 아크기능사용
                               "S" ll-pnt2 e-pnt "")
  (setq plent (list (entlast) ll-pnt1))

  (setvar "CECOLOR" "3")                            ;색을 녹색으로
  (setq rr (/ rdo 2.0))                                 ;첫 Round Radious
  (while (>= rr (* 0.25 DS))                            ;최외측 round가 0.25보다 큰 동안만
    (setq tdsti (abs (/ (- rdo rr) (sin ang))))                 ;파일경사에 따라 변하는 파일 반지름
    (setq sl-pnt0i (polar s-pnt (- ang (/ pi 2.0)) (- rdo rr))) ;라운드 좌측(가상)시점
    (setq sr-pnt0i (polar s-pnt (+ ang (/ pi 2.0)) (- rdo rr))) ;라운드 우측(가상)시점
    (setq sl-pnti (list (- (car s-pnt) tdsti) (cadr s-pnt) 0.0))    ;라운드 좌측 시점
    (setq sr-pnti (list (+ (car s-pnt) tdsti) (cadr s-pnt) 0.0))    ;라운드 우측 시점
    (setq el-pnti (polar sl-pnt0i ang (- pl (* rdo 0.3))))     ;라운드 좌측 끝점
    (setq er-pnti (polar sr-pnt0i ang (- pl (* rdo 0.3))))     ;라운드 우측 끝점

;    (setvar "CELTYPE" "HIDDEN")                        ;라인 타입을 점선으로
    (command "LINE" sl-pnti el-pnti "")                 ;왼쪽 안선과 오른쪽 안선 그리기
    (setq lient (list (entlast) el-pnti))
    (command "LINE" sr-pnti er-pnti "")
    (setq rient (list (entlast) er-pnti))
    (command "EXTEND" plent "" lient rient "")
    (setq rr (/ rr 2.0))                                ;라운드 거리를 반으로
  ) ;of WHILE

  (setvar "CECOLOR" "7")                            ;색을 흰색으로

  (pop-env)                                             ;환경변수 복귀

  (setq *error* oer seterr nil)
  (princ)

) ;of defun
