;**************************************
; Program : PILE
;           PILE drawing
;           Suk-Jong Yi
;           1995. 3. 18
;**************************************

(defun C:PILE(/
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
(push-env)                                      ;환경변수 대피

(setq oldclt (getvar "CELTYPE"))

(setq ds (getvar "DIMSCALE"))

(setq s-pnt (getpoint "\nPick start point: "))   ;지작점 입력
(setq pd (getdist "\nPile diameter: "))         ;파일 지름 (외측)
(setq rdo (/ pd 2.0))                            ;파일 반지름(외측)
(setq pt (getdist "\Pile thickness: "))         ;파일 두께
(setq rdi (- (/ pd 2.0) pt))                     ;파일 반지름(외측)
(setq pl (getdist s-pnt "\nPile length: "))
(initget "Tilt Angle")
(setq tang (getreal "\nTilt/<Angle>: "))       ;1:?로 입력하려면 "T"를 입력
(if (= tang "Tilt")
  (progn
    (setq tang (getreal "\nTilt ratio(Holizontal:Vertival=1:?): "))
    (setq ang (+ (* pi (/ 3.0 2.0)) (atan (/ 1.0 tang))))   ;기울기값을 각도로 환산
  ) ;of progn
  (setq ang (+ tang (* pi (/ 3.0 2.0))))
) ;of if

(setq tdsto (abs (/ rdo (sin ang))))        ;파일경사에 따른 파일 반경의 길이
(setq tdsti (abs (/ rdi (sin ang))))

(setq e-pnt (polar s-pnt ang pl))                   ;파일 중심 끝점

(setq sl-pnt0o (polar s-pnt (- ang (/ pi 2.0)) rdo)) ;파일의 외측 좌측(가상)시점
(setq sr-pnt0o (polar s-pnt (+ ang (/ pi 2.0)) rdo)) ;파일의 외측 우측(가상)시점
(setq sl-pnt0i (polar s-pnt (- ang (/ pi 2.0)) rdi)) ;파일의 내측 좌측(가상)시점
(setq sr-pnt0i (polar s-pnt (+ ang (/ pi 2.0)) rdi)) ;파일의 내측 우측(가상)시점

(setq sl-pnto (list (- (car s-pnt) tdsto) (cadr s-pnt) 0.0))    ;좌외측 시점
(setq sr-pnto (list (+ (car s-pnt) tdsto) (cadr s-pnt) 0.0))    ;우외측 시점
(setq sl-pnti (list (- (car s-pnt) tdsti) (cadr s-pnt) 0.0))    ;좌내측 시점
(setq sr-pnti (list (+ (car s-pnt) tdsti) (cadr s-pnt) 0.0))    ;우내측 시점

(setq el-pnto (polar sl-pnt0o ang pl))                   ;파일 좌외측 끝점
(setq er-pnto (polar sr-pnt0o ang pl))                   ;파일 우외측 끝점
(setq el-pnti (polar sl-pnt0i ang (- pl (* rdo 0.25))))     ;파일 좌내측 끝점
(setq er-pnti (polar sr-pnt0i ang (- pl (* rdo 0.25))))     ;파일 우내측 끝점

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
(setvar "CELTYPE" "HIDDEN")           ;라인 타입을 점선으로
(command "LINE" sl-pnti el-pnti "")   ;왼쪽 안선과 오른쪽 안선 그리기
(setq lient (list (entlast) el-pnti))
(command "LINE" sr-pnti er-pnti "")
(setq rient (list (entlast) er-pnti))
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

(command "EXTEND" plent "" lient rient "")

(pop-env)                                       ;환경변수 복귀
  (setq *error* oer seterr nil)

) ;of defun
