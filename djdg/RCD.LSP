;************************************    
; Program : RCD    
;           RCD pile drawing    
;           Suk-Jong Yi    
;           1995. 3. 19    
;************************************    
    
(defun C:RCD(/                                              ;지역변수 정의    
ds oldclt oldclr s-pnt pd rdo pl tang ang tdsto e-pnt    
sl-pnt0 sr-pnt0 sl-pnt sr-pnt el-pnt er-pnt    
)    
    
  (setq ulbulh 100                              ;울퉁불퉁높이    
        ulbull 300)                             ;울퉁불퉁길이    
    
  (defun SETERR(s)    
    (if (/= s "Function cancelled")    
        (princ (strcat "\nError: " s))    
    ); of If    
    (setq *error* oer seterr nil)    
    (princ)    
  ); of SETERR    
    
  (setq oer *error* *error* seterr)    
    
  (push-env)                                                  ;환경변수 대피    
    
  (setq ds (getvar "DIMSCALE"))    
  (setq oldclt (getvar "CELTYPE"))                            ;현재 라인타입 대피    
  (setq oldclr (getvar "CECOLOR"))                            ;현재 칼라 대피    
    
  (setq s-pnt (getpoint "\nPick start point: "))              ;시작점 입력    
  (setq pd (getdist "\nPile diameter (mm): "))                ;파일 지름 입력    
  (setq rdo (/ pd 2.0))                                       ;파일 반지름 입력    
  (setq pl (getdist s-pnt "\nPile length (m): "))             ;파일 길이 입력    
  (setq pl (* pl 1000.0))                                     ;mm단위로 환산    
  (initget "Tilt Angle")    
  (setq tang (getreal "\nAngle/<1:?>: "))       ;1:?로 입력하려면 "T"를 입력    
  (if (= tang "Angle")    
    (progn    
      (setq tang (getreal "\nAngle (Degree): "))      ;각도 입력    
      (setq ang (+ (dtor tang) (* pi (/ 3.0 2.0))))   ;radian으로 치환    
      (setq pl  (/ pl (cos tang)))                    ;경사파일길이    
    ) ;of progn    
    (if (or (= tang 0) (= tang nil))    
      (setq ang (* pi (/ 3.0 2.0)))                          ;radian으로 치환    
      (setq ang (+ (* pi (/ 3.0 2.0)) (atan (/ 1.0 tang)))   ;기울기값을 각도로 환산    
            pl  (/ pl (cos (atan (/ 1.0 tang)))))            ;경사파일길이    
    ) ;of IF    
  ) ;of if    
    
  (setq tdsto (abs (/ rdo (sin ang))))        ;파일경사에 따른 파일 반경의 길이    
    
  (setq e-pnt (polar s-pnt ang pl))                   ;파일 중심 끝점    
    
  (setq sl-pnt0 (polar s-pnt (- ang (/ pi 2.0)) rdo)) ;파일의 외측 좌측(가상)시점    
  (setq sr-pnt0 (polar s-pnt (+ ang (/ pi 2.0)) rdo)) ;파일의 외측 우측(가상)시점    
    
  (setq sl-pnt (list (- (car s-pnt) tdsto) (cadr s-pnt) 0.0))    ;좌외측 시점    
  (setq sr-pnt (list (+ (car s-pnt) tdsto) (cadr s-pnt) 0.0))    ;우외측 시점    
    
  (setq el-pnt (polar sl-pnt0 ang pl))                   ;파일 좌외측 끝점    
  (setq er-pnt (polar sr-pnt0 ang pl))                   ;파일 우외측 끝점    
    
    
  (setvar "CELTYPE" "BYLAYER")            ;라인타입을 실선으로    
;  (command "LINE" sl-pnt el-pnt "")       ;왼쪽 바깥선과    
;  (command "LINE" sr-pnt er-pnt "")       ;오른쪽 바깥선 그리기    
;  (command "LINE" el-pnt er-pnt "")       ;끝단 그리기    
    
  ;울퉁불퉁 선 그리기    
  (pado el-pnt sl-pnt ulbulh ulbull)            ;울퉁불퉁 그리기(왼쪽)    
  (pado sr-pnt er-pnt ulbulh ulbull)            ;울퉁불퉁 그리기(오른쪽)    
  (pado er-pnt el-pnt ulbulh ulbull)            ;울퉁불퉁 그리기(바닥)    
    
  (setvar "CELTYPE" "CENTER")             ;라인 타입을 일점쇄선으로    
  (setvar "CECOLOR" "1")                ;빨간색으로 바꿔서    
  (command "LINE" s-pnt e-pnt "")         ;중심선 그리기    
    
  (setvar "CECOLOR" "3")              ;색깔은 초록색    
  (setvar "CELTYPE" "BYLAYER")            ;라인타입은 실선    
;  (hround s-pnt e-pnt sl-pnt el-pnt)      ;왼쪽 라운딩 그리기    
;  (hround s-pnt e-pnt sr-pnt er-pnt)      ;오른쪽 라운딩 그리기    
    
  (setvar "CECOLOR" oldclr)               ;원래색깔로 복귀시키기    
  (setvar "CELTYPE" oldclt)               ;원래 라인타입으로 복귀시키기    
    
  (pop-env)                               ;대피시켜둔 환경변수 복귀    
;  (setq *error* oer seterr nil)    
  (princ)    
) ;of defun    
    
    
;**************************************    
; Function : HROUND    
;            Half ROUNDing    
;            Suk-Jong Yi    
;            1995. 3. 16    
;**************************************    
    
(defun HROUND(p1s p1e p2s p2e /       ;반 라운딩을 해주는 Function    
sdst edst lpnts lpnte ds p1s p1e      ;Argument는 두선의 시작점과 끝점    
oldclr    
)    
    
(setq ds (getvar "DIMSCALE"))         ;DIMSCALE을 알아낸다.    
    
(setq ssdst (distance p1s p2s))     ;첫라인의 시작점에서 두째라인의 시작점거리    
(setq sedst (distance p1s p2e))     ;첫라인의 시작점에서 두째라인의 끝점거리    
(if (< sedst ssdst)                 ;위의 두 거리중 가까운쪽이 시작점이 된다    
  (progn    
    (setq tmp p2s)    
    (setq p2s p2e)    
    (setq p2e tmp)    
  ) ;of progn    
) ; of if    
    
(setq sdst (distance p1s p2s))    ;첫째라인 시작점에서 둘째라인 시작점까지 거리    
(setq edst (distance p1e p2e))    ;첫째라인 끝점에서 둘째라인 끝점까지 거리    
    
             ;두 선간의 거리가 2미리이상 일때만 두 선의 중간에다 선을 그린다    
(setq oldclr (getvar "CECOLOR"))    
(setvar "CECOLOR" "3")    
(while (or (> sdst (* ds 1.0)) (> edst (* ds 1.0)))    
  (setq lpnts (list (/ (+ (car p1s) (car p2s)) 2.0)      ;두 시작점의 중간점 X    
                    (/ (+ (cadr p1s) (cadr p2s)) 2.0)))  ;                   Y    
  (setq lpnte (list (/ (+ (car p1e) (car p2e)) 2.0)      ;두 끝점의 중간점   X    
                    (/ (+ (cadr p1e) (cadr p2e)) 2.0)))  ;                   Y    
  (command "LINE" lpnts lpnte "")     ; 중간선을 그린다    
  (setq p1s lpnts)             ;중간선의 시작점을 첫째선의 시작점으로 바꾼다    
  (setq p1e lpnte)             ;중간선의 끝점을 첫째선의 끝점으로 바꾼다    
  (setq sdst (distance lpnts p2s))  ;두 시작점간의 거리를 구한다    
  (setq edst (distance lpnte p2e))  ;두 끝점간의 거리를 구한다    
) ;of while    
(setvar "CECOLOR" oldclr)    
) ;of defun HROUND    
    
;***************************************    
; Function : PADO    
;            물결무늬그려주기    
;            이석종    
;            Yi Suk Jong    
;***************************************    
; 넘어오는 값    
;  p1,p2      :  시작점 끝점    
;  h, l       : 파도 높이, 길이, 반지름    
;***************************************    
;    
    
(defun pado(p1 p2 h l    
/    
 )    
 (setq leng (distance p1 p2))                   ;시작점에서 끝점까지 거리    
 (setq ang (angle p1 p2))                       ;시작점에서 끝점까지 각도    
 (setq n (fix (/ leng l)))                      ;파도 갯수    
 (setq newl (/ leng n))                         ;최종 파도의 길이    
 (setq ang_p90 (+ ang (* pi 0.5)))               ;각도+90도    
    
 ;첫번째 파도 그리기 및 정보 얻어내기    
 (setq pnt1 (polar (polar p1 ang (* newl 0.5)) ang_p90 h)    
       pnt2 (polar p1 ang newl)    
       pnt3 (polar (polar p1 ang (* newl 1.5)) ang_p90 h))    
    
    
; (command "ARC" pnt1 pnt2 pnt3)    
; (setq r (cdr (assoc 40 (entget (entlast)))))    ;호의 반지름 구하기    
 (setq alpha (atan (/ (* 0.5 l) h))    
       r (* (sin (- pi (* 2 alpha)))              ;호의 반지름 구하기    
            (sqrt (+ (* h h) (* l l 0.25)))))    
    
(command "PLINE" p1 "A" "R" r pnt1)    
    
 ;첫 반호 그리기    
; (command "ARC" p1 "e" pnt1 "r" r)    
    
 (setq count 1)       ;두번째 파도부터    
 (repeat (- n 1)    
   (setq pnt1 (polar (polar p1 ang (* newl (- count 0.5))) ang_p90 h)    
         pnt2 (polar p1 ang (* newl count ))    
         pnt3 (polar (polar p1 ang (* newl (+ count 0.5))) ang_p90 h))    
    
;   (command "ARC" pnt1 pnt2 pnt3)    
   (command "s" pnt2 pnt3)    
    
   (setq count (1+ count))                      ;다음 파도로    
    
 );repeat    
    
 ;마지막 반호 그리기    
; (command "ARC" pnt3 "e" p2 "r" r)    
 (command "r" r p2 "")    
    
    
);defun    
    
;(defun c:pado()    
;  (pado (getpoint "p1") (getpoint "p2") (getdist "d1" ) (getdist "d2"))    
;);defun    
