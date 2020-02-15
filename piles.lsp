;*************************
; Program : PILES
;           PILES
;           Suk-Jong Yi
;           04/05/03
;*************************
; 말뚝군을 그려준다.(using dialog Box)
;*************************

(defun C:PILES(
/ ipnt d l n p count na a yscl)

  (defun SETERR(s)                          ;내장에러루틴 정의
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)         ;내장에러루틴 가동

  (push-env)                                                ;환경변수 대피

  (initget "2point")
    (setq sel (entsel "\nSelect footing line or [2point]: "))
    (if (= sel "2point")
      (progn
        (setq p1 (getpoint "\nPick start point: ")
	      p2 (getpoint p1 "\nPick end point: "))
      );if true
      (progn
        (setq ente (entget (car sel)))   ;edge selection
        (setq p1 (cdr (assoc 10 ente))
              p2 (cdr (assoc 11 ente)))
      );;if false
    );if  

  (setq width (abs (* (- (car p2) (car p1)) 0.001)))

  (piles_dia)
  
  (setq ipnt (mid-point p1 p2))
  (setq d (atoi dia))                     ;파일 지름 (mm)
  (setq l (getreal "\nPile Length(m): "))                   ;파일길이  (m)
  (setq n (1+ (atoi num)))                    ;파일 갯수
  (setq p (atof pitch))                    ;파일 간격

  (setq count 1
        na nil)                                             ;slop list

  (repeat n                                                 ;pile갯수만큼 반복
;    (princ "\nSlop of Pile ") (princ count) (princ "/") (princ n)
;    (setq a (getreal "<0>: "))
    (setq a 0)    
;    (if (= a nil) (setq a 0))
    (setq na (append na (list a)))
    (setq count (1+ count))
  ) ;of defun

;  (setq yscl (getreal "\nY-scal<1.0>: "))                   ;y-scale (종평면도)
  (if (= yscl nil) (setq yscl 1))

  (f_spiles ipnt  d  l  na  p yscl)                         ;파일군 그리기 함수호출

  (pop-env)                                             ;환경변수 복귀
  (setq *error* oer seterr nil)

) ;of defun

;***************************************
; Function : F_SPILES
;            Function Steel PILES
;            Suk-Jong Yi
;            96/7/19
;***************************************
; 파일군 그리기 (steel pile)
; 넘어오는 값
;   IP : Insert Point
;    D : Dia             (mm)
;    L : Length          (m)
;   NA : Nunber & Angle
;    P : Pitch           (m)
; YSCL : Y-SCaLe         (default=1.0)
;***************************************
(defun F_SPILES(IP D L NA P YSCL
/  p n l ix iy ix1 count pnt)

  (setq p (* p 1000)                                ;mm단위로 환산
        n  (length na)                              ;pile갯수
        l (* l yscl)                                ;pile길이 수직방향 환산
        ix (car ip)
        iy (cadr ip)
        ix1 (- ix (/ (* (1- n) p) 2))               ;첫 파일 시작점 x
        count 0)


  (repeat n                                         ;파일들 그리기
    (setq pnt (list (+ ix1 (* count p)) iy))
    (f_spile pnt d l (* (nth count na) yscl))       ;파일 그리기 함수 호출
    (setq count (1+ count))
  ) ;of repeat
) ;of defun

;(defun F_RCDS(
;/)
;
;)

;***************************************
; Function : F_SPILE
;           Steel PILE
;           Suk-Jong Yi
;           96/7/18
;***************************************
; 강관파일을 그려준다. (외면을 표현)
; IP : Insert Point
; D  : DIA    (mm)
; L  : Length (m)
; S  : Slop
;***************************************
(defun F_SPILE(IP D L S
/   oldclt ds s-pnt pd rdo pt rdi pl tang ang tdsto tdsti e-pnt
    sl-ont0o sr-pnt0o sl-pnt0i sr-pnt0i sl-pnto sr-pnto sl-pnti sr-pnti
    el-pnto dr-pnto el-pnti er-pnti cl-pnt cr-pnt ll-pnt1 ll-pnt2 rl-pnt
    lient rient cclr plent
)

  (if (= (tblsearch "LTYPE" "CENTER") nil)         ;쟢랭 Hidden type 댜톋죇 load
    (command "LINETYPE" "L" "CENTER" "ACAD" "")
  ) ;of if

  (setq oldclt (getvar "CELTYPE"))

  (setq ds (getvar "DIMSCALE"))

  (setq s-pnt IP)                                       ;지작점 입력
  (setq pd D)                                           ;파일 지름 (외측)
  (setq rdo (/ pd 2.0))                                 ;파일 반지름(외측)
  (setq pl L)                                           ;파일 길이
  (setq pl (* pl 1000))                                 ;mm 단위로 환산
  (setq tang S)

  (if (= tang 0)                                            ;각이 0인 경우
    (setq ang (+ (dtor tang) (* pi (/ 3.0 2.0))))           ;radian으로 치환
    (setq ang (+ (* pi (/ 3.0 2.0)) (atan (/ 1.0 tang))))   ;기울기값을 각도로 환산
  ) ;of IF

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
  (setvar "CECOLOR" "RED")
  (setvar "CELTYPE" "CENTER")           ;라인 타입을 일점쇄선으로
  (command "LINE" s-pnt e-pnt "")       ;중심선 그리기
  (setvar "CECOLOR" cclr)

  (setvar "CELTYPE" oldclt)             ;원래라인 타입으로 복귀시키기

  (command "PLINE" er-pnto "A" "S" rl-pnt e-pnt       ;파일끝단의 절단표시
                               "S" ll-pnt1 el-pnto    ;폴리라인의 아크기능사용
                               "S" ll-pnt2 e-pnt "")
  (setq plent (list (entlast) ll-pnt1))

  (setvar "CECOLOR" "GREEN")                            ;색을 녹색으로
  (setq rr (/ rdo 2.0))                                 ;첫 Round Radious
  (while (>= rr (* 0.25 DS))                            ;최외측 round가 0.25보다 큰 동안만
    (setq tdsti (abs (/ (- rdo rr) (sin ang))))                 ;파일경사에 따라 변하는 파일 반지름
    (setq sl-pnt0i (polar s-pnt (- ang (/ pi 2.0)) (- rdo rr))) ;라운드 좌측(가상)시점
    (setq sr-pnt0i (polar s-pnt (+ ang (/ pi 2.0)) (- rdo rr))) ;라운드 우측(가상)시점
    (setq sl-pnti (list (- (car s-pnt) tdsti) (cadr s-pnt) 0.0))    ;라운드 좌측 시점
    (setq sr-pnti (list (+ (car s-pnt) tdsti) (cadr s-pnt) 0.0))    ;라운드 우측 시점
    (setq el-pnti (polar sl-pnt0i ang (- pl (* rdo 0.3))))     ;라운드 좌측 끝점
    (setq er-pnti (polar sr-pnt0i ang (- pl (* rdo 0.3))))     ;라운드 우측 끝점

    (command "LINE" sl-pnti el-pnti "")                 ;왼쪽 안선과 오른쪽 안선 그리기
    (setq lient (list (entlast) el-pnti))
    (command "LINE" sr-pnti er-pnti "")
    (setq rient (list (entlast) er-pnti))
    (command "EXTEND" plent "" lient rient "")
    (setq rr (/ rr 2.0))                                ;라운드 거리를 반으로
  ) ;of WHILE

  (setvar "CECOLOR" "WHITE")                            ;색을 흰색으로

) ;of defun

;-----------------------------------------
; function : piles_DIA
;            get data using dialog box
;            Yi Suk Jong
;            04/05/03
;-----------------------------------------
(defun PILES_DIA (/
        dcl_id
  )

  (defun setdia( / )
    (setq dia (get_tile "dia"))
    (setq minpitch (strcat "m  Min: " (rtos (* 2.5 (atof dia) 0.001) 2 3) " m"))    
    (setq minedge (strcat "m  Min: " (rtos (* 1.25 (atof dia) 0.001) 2 3) " m"))
    (set_tile "pitchtxt"  minpitch)
    (set_tile "edgetxt"  minedge)
  );defun setdia

  (defun pile_check( / )
    (setq num (get_tile "num")
	  pitch (get_tile "pitch")
	  edge (get_tile "edge"))
    (setq w (+ (* (atof num) (atof pitch)) (* 2.0 (atof edge))))
    (setq widthtxt (rtos width 2 3)
	  wtxt (rtos w 2 3))
    (setq resulttxt (strcat wtxt (if (< w width) "<" (if (> w width) ">" "=")) widthtxt))
    (set_tile "result" (strcat "Width: " resulttxt))
    (if (/= w width) (alert resulttxt))
  );defun pile_check
  
  (setq dcl_id (load_dialog "DJDG.DCL"))                  ;dialog호출
  (if (not (new_dialog "PILES" dcl_id)) (exit))


;-------------------
; 초기값설정
;-------------------

  (if (= dia nil) (setq dia "508"))
  (setq minpitch (strcat "m  Min: " (rtos (* 2.5 (atof dia) 0.001) 2 3) " m"))
  (setq minedge (strcat "m  Min: " (rtos (* 1.25 (atof dia) 0.001) 2 3) " m"))
  (if (= scthick nil) (setq scthick "12"))
  (if (= sctype nil) (setq sctype 1))
  (if (= #piletype nil) (setq #piletype 1))
    
;---------------------------
; dialog box 초기화
;---------------------------
  (set_tile "widthf" (strcat "Width of Footing: " (rtos width 2 3) "m"))
  (set_tile "dia" dia)
  (set_tile "pitchtxt"  minpitch)
  (set_tile "edgetxt"  minedge)
  (if (= #piletype 1)
    (set_tile "steeltype" "1")	
    (set_tile "conctype" "1")
  );if
  
  (action_tile "steeltype" "(setq #piletype 1)")   ;steel pile
  (action_tile "conctype" "(setq #piletype 2)")    ;conc pile 
  (action_tile "dia" "(setdia)")        
  (action_tile "num" "(setq num $VALUE)")      
  (action_tile "pitch" "(setq pitch $VALUE)")
  (action_tile "edge"   "(setq edge $VALUE)")

  (action_tile "check" "(pile_check)")
  
  (action_tile "accept"  "(done_dialog)")
  (action_tile "cancel"  "(exit)")
    
;   (mode_tile "dia" 2)

  (start_dialog)

  (unload_dialog dcl_id)

) ;of defun SPLICE_DIALOG
