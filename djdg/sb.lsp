;****************************************************
; Program: SB.LSP
;          Split Bar
;          Yi Suk Jong
;          96/12/12
;****************************************************

(defun C:SB(/
)

  ;;
  ;; 내장 error routine
  ;;
  (defun SETERR(s)                                      ;내장 에러루틴 정의
  ;If an error (CTRL-C) occurs when this command is active.
    (if (/= s "Function cancelled")
      (if (= s "quit / exit abort")
        (princ "\n*cancel*")
        (princ (strcat "\nError " s))
      ) ;of if
    ); of If
  ;Restore previous error handler
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  ;;
  ;; Function: SB_DIALOG (Dialog box로 입력받기)
  ;;
  (defun SB_DIALOG (/
                       dcl_id
  )
    (setq dcl_id (load_dialog "DJDG"))                  ;dialog호출
    (if (not (new_dialog "SB" dcl_id)) (exit))

    (start_image "sb")                                  ;image 보이기
    (slide_image
                 0 0
                 (dimx_tile "sb") (dimy_tile "sb")
                 "djdg(ddsb)"
    )
    (end_image)

    (if (= #N1 nil) (setq #N1 13))                      ;초기치 설정
    (if (= #P1 nil) (setq #P1 100))
    (if (= #N2 nil) (setq #N2 13))
    (if (= #P2 nil) (setq #P2 100))
    (if (= #EX nil) (setq #EX  50))
    (if (= #M1 nil) (setq #M1 "S"))
    (if (= #D1 nil) (setq #D1 "D16"))
    (if (= #M2 nil) (setq #M2 "S"))
    (if (= #D2 nil) (setq #D2 "D16"))
    (if (= #SCALE nil) (setq #SCALE 20))


    (set_tile "n1" (rtos #N1 2 0))                  ;초기치로 edit box setting
    (set_tile "p1" (rtos #P1 2 0))
    (set_tile "n2" (rtos #N2 2 0))
    (set_tile "p2" (rtos #P2 2 0))
    (set_tile "ex" (rtos #EX 2 0))
    (set_tile "m1" #M1)
    (set_tile "d1" #D1)
    (set_tile "m2" #M2)
    (set_tile "d2" #D2)
    (set_tile "total1" (rtos (+ (* #EX 2) (* #N1 #P1)) 2 0))
    (set_tile "total2" (rtos (+ (* #EX 2) (* #N2 #P2)) 2 0))
    (set_tile "scale" (rtos #SCALE 2 0))

    (action_tile "n1" "(set_n1)")                   ;dialog box Action
    (action_tile "p1" "(set_p1)")
    (action_tile "n2" "(set_n2)")
    (action_tile "p2" "(set_p2)")
    (action_tile "ex" "(set_ex)")
    (action_tile "m1" "(set_m1)")
    (action_tile "d1" "(set_d1)")
    (action_tile "m2" "(set_m2)")
    (action_tile "d2" "(set_d2)")
    (action_tile "scale" "(set_scale)")
    (action_tile "accept" "(do_accept)")
    (action_tile "cancel" "(do_cancel)")
    (mode_tile "n1" 2)
    (start_dialog)
    (unload_dialog dcl_id)
  ) ;of defun SPLICE_DIALOG

  ;;
  ;; FUNCTION : SET_N1 (n1값 입력)
  ;;
  (defun SET_N1 (/ in value)              ;n1 edit_box에 입력이 들어왔을 때
    (setq in (get_tile "n1"))
    (setq value (atoi in))
    (if (= value 0)
      (progn
        (do_error "n1" in)
        nil
      ) ;of THEN
      (progn
        (setq #N1 value)
        (set_tile "error" "")
        (set_tile "total1" (rtos (+ (* #EX 2) (* #N1 #P1)) 2 0))
        T
      ) ;of ELSE
    ) ;of IF
   ) ;of defun

  ;;
  ;; FUNCTION : SET_P1 (p1값 입력)
  ;;
  (defun SET_P1(/ in value)              ;p1 edit_box에 입력이 들어왔을 때
    (setq in (get_tile "p1"))
    (setq value (atof in))
    (if (= value 0)
      (progn
        (do_error "p1" in)
        nil
      ) ;of THEN
      (progn
        (setq #P1 value)
        (set_tile "error" "")
        (set_tile "total1" (rtos (+ (* #EX 2) (* #n1 #p1)) 2 0))
        T
      ) ;of ELSE
    ) ;of IF
   ) ;of defun

  ;;
  ;; FUNCTION : SET_N2 (n2값 입력)
  ;;
  (defun SET_N2(/ in value)              ;n2 edit_box에 입력이 들어왔을 때
    (setq in (get_tile "n2"))
    (setq value (atoi in))
    (if (= value 0)
      (progn
        (do_error "n2" in)
        nil
      ) ;of THEN
      (progn
        (setq #N2 value)
        (set_tile "error" "")
        (set_tile "total2" (rtos (+ (* #N2 #P2) (* 2 #EX)) 2 0))
        T
      ) ;of ELSE
    ) ;of IF
   ) ;of defun

  ;;
  ;; FUNCTION : SET_P2 (P2값 입력)
  ;;
  (defun SET_P2(/ in vlaue)              ;p2 edit_box에 입력이 들어왔을 때
    (setq in (get_tile "p2"))
    (setq value (atof in))
    (if (= value 0)
      (progn
        (do_error "p2" in)
        nil
      ) ;of THEN
      (progn
        (setq #P2 value)
        (set_tile "error" "")
        (set_tile "total2" (rtos (+ (* #N2 #P2) (* 2 #EX)) 2 0))
        T
      ) ;of ELSE
    ) ;of IF
   ) ;of defun

  ;;
  ;; FUNCTION : SET_EX (ex값 입력)
  ;;
  (defun SET_EX(/ in value)              ;g  edit_box에 입력이 들어왔을 때
    (setq in (get_tile "ex"))
    (setq value (atoi in))
    (if (= value 0)
      (progn
        (do_error "ex" in)
        nil
      ) ;of THEN
      (progn
        (setq #EX value)
        (set_tile "error" "")
        (set_tile "total1" (rtos (+ (* #EX 2) (* #N1 #P1)) 2 0))
        (set_tile "total2" (rtos (+ (* #EX 2) (* #N2 #P2)) 2 0))
        T
      ) ;of ELSE
    ) ;of IF
   ) ;of defun

  ;;
  ;; FUNCTION : SET_SCALE (scale값 입력)
  ;;
  (defun SET_SCALE(/ in value)              ;scale edit_box에 입력이 들어왔을 때
    (setq in (get_tile "scale"))
    (setq value (atoi in))
    (if (= value 0)
      (progn
        (do_error "scale" in)
        nil
      ) ;of THEN
      (progn
        (setq #SCL value)
        (set_tile "error" "")
        T
      ) ;of ELSE
    ) ;of IF
   ) ;of defun

  ;;
  ;; FUNCTION : SET_M1 (Mark-1값 입력)
  ;;
  (defun SET_M1(/ in value)              ;m1 edit_box에 입력이 들어왔을 때
    (setq #M1 (get_tile "m1"))
    T
   ) ;of defun

  ;;
  ;; FUNCTION : SET_D1 (Dia-1값 입력)
  ;;
  (defun SET_D1(/ in value)              ;d1 edit_box에 입력이 들어왔을 때
    (setq #D1 (get_tile "d1"))
    T
   ) ;of defun

  ;;
  ;; FUNCTION : SET_M2 (Mark-2값 입력)
  ;;
  (defun SET_M2(/ in value)              ;m2 edit_box에 입력이 들어왔을 때
    (setq #M2 (get_tile "m2"))
    T
   ) ;of defun

  ;;
  ;; FUNCTION : SET_D2 (Dia-2값 입력)
  ;;
  (defun SET_D2(/ in value)              ;d2 edit_box에 입력이 들어왔을 때
    (setq #D2 (get_tile "d2"))
    T
   ) ;of defun

  ;;;
  ;;; ok버튼을 누렀을 때
  ;;;
  (defun do_accept()           ;dialog box를 끝내기 전에 모든 입력 데이타 확인
    (if (and (set_n1) (set_p1) (set_n2) (set_p2) (set_ex) (set_scale))
      (done_dialog)
    ) ;of IF
  ) ;of defun


  ;;;
  ;;; 잘못된 값이 입력?을 때
  ;;;
  (defun do_error(tile value)
    (set_tile "error" "Invalid input")            ;error massage창에 에러표시

    (cond
      ((or (= tile "n1") (= tile "n2"))
        (progn
          (set_tile  tile value)
          (mode_tile tile 2)
        )
      )
      ((or (= tile "p1") (= tile "p2") (= tile "ex"))
        (progn
          (set_tile tile value)
          (mode_tile tile 2)
        )
      )
    ) ;of COND
  )


  ;;;
  ;;; Cancel 버튼을 눌렀을 경우
  ;;;
  (defun do_cancel()
    (done_dialog)
    (exit)
  )

  ;;;
  ;;; MAIN ROUTINE
  ;;;

  (setq oer *error* *error* seterr)

  (push-env)                            ;환경변수 보관

  (setq ds (getvar "DIMSCALE"))         ;스케일값잡아내기

  (sb_dialog)                           ;dialog박스로 입력받기

  (setq SCL #SCL                         ;Split철근의 스케일
        EX  #EX                          ;빠진길이
        N1  #N1                          ;가로방향 갯수
        P1  #P1                          ;가로방향 간격
        N2  #N2                          ;세로방향 갯수
        P2  #P2)                         ;세로방향 간격


  (setq scl1 (/ ds scl)                 ; 두 scale의 비(그림확대)
        ex (* ex scl1)                  ; 환산길이
        p1 (* p1 scl1)                  ; 환산길이
        p2 (* p2 scl1))                 ; 환산길이

  (setq l1 (+ (* 2 ex) (* n1 p1))           ;가로길이
        l2 (+ (* 2 ex) (* n2 p2)))          ;세로길이

  (setq ip (getpoint "\nPick Insert Point"))  ;삽입점 (그림의 좌측상단)

  (setq ix (car ip)                         ;삽입점 x
        iy (cadr ip))                       ;삽입점 y

  ;;; 수직철근반복
  (setvar "CECOLOR" "YELLOW")
  (setq n 0)
  (repeat (1+ n1)
    (command "LINE" (list (+ ix ex (* n p1)) iy)    ;시작점(위)
                    (list (+ ix ex (* n p1)) (- iy (* 2 ex) (* n2 p2)))
                    "")
    (setq n (1+ n))                                 ;다음철근
  ) ;of repeat

  (if (= (rem n2 2) 0)
    (setq my (- iy (/ l2 2.0) (/ p2 2.0)))  ;수직철근 마킹보조선 y위치
    (setq my (- iy (/ l2 2.0)))
  ) ;of if

  (setq hgap (* ds (+ 7 (* 2.5 3) 5)))      ;철근과 마킹사이 수평간격
  (setq mp1 (list (- (+ ix l1) ex) my)      ;보조선 오른쪽
        mp2 (list (- ix hgap) my))          ;보조선 왼쪽(Marking위치)
  (setq ssv (ssget "F" (list mp1 mp2)))

  ;;; 수평철근반복
  (setq n 0)
  (repeat (1+ n2)
    (command "LINE" (list ix (- iy ex (* n p2))) ;시작점(위)
                    (list (+ ix (* 2 ex) (* n1 p1)) (- iy ex (* n p2)))
                    "")
    (setq n (1+ n))                         ;다음철근
  ) ;of repeat

  (setvar "CECOLOR" "BYLAYER")

  (if (= (rem n1 2) 0)
    (setq mx (+ ix (/ l1 2.0) (/ p1 2.0)))  ;수평철근 마킹보조선 x위치
    (setq mx (+ ix (/ l1 2.0)))
  ) ;of if

  (setq vgap (* ds 5))                      ;철근과 마킹사이 수직간격
  (setq mp3 (list mx (+ (- iy l2) ex))      ;보조선 아래쪽
        mp4 (list mx (+ iy vgap)))           ;보조선 위쪽(Marking위치)
  (setq ssh (ssget "F" (list mp3 mp4)))


  ;; 수평치수선 작업
  (setq scl2 (/ scl ds))                    ;두 scale의 비
  (setq hdim0 (list ix (- iy l2)))          ;수평치수선 첫점

  (setq pp (f_dh hdim0 ex 1 -1 scl2))
  (setq pp (f_dh pp p1 n1 -1 scl2))
  (setq pp (f_dh pp ex 1 -1 scl2))
  (setq pp (f_dh hdim0 l1 1 -2 scl2))

  ;수직치수선 작업
  (setq pp (f_dv (list (+ ix l1) (- iy l2)) ex 1 1 scl2))
  (setq pp (f_dv pp p2 n2 1 scl2))
  (setq pp (f_dv pp ex 1 1 scl2))
  (setq p0 (f_dv (list (+ ix l1) (- iy l2)) l2 1 2 scl2))

  ;수직철근 마킹
  (F_B5 ssv mp1 mp2 1 #M2 #D2)
  (F_B5 ssh mp3 mp4 1 #M1 #D1)

  (setq *error* oer seterr nil)

  (pop-env)                             ;

) ;of defun


;******************************************
; Program : F_DHS
;           Function Dimension Horizontal
;           Jong-Suk Yi
;           96/6/29, 12/12
;******************************************
; 수평치수선을 함수로 처리해준다. (Scale처리 가능)
; 넘어오는 변수
;        SP : 시작점
;       DST : 거리
;         N : 반복갯수
;        UD : Up/DOWN (절대값은 LEVEL)
;      TXT1 : 직접입력하고싶은 text
; 돌려주는 값 - 끝점 좌표
;******************************************

(defun F_DH(SP DST N UD TXT1
/  sp dst n ud txt1
   th       dim_gap     ds      sgn     dy      next    ep      dxy
   dx       txt         divl    divn    txtlen  dtxt1   dtxt2   dtxt1p
   dtxt2p
)

  (setq th 2.5
        dim_gap 10.0)                                       ;글자 크기 지정

  (setq ds (getvar "DIMSCALE"))                             ;scale factor

  (if (or (= (type txt1) (type 1)) (= (type txt1) (type 1.0)))  ;정수/실수이면
    (setq sc txt1)
    (setq  sc 1)
  ) ;of if                                            ;factor로

  (if (> ud 0)                                              ;위 아래
    (setq sgn 1)
    (setq sgn -1)
  ) ;of if

  (setq dy (* ds (+ 20 (* dim_gap (- (abs ud) 1)))))        ;치수선 위치 계산 (절대값)

  (setq next (* dst n))                                     ;시작점에서 끝점까지 거리

  (setq ep (list (+ (car sp) next) (cadr sp)))              ;ep 위치계산

  (setq dxy (list (car ep) (+ (cadr ep) (* dy sgn)) 0.0))  ;치수선 위치

  (setq dx (distance sp ep))                          ;거리 계산

  (if (< (* dx sc) 1000.0)
    (setq txt (rtos (* dx sc) 2 0))                          ;1000미만일 때
    (setq txt (rtos (* dx 0.001 sc) 2 3))                ;1000이상일 때
  ) ;of if(dx < 1000)

  (if (> n 1)                                           ;골뱅이 옵션일 경우
    (progn
      (setq divl dst)                                   ;나누는 길이 입력
      (setq divn (rtos n 2 0))                          ;나눈 갯수 계산
      (if (< divl 1000.)
        (setq divl (rtos (* divl sc) 2 0))                   ;1000미만일 때
        (setq divl (rtos (* 0.001 divl sc) 2 3))) ;of if ;1000이상일 때
      (setq txtlen (* (+ (strlen txt) (strlen divn) (strlen divl) 2) th ds  ;text전체길이
                   (cdr (assoc 41 (tblsearch "STYLE" (getvar "TEXTSTYLE"))))))
      (if (>= txtlen dx)                       ;치수보조선 내에 text 안들어가면
        (progn
          (setq dtxt1 (strcat divn "@" divl))       ;위 아래 두줄로 나눈다
          (setq dtxt2 (strcat "=" txt))
          (setq dtxt1p (mapcar '+ (mid-point sp ep)
                                  (list 0.0 (+ (* dy sgn) (* ds 2.5)) 0.0)))
          (setq dtxt2p (mapcar '+ (mid-point sp ep)
                                  (list 0.0 (- (* dy sgn) (* ds 2.5)) 0.0)))
          (command "TEXT" "M" dtxt1p (* th ds) "0" dtxt1)
          (command "TEXT" "M" dtxt2p (* th ds) "0" dtxt2)
          (command "DIM1" "HOR" sp ep dxy " ")               ;DIM명령 내림
        ) ;of progn THEN
        (progn                                 ;치수보조선 내에 text 들어가면
          (setq dtxt1 (strcat divn "@" divl "=" txt))
          (command "DIM1" "HOR" sp ep dxy dtxt1)               ;DIM명령 내림
        ) ;of progn ELSE
      ) ;of IF
    ) ;of progn THEN
    (progn
      (if (or (= txt1 nil)
              (or (= (type txt1) (type 1)) (= (type txt1) (type 1.0))))
        (setq txt1 txt))                  ;리턴입력시 옛 text를 씀
      (command "DIM1" "HOR" sp ep dxy txt1)             ;DIM명령 내림
    ) ;of progn ELSE
  ) ;of if

  ep
) ;defun


;*******************************************************
; Function : F_DVS
;           Fuction Dimension Vertical
;           Jong-Suk Yi
;           96/7/1, 96/12/12
;*******************************************************
; 수직치수선을 함수로 처리해준다. (Scale처리 가능)
; 넘어오는 변수
;        SP : 시작점
;       DST : 거리
;         N : 반복갯수
;        UD : Right/Left (절대값은 LEVEL)
;      TXT1 : 직접입력하고싶은 text
; 돌려주는 값 - 끝점 좌표
;******************************************************

(defun F_DV(SP DST N LR TXT1
/ sp dst n lr txt1
  th        dim_gap     ds      sgn     dx      next    ep
  dxy       dy          txt     divl    divn    txtlen  dtxt1
  dtxt2     dtxt1p      dtxt2p
)

  (setq th 2.5                                        ;text크기 = 2.5
        dim_gap 10.0)                                 ;치수선 간격
  (setq ds (getvar "DIMSCALE"))                       ;scale factor

  (if (or (= (type txt1) (type 1)) (= (type txt1) (type 1.0)))  ;정수/실수이면
    (setq sc txt1)
    (setq  sc 1)
  ) ;of if                                            ;factor로

  (if (> lr 0)                                        ;왼쪽/오른쪽
    (setq sgn 1)
    (setq sgn -1)
  ) ;of if

  (setq dx (* ds (+ 20 (* dim_gap (- (abs lr) 1)))))

  (setq next (* dst n))                                 ;끝점까지 거리

  (setq ep (list (car sp) (+ (cadr sp) next)))          ;수정된 끝점

  (setq dxy (list (+ (car ep) (* dx sgn)) (car ep) 0.0))  ;치수선이 놓일 위치

  (setq dy (distance sp ep))                          ;두 점의 거리

  (if (< (* dy sc) 1000.0)
    (setq txt (rtos (* dy sc) 2 0))                          ;1000미만일 때
    (setq txt (rtos (* dy 0.001 sc) 2 3))                ;1000이상일 때
  ) ;of if(dy < 1000)

  (if (> n 1)
    (progn
      (setq divl dst)                                   ;나누는 길이 입력
      (setq divn (rtos n 2 0))                          ;나눈 갯수계산
      (if (< divl 1000.)
        (setq divl (rtos (* divl sc) 2 0))                   ;나누는 길이가 1000미만시
        (setq divl (rtos (* divl 0.001 sc) 2 3))) ;of if           ;나누는 길이가 1000이상시
      (setq txtlen (* (+ (strlen txt) (strlen divn) (strlen divl) 2) th ds
                   (cdr (assoc 41 (tblsearch "STYLE" (getvar "TEXTSTYLE"))))))
      (if (>= txtlen dy)
        (progn                                  ;text가 보조선 내에 안들어가면
          (setq dtxt1 (strcat divn "@" divl))   ;두줄로 나눔
          (setq dtxt2 (strcat "=" txt))
          (setq dtxt1p (mapcar '+ (mid-point sp ep)
                                  (list (- (* dx sgn) (* ds 2.5)) 0.0 0.0)))
          (setq dtxt2p (mapcar '+ (mid-point sp ep)
                                  (list (+ (* dx sgn) (* ds 2.5)) 0.0 0.0)))
          (command "TEXT" "M" dtxt1p (* th ds) "90" dtxt1)
          (command "TEXT" "M" dtxt2p (* th ds) "90" dtxt2)
          (command "DIM1" "VER" sp ep dxy " ")               ;DIM명령 내림
        ) ;of progn THEN
        (progn                                  ;text가 보조선 내에 들어가면
          (setq dtxt1 (strcat divn "@" divl "=" txt))
          (command "DIM1" "VER" sp ep dxy dtxt1)               ;DIM명령 내림
        ) ;of progn ELSE
      ) ;of IF
    ) ;of progn THEN
    (progn
      (if (or (= txt1 nil)
              (or (= (type txt1) (type 1)) (= (type txt1) (type 1.0))))
        (setq txt1 txt))                  ;리턴입력시 옛 text를 씀
      (command "DIM1" "VER" sp ep dxy txt1)               ;DIM명령 내림
    ) ;of progn ELSE
  ) ;of if
  ep
) ;defun


;*************************************
;     Program : F_B5
;               Function reBar marking 5
;               By Suk-Jong Yi
;               96/12/13
;**************************************************************
; 철금 마킹중 아래의 모양 (함수)
;     /--/--/--Uc
;
; (F_B5 SSLST SP EP DIR MK DIA)
;       SSLST                 : 선택 entity list
;             SP              : Start point, 보조선의 시작점
;                EP           : End point, 보조선의 끝점
;                   P3        : 밑선의 위치
;                      MK     : Marking
;                         DIA : 철근 종류 및 DIA
;**************************************************************
(defun F_B5(SSLST SP EP DIR MK DIA /                       ;지역변수 정의
        oldclr      index       nnsent      entl        entype
        crsxy       spexy       epexy       crsxy       slp         elp
        sp          ep          sslst       enum        index       enum
)

  (push-env)

  (setvar "CMDECHO" 0)                        ;환경변수 셋팅 명령메아리 끄기
  (setvar "BLIPMODE" 0)                       ;BLIP MODE 끄기
  (setq oldclr (getvar "CECOLOR"))            ;옛색깔 기억하기
  (setvar "CECOLOR" "RED")                    ;색깔은 빨간색으로
  (setq ds (getvar "DIMSCALE"))               ;스케일
  (setq rc (* ds 3.5))                        ;마킹원의 반지름

;  (princ "\nSelect objects: ")                ;마킹 대상 엔티티 선택
;  (setq sslst (ssget))

  (setq enum (sslength sslst))                ;seclection set의 엔티티 갯수


  ;**** 대상 엔티티 강조, 대상엔티티 갯수 파악 (LINE과 ARC만)
  (setq index 0                               ;엔티티 갯수
        nssent 0)                             ;line이거나 arc인 엔티티 갯수

  (repeat enum                  ;엔티티 갯수 만큼 반복
    (setq entl (entget (ssname sslst index)))      ;엔티티 리스트
    (setq entype (cdr (assoc 0 entl)))             ;엔티티 타입
    (if (or (= entype "LINE") (= entype "ARC"))    ;엔티티가 line이거나 arc인경우
      (progn
        (redraw (ssname sslst index) 3)            ;엔티티 강조
        (if (= nssent 0) (setq ssent (ssadd (ssname sslst index)))
                         (setq ssent (ssadd (ssname sslst index) ssent))
        ) ;of if                    ; line이거나 arc엔티티모은 slection set 만들기
        (setq nssent (1+ nssent))                     ;대상 엔티티 갯수 count up
      ) ;of progn
    ) ; of if
    (setq index (1+ index))                           ;다음 엔티티로
  ) ;of repeat
  (print nssent)
  (princ "Entity Found")                              ;LINE이나 ARC가 몇개인가?

;  (setq sp (getpoint "\nPick start point: "))         ;보조선의 첫점
;  (setq ep (getpoint sp "\nPick end point: "))        ;보조선의 끝점
  (setq seang (angle sp ep))                          ; 보조선의 각도
  (command "LINE" sp ep "")                           ;보조선 그리기

  (setq index 0)
  (repeat nssent                                  ;ARC이거나 LINE인 엔티티만큼
     (setq entl (entget (ssname ssent index)))    ;엔티티 리스트 구하기
     (cond
       ((= (cdr (assoc 0 entl)) "ARC")            ;마킹 대상물이 ARC인 경우
         (setq crsxy (cross entl sp ep))            ;ARC와 보조선의 교차점찾기
       ) ;of entity=ARC
       ((= (cdr (assoc 0 entl)) "LINE")           ; 마킹 대상물이 LINE인 경우
         (setq spexy (cdr (assoc 10 entl))          ;보조선의 시작과 끝점
               epexy (cdr (assoc 11 entl)))
         (setq crsxy (inters spexy epexy sp ep))    ;보조선과 LINE의 교차점 찾기
       ) ;of entity=LINE
     ) ;of cond
     (setq slp (polar crsxy (+ seang (* pi 0.25)) (* 1.3 ds)))
     (setq elp (polar crsxy (+ seang (* pi 1.25)) (* 1.3 ds)))
     (command "LINE" slp elp "")                  ; Tick line그리기 /
     (redraw (ssname ssent index) 4)              ; 강조된 엔티티 원상복구
     (setq index (1+ index))                      ; 다음 엔티티로
  ) ; repeat

;  (setq p3 (getpoint ep "\nPick base line: "))              ;base line point


   (cond                                                 ;Marking의 방향
     ((= dir 1) (setq xsgn 1))
     ((= dir 2) (setq ysgn 1))
     ((= dir 3) (setq xsgn -1))
     ((= dir 4) (setq ysgn -1))
   ) ;of cong

;  (setq dx (- (car p3) (car ep)))
;  (if (< dx 0)                                          ;base line의 x방향인식
;    (setq xsgn -1)
;    (setq xsgn 1)
;  ) ;of if
;
;  (setq dy (- (cadr p3) (cadr ep)))
;  (if (<  dy 0)                                         ;base line의 y방향인식
;    (setq ysgn -1)
;    (setq ysgn  1)
;  ) ;of if

  (setq blen (+ (* ds 7) (* 4 ds 2.5)))                     ;base line의 길이

;  (if (> (abs dx) (abs dy))                                 ;누워있나 서있나?
  (if (or (= dir 1) (= dir 3))                                 ;누워있나 서있나?
    (progn
      (setq p4 (list (+ (car ep) (* blen xsgn)) (cadr ep)))
      (command "LINE" ep p4 "")
;      (if (< dx 0)
      (if (< xsgn 0)
        (setq ip p4)
        (setq ip ep)
      ) ;of if
      (setq cp (list (+ (car ip) (* ds 3.5))
                     (+ (cadr ip) (* ds 3.5))))
      (setq diaxy (list (+ (car cp) (* 4 ds)) (- (cadr cp) (* 3 ds)) 0.0))      ;dia표시 위치
      (setq txtrot 0)                                   ;text회전각
    ) ;of progn
    (progn
      (setq p4 (list (car ep) (+ (cadr ep) (* blen ysgn))))
      (command "LINE" ep p4 "")
;      (if (< dy 0)
      (if (< ysgn 0)
        (setq ip p4)
        (setq ip ep)
      ) ;of if
      (setq cp (list (- (car ip) (* ds 3.5))
                     (+ (cadr ip) (* ds 3.5))))
      (setq diaxy (list (+ (car cp) (* 3 ds)) (+ (cadr cp) (* 4 ds)) 0.0))      ;dia표시 위치
      (setq txtrot 90)
    ) ;of progn
  ) ;of if


  (command "CIRCLE" cp rc)
  (setvar "CECOLOR" oldclr)

  (setvar "CECOLOR" "WHITE")
;  (setq mk (getstring "\nEnter Marking: "))
  (txtinc mk cp txtrot)
;  (setq dia (getstring "\nEnter Rebar Dia: "))
  (command "TEXT" diaxy (* 2.5 ds) txtrot (strcase dia))
  (setvar "CECOLOR" oldclr)

  (pop-env)

  (princ)
) ; defun

;********************************************
; FUCTION : TXTINC
;           TeXT IN Circle
;           Suk-Jong Yi
;           96/5/7
;********************************************
; 원 안에 철근번호를 기입해준다.
; 넘어오는 값
;         TXT: TEXT
;        IPNT: Insert point
;      TXTROT: TeXT ROTation
;********************************************

(defun TXTINC(TXT IPNT TXTROT)
  (setq txtl (strlen txt))
  (if (> txtl 3)
    (progn
      (setq count 1)
      (while (and (/= (substr txt count 1) "-")
                 (< count txtl))
        (setq count (1+ count)))
      (if (= count txtl)
        (command "TEXT" "M" ipnt (* 2.5 ds) TXTROT (strcase txt))
        (progn
          (command "TEXT" "C" ipnt (* 2.5 ds) TXTROT
                   (strcase (substr txt 1 (- count 1))))
          (command "TEXT" "TC" ipnt (* 2.5 ds) TXTROT
                   (strcase (substr txt count (+ (- txtl count) 1))))
        ) ;of progn
      ) ;of IF
    ) ;of PROGN
    (command "TEXT" "M" ipnt (* 2.5 ds) TXTROT (strcase txt))
  ) ;of IF
) ;of DEFUN


