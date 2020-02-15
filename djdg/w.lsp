;****************************************************
; Program: WELD.LSP
;          WELD mark
;          Yi Suk Jong
;          97/2/29
;****************************************************

(defun C:WELD(/
)

  ;;
  ;; 내장 error routine
  ;;
  (defun SETERR(s)                                      ;내장 에러루틴 정의
  ;If an error (CTRL-C) occurs when this command is active.
    (if (/= s "Function cancelled! ")
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
  ;; Function: WELD_DIA (Dialog box로 입력받기)
  ;;
  (defun WELD_DIA (/
        dcl_id
  )
    (setq dcl_id (load_dialog "DJDG"))                  ;dialog호출
    (if (not (new_dialog "WLED" dcl_id)) (exit))

    (start_image "weld")                                  ;image 보이기
    (slide_image
                 0 0
                 (dimx_tile "weld") (dimy_tile "weld")
                 "djdg(weld)"
    )
    (end_image)

    (if (= #X1 nil) (setq #X1  50))
    (if (= #N1 nil) (setq #N1   8))                      ;초기치 설정
    (if (= #P1 nil) (setq #P1 150))
    (if (= #X2 nil) (setq #X2 100))
    (if (= #N2 nil) (setq #N2   8))
    (if (= #P2 nil) (setq #P2 150))
    (if (= #N3 nil) (setq #N3   3))
    (if (= #P3 nil) (setq #P3 150))
    (if (= #A1 nil) (setq #A1  50))
    (if (= #A2 nil) (setq #A2  62))
    (if (= #A3 nil) (setq #A3  50))
    (if (= #A4 nil) (setq #A4 850))
    (if (= #B1 nil) (setq #B1 100))
    (if (= #B2 nil) (setq #B2 100))
    (if (= #M1 nil) (setq #M1 "S1"))
    (if (= #D1 nil) (setq #D1 "D16"))
    (if (= #M2 nil) (setq #M2 "S2"))
    (if (= #D2 nil) (setq #D2 "D16"))
    (if (= #M3 nil) (setq #M3 "S"))
    (if (= #D3 nil) (setq #D3 "D16"))
    (if (= #M4 nil) (setq #M4 "S"))
    (if (= #D4 nil) (setq #D4 "D16"))
    (if (= #SCALE nil) (setq #SCALE 20))


    (set_tile "x1" (rtos #X1 2 0))
    (set_tile "n1" (rtos #N1 2 0))                  ;초기치로 edit box setting
    (set_tile "p1" (rtos #P1 2 0))
    (set_tile "x2" (rtos #X2 2 0))
    (set_tile "n2" (rtos #N2 2 0))
    (set_tile "p2" (rtos #P2 2 0))
    (set_tile "n3" (rtos #N3 2 0))
    (set_tile "p3" (rtos #P3 2 0))
    (set_tile "a1" (rtos #A1 2 0))
    (set_tile "a2" (rtos #A2 2 0))
    (set_tile "a3" (rtos #A3 2 0))
    (set_tile "a4" (rtos #A4 2 0))
    (set_tile "b1" (rtos #B1 2 0))
    (set_tile "b2" (rtos #B2 2 0))
    (set_tile "m1" #M1)
    (set_tile "d1" #D1)
    (set_tile "m2" #M2)
    (set_tile "d2" #D2)
    (set_tile "m3" #M3)
    (set_tile "d3" #D3)
    (set_tile "m4" #M4)
    (set_tile "d4" #D4)
    (set_tile "scale" (rtos #SCALE 2 0))
    (set_tile "total1" (rtos (+ (* #X1 2) (* #N1 #P1)) 2 0))
    (set_tile "total2" (rtos (+ (* #X2 2) (* #N2 #P2)) 2 0))


    (action_tile "x1" "(set_val $key)")
    (action_tile "n1" "(set_val $key)")                   ;dialog box Action
    (action_tile "p1" "(set_val $key)")
    (action_tile "x2" "(set_val $key)")
    (action_tile "n2" "(set_val $key)")
    (action_tile "p2" "(set_val $key)")
    (action_tile "n3" "(set_val $key)")
    (action_tile "p3" "(set_val $key)")
    (action_tile "a1" "(set_val $key)")
    (action_tile "a2" "(set_val $key)")
    (action_tile "a3" "(set_val $key)")
    (action_tile "a4" "(set_val $key)")
    (action_tile "b1" "(set_val $key)")
    (action_tile "b2" "(set_val $key)")
    (action_tile "m1" "(set_val $key)")
    (action_tile "d1" "(set_val $key)")
    (action_tile "m2" "(set_val $key)")
    (action_tile "d2" "(set_val $key)")
    (action_tile "m3" "(set_val $key)")
    (action_tile "d3" "(set_val $key)")
    (action_tile "m4" "(set_val $key)")
    (action_tile "d4" "(set_val $key)")
    (action_tile "scale" "(set_scale)")
    (action_tile "accept" "(do_accept)")
    (action_tile "cancel" "(do_cancel)")
    (mode_tile "x1" 2)
    (start_dialog)
    (unload_dialog dcl_id)
  ) ;of defun SPLICE_DIALOG

  ;;;
  ;;; dialog box에서 값 입력받아 변수에 저장
  ;;;
  (defun SET_VAL (key / in value)              ;n1 edit_box에 입력이 들어왔을 때
    (setq in (get_tile key))
    (cond
      ((= key "x1")
        (setq value (atoi in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #X1 value)
            (set_tile "error" "")
            (set_tile "total1" (rtos (+ (* #X1 2) (* #N1 #P1)) 2 0))
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=x1
      ((= key "n1")
        (setq value (atoi in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #N1 value)
            (set_tile "error" "")
            (set_tile "total1" (rtos (+ (* #X1 2) (* #N1 #P1)) 2 0))
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=n1
      ((= key "p1")
        (setq value (atoi in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #P1 value)
            (set_tile "error" "")
            (set_tile "total1" (rtos (+ (* #X1 2) (* #N1 #P1)) 2 0))
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=p1
      ((= key "x2")
        (setq value (atoi in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #X2 value)
            (set_tile "error" "")
            (set_tile "total2" (rtos (+ (* #X2 2) (* #N2 #P2)) 2 0))
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=x2
      ((= key "n2")
        (setq value (atoi in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #N2 value)
            (set_tile "error" "")
            (set_tile "total2" (rtos (+ (* #X2 2) (* #N2 #P2)) 2 0))
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=n2
      ((= key "p2")
        (setq value (atoi in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #P2 value)
            (set_tile "error" "")
            (set_tile "total2" (rtos (+ (* #X2 2) (* #N2 #P2)) 2 0))
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=p2
      ((= key "n3")
        (setq value (atoi in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #N3 value)
            (set_tile "error" "")
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=n3
      ((= key "p3")
        (setq value (atoi in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #P3 value)
            (set_tile "error" "")
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=p3
      ((= key "a1")
        (setq value (atoi in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #A1 value)
            (set_tile "error" "")
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=a1
      ((= key "a2")
        (setq value (atoi in))
;        (if (= value 0)
;          (progn
;            (do_error key in)
;            nil
;          ) ;of THEN
;          (progn
            (setq #A2 value)
            (set_tile "error" "")
            T
;          ) ;of ELSE
;        ) ;of if
      ) ;of key=a2
      ((= key "a3")
        (setq value (atoi in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #A3 value)
            (set_tile "error" "")
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=a3
      ((= key "a4")
        (setq value (atoi in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #A4 value)
            (set_tile "error" "")
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=a4
      ((= key "b1")
        (setq value (atoi in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #B1 value)
            (set_tile "error" "")
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=b1
      ((= key "b2")
        (setq value (atoi in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #B2 value)
            (set_tile "error" "")
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=b2
      ((= key "scale")
        (setq value (atoi in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #SCALE value)
            (set_tile "error" "")
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=p3
      ((= key "m1")
        (setq #M1 (get_tile "m1"))
        T
      ) ;of key=m1
      ((= key "d1")
        (setq #D1 (get_tile "d1"))
        T
      ) ;of key=d1
      ((= key "m2")
        (setq #M2 (get_tile "m2"))
        T
      ) ;of key=m2
      ((= key "d2")
        (setq #D2 (get_tile "d2"))
        T
      ) ;of key=d2
      ((= key "m3")
        (setq #M3 (get_tile "m3"))
        T
      ) ;of key=m3
      ((= key "d3")
        (setq #D3 (get_tile "d3"))
        T
      ) ;of key=d3
      ((= key "m4")
        (setq #M4 (get_tile "m4"))
        T
      ) ;of key=m4
      ((= key "d4")
        (setq #D4 (get_tile "d4"))
        T
      ) ;of key=d4
    ) ;of cond
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
        (setq #SCALE value)
        (set_tile "error" "")
        T
      ) ;of ELSE
    ) ;of IF
   ) ;of defun


  ;;;
  ;;; ok버튼을 누렀을 때
  ;;;
  (defun do_accept()           ;dialog box를 끝내기 전에 모든 입력 데이타 확인
    (if (and (set_val "x1") (set_val "n1") (set_val "p1")
             (set_val "x2") (set_val "n2") (set_val "p2")
             (set_val "a1") (set_val "a2") (set_val "a3") (set_val "a4")
             (set_val "b1") (set_val "b2") (set_val "n3") (set_val "p3")
             (set_val "m1") (set_val "d1")
             (set_val "m2") (set_val "d2")
             (set_val "m3") (set_val "d3")
             (set_val "m4") (set_val "d4")
             (set_val "scale"))
      (done_dialog)
    ) ;of IF
  ) ;of defun


  ;;;
  ;;; 잘못된 값이 입력됬을 때
  ;;;
  (defun do_error(tile value)
    (set_tile "error" "Invalid input")            ;error massage창에 에러표시

    (if (or (= tile "x1") (= tile "n1") (= tile "p1")
            (= tile "x2") (= tile "n2") (= tile "p2")
            (= tile "a1") (= tile "a2") (= tile "a3") (= tile "a4")
            (= tile "b1") (= tile "b2") (= tile "n3") (= tile "p3")
            (= tile "scale"))
      (progn
        (set_tile  tile value)
        (mode_tile tile 2)
      ) ;of THEN
    ) ;of IF
  ) ;of defun


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

  (push-env)

  (setq p1 (getpoint "\nPick first point: "))
  (setq p2 (getpoint p1 "\nPick first point: "))

;  (setq dx (- (car p1) (car p2)))

;  (if (< dx 0)
;    (

  (weld_dia)

  (pop-env)

) ;of defun

;******************************************
; Program : F_DH
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
;      TXT1 : 직접입력하고싶은 text(숫자이면 scale)
; 돌려주는 값 - 끝점 좌표
; ex) (setq pp (f_dh SP DST N UD TXT1))
;******************************************

(defun F_DH(SP DST N UD TXT1
/  sp dst n ud txt1
   th       dim_gap     ds      sgn     dy      next    ep      dxy
   dx       txt         divl    divn    txtlen  dtxt1   dtxt2   dtxt1p
   dtxt2p
)

  (setq th (getvar "DIMTXT")
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
                                  (list 0.0 (+ (* dy sgn) (* ds th)) 0.0)))
          (setq dtxt2p (mapcar '+ (mid-point sp ep)
                                  (list 0.0 (- (* dy sgn) (* ds th)) 0.0)))
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
; Function : F_DV
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

  (setq th (getvar "DIMTXT")                          ;text크기 = dimtxt
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
                                  (list (- (* dx sgn) (* ds th)) 0.0 0.0)))
          (setq dtxt2p (mapcar '+ (mid-point sp ep)
                                  (list (+ (* dx sgn) (* ds th)) 0.0 0.0)))
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
;     /--/--/--①
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
;  (print nssent)
;  (princ "Entity Found")                              ;LINE이나 ARC가 몇개인가?

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

  (setq blen (+ (* ds 7) (* 4 ds th)))                     ;base line의 길이

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
  (command "TEXT" diaxy (* th ds) txtrot (strcase dia))
  (setvar "CECOLOR" oldclr)

  (pop-env)

  (princ)
) ; defun

;**************************************
; Program : F_B3
;           Bar Mark 3
;           Suk-Jong Yi
;           97/1/22
;**************************************
;철근마킹중 아래의 모양을 만들어준다.
;            ①
;           /|\
;         /  |  \
; (F_B3 SSLST  MP DIR MK DIA)
;       SSLST                : 선택 entity list
;              MP            : Marking위치
;                 DIR        : 밑선의 위치
;                     MK     : Marking
;                        DIA : 철근 종류 및 DIA
;**************************************

(defun F_B3( SSLST MP DIR MK DIA /
       plst   oldclr   ds      entlst  mpnt    nent           ;지역변수 정의
       index  ent      entype  sp      ep      mp     npnt
       vtx1   nxt1     vtx2    vtx1p   vtx2p   cenp
       cp     ang      minang  maxang  dtang   mang
       ccen   mk       dia     diaxy
)

  (push-env)                                    ;환경변수값 대피
  (setq oldclr (getvar "CECOLOR"))
  (setvar "CECOLOR" "RED")

  (setq ds (getvar "DIMSCALE"))
  (setq rc (* 3.5 ds))                            ;마킹원의 반지름

;  (setq entlst (ssget))                           ;마킹대상 entity선택
   (setq entlst sslst)
;  (setq mpnt (getpoint "Pick marking point: "))   ;마킹 원을 그릴 위치선택
   (setq mpnt mp)
  (setq nent (sslength entlst))                   ;마킹대상 엔티티갯수

  (setq plst nil)                 ;plst: 마킹대상 entity들의  포인트리스트
  (setq npnt 0)       ;포인트 갯수 (마킹대상 엔티티갯수 /= 마킹대상포인트갯수)
  (setq index 0)
  (repeat nent                                    ;대상 엔티티갯수만큼 반복
    (setq ent (entget (ssname entlst index)))
    (setq entype (cdr (assoc 0 ent)))             ;엔티티타입 구함
    (cond
      ((= entype "LINE")                          ;엔티티가 라인인경우
        (setq sp (cdr (assoc 10 ent)))            ;라인의 시작점
        (setq ep (cdr (assoc 11 ent)))            ;라인의 끝점
        (setq mp (list (/ (+ (car sp) (car ep)) 2.0)          ;라인중간점 X
                       (/ (+ (cadr sp) (cadr ep)) 2.0) 0.0))  ;           Y
        (setq plst (append plst (list mp)))       ;중간점을 마킹포인트에 추가
        (setq npnt (1+ npnt))                     ;마킹포인트의 갯수 증가
      ) ;of entype="LINE"
      ((= entype "POLYLINE")      ;폴리라인인 경우(철근을 도나스로 그린 경우)
        (setq vtx1 (entget (setq nxt1 (entnext (ssname entlst index))))) ;첫점정보
        (if (/= (abs (cdr (assoc 42 vtx1))) 0)         ;폴리라인이 아크형인가?
          (progn
            (setq vtx2 (entget (setq nxt2 (entnext nxt1))))   ;두번째점 정보
            (setq vtx1p (cdr (assoc 10 vtx1)))                ;첫점 포인트
            (setq vtx2p (cdr (assoc 10 vtx2)))                ;둘째점 포인트
            (setq cenp (list (/ (+ (car vtx1p) (car vtx2p)) 2.0)     ;센타포인트 X
                             (/ (+ (cadr vtx1p) (cadr vtx2p)) 2.0))) ;           Y
            (setq plst (append plst (list cenp)))             ;포인트리스트에 추가
            (setq npnt (1+ npnt))                             ;포인트 갯수 증가
          ) ;of progn
        ) ;of if
      ) ;of entype="PLINE"
      ((= entype "CIRCLE")                                ;엔티티가 서클인 경우
        (setq cp (cdr (assoc 10 ent)))                    ;서클의 센타포인트
        (setq plst (append plst (list cp)))               ;포인트리스트에 추가
        (setq npnt (1+ npnt))                             ;포인트 갯수 증가
      ) ;of entype="CIRCLE"
    ) ;of cond
  (setq index (1+ index))                                 ;index=엔티티번호
  ) ; of repeat


  (setq ang (angle mpnt (nth 0 plst)))        ;첫점의 각을 구한다
  (setq minang ang)                           ;최대각과 최소각을 첫점의 각으로한다
  (setq maxang ang)
  (setq index 0)
  (repeat npnt                                ;포인트 갯수만큼 반복
    (setq ang (angle mpnt (nth index plst)))  ;마킹원의 포인트와 점의 각을 구한다
    (if (<= ang minang) (setq minang ang))    ;최대각과 최소각 찾기
    (if (>= ang maxang) (setq maxang ang))
    (command "LINE" mpnt (nth index plst) "") ;포인트에서 마킹원점까지 선을그린다
    (setq index (1+ index))                   ;포인트갯수만큼 반복
  ) ;of repeat

  ;-------------------------------
  ; base line과 making/dia 쓰기
  ;-------------------------------

;  (setq p3 (getpoint mpnt "\nPick base line: "))              ;base line point

;  (setq dx (- (car p3) (car mpnt)))
;  (if (< dx 0)                                          ;base line의 x방향인식
;    (setq xsgn -1)
;    (setq xsgn 1)
;  ) ;of if
;
;  (setq dy (- (cadr p3) (cadr mpnt)))
;  (if (<  dy 0)                                         ;base line의 y방향인식
;    (setq ysgn -1)
;    (setq ysgn  1)
;  ) ;of if

   (cond                                                 ;Marking의 방향
     ((= dir 1) (setq xsgn 1))
     ((= dir 2) (setq ysgn 1))
     ((= dir 3) (setq xsgn -1))
     ((= dir 4) (setq ysgn -1))
   ) ;of cong

  (setq blen (+ (* ds 7) (* 4 ds th)))                     ;base line의 길이

;  (if (> (abs dx) (abs dy))                                             ;누워있나 서있나?
  (if (or (= dir 1) (= dir 3))                                 ;누워있나 서있나?
    (progn
      (setq p4 (list (+ (car mpnt) (* blen xsgn)) (cadr mpnt)))
      (command "LINE" mpnt p4 "")
;      (if (< dx 0)
      (if (< xsgn 0)
        (setq ip p4)
        (setq ip mpnt)
      ) ;of if
      (setq cp (list (+ (car ip) (* ds 3.5))
                     (+ (cadr ip) (* ds 3.5))))
      (setq diaxy (list (+ (car cp) (* 4 ds)) (- (cadr cp) (* 3 ds)) 0.0))      ;dia표시 위치
      (setq txtrot 0)                                   ;text회전각
    ) ;of progn
    (progn
      (setq p4 (list (car mpnt) (+ (cadr mpnt) (* blen ysgn))))
      (command "LINE" mpnt p4 "")
;      (if (< dy 0)
      (if (< ysgn 0)
        (setq ip p4)
        (setq ip mpnt)
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
  (command "TEXT" diaxy (* th ds) txtrot (strcase dia))
  (setvar "CECOLOR" oldclr)

  (pop-env)   ;환경변수값 복귀

  (princ)


); of defun


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
        (command "TEXT" "M" ipnt (* th ds) TXTROT (strcase txt))
        (progn
          (command "TEXT" "C" ipnt (* th ds) TXTROT
                   (strcase (substr txt 1 (- count 1))))
          (command "TEXT" "TC" ipnt (* th ds) TXTROT
                   (strcase (substr txt count (+ (- txtl count) 1))))
        ) ;of progn
      ) ;of IF
    ) ;of PROGN
    (command "TEXT" "M" ipnt (* th ds) TXTROT (strcase txt))
  ) ;of IF
) ;of DEFUN