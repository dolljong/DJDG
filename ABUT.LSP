;******************************
; Program : ABUT
;           Abutment drawing
;           Suk-Jong Yi
;           96/6/22
;******************************
; 교대 그리기
;******************************

(defun C:ABUT(
/)

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
  ;; Function: ABUT_DIALOG (Dialog box로 입력받기)
  ;;
  (defun ABUT_DIALOG (/
                       dcl_id
  )
    (setq dcl_id (load_dialog "DJDG"))                ;dialog호출
    (if (not (new_dialog "ABUT" dcl_id)) (exit))

    (start_image "dd_abut")                           ;image 보이기
    (slide_image
                 0 0
                 (dimx_tile "dd_abut") (dimy_tile "dd_abut")
                 "djdg(ddabut)"
    )
    (end_image)

    (if (= #H1 nil) (setq #H1 2.0))                       ;초기치 설정
    (if (= #H2 nil) (setq #H2 4.0))
    (if (= #H3 nil) (setq #H3 1.5))
    (if (= #H4 nil) (setq #H4 0.6))
    (if (= #H5 nil) (setq #H5 2.0))
    (if (= #H6 nil) (setq #H6 0.7))
    (if (= #B1 nil) (setq #B1 1.2))
    (if (= #B2 nil) (setq #B2 0.5))
    (if (= #B3 nil) (setq #B3 0.7))
    (if (= #B4 nil) (setq #B4 1.2))
    (if (= #B5 nil) (setq #B5 1.5))
    (if (= #B6 nil) (setq #B6 3.0))
    (if (= #YS nil) (setq #YS 1.0))
    (setq #DIM "0")

    (set_tile "h1"  (rtos #H1 2 3))                      ;초기치로 edit box setting
    (set_tile "h2"  (rtos #H2 2 3))
    (set_tile "h3"  (rtos #H3 2 3))
    (set_tile "h4"  (rtos #H4 2 3))
    (set_tile "h5"  (rtos #H5 2 3))
    (set_tile "h6"  (rtos #H6 2 3))
    (set_tile "b1"  (rtos #B1 2 3))
    (set_tile "b2"  (rtos #B2 2 3))
    (set_tile "b3"  (rtos #B3 2 3))
    (set_tile "b4"  (rtos #B4 2 3))
    (set_tile "b5"  (rtos #B5 2 3))
    (set_tile "b6"  (rtos #B6 2 3))
    (set_tile "ys"  (rtos #YS 2 3))

    (action_tile "h1" "(set_h1)")                       ;dialog box Action
    (action_tile "h2" "(set_h2)")
    (action_tile "h3" "(set_h3)")
    (action_tile "h4" "(set_h4)")
    (action_tile "h5" "(set_h5)")
    (action_tile "h6" "(set_h6)")
    (action_tile "b1" "(set_b1)")
    (action_tile "b2" "(set_b2)")
    (action_tile "b3" "(set_b3)")
    (action_tile "b4" "(set_b4)")
    (action_tile "b5" "(set_b5)")
    (action_tile "b6" "(set_b6)")
    (action_tile "ys" "(set_ys)")
    (action_tile "dim" "(set-dim)")                     ;dimension ON/OFF toggle
    (action_tile "accept" "(do_accept)")
    (action_tile "cancel" "(do_cancel)")
    (mode_tile "h1" 2)
    (start_dialog)
    (unload_dialog dcl_id)
  ) ;of defun ABUT_DIALOG

  ;;
  ;; FUNCTION : SET_H1 (h1값 입력)
  ;;
  (defun SET_H1 (/ in value)              ;n1 edit_box에 입력이 들어왔을 때
    (setq in (get_tile "h1"))
    (setq value (atof in))                ;string을 값으로
    (if (= value 0)                            ;0이 입력되면
      (progn
        (do_error "h1" in)
        nil
      ) ;of THEN
      (progn
        (setq #H1 value)
        (set_tile "error" "")
        T
      ) ;of ELSE
    ) ;of IF
   ) ;of defun

  ;;
  ;; FUNCTION : SET_H2 (c2값 입력)
  ;;
  (defun SET_H2(/ in value)              ;p1 edit_box에 입력이 들어왔을 때
    (setq in (get_tile "h2"))
    (setq value (atof in))              ;string을 값으로
    (if (= value 0)                     ;값이 0인 경우
      (progn
        (do_error "h2" in)
        nil
      ) ;of THEN
      (progn
        (setq #H2 value)
        (set_tile "error" "")
        T
      ) ;of ELSE
    ) ;of IF
   ) ;of defun

  ;;
  ;; FUNCTION : SET_H3 (h3값 입력)
  ;;
  (defun SET_H3(/ in value)              ;e1 edit_box에 입력이 들어왔을 때
    (setq in (get_tile "h3"))
    (setq value (atof in))
    (if (= vlaue 0)
      (progn
        (do_error "h3" in)
        nil
      ) ;of THEN
      (progn
        (setq #H3 value)
        (set_tile "error" "")
        T
      ) ;of ELSE
    ) ;of IF
   ) ;of defun

  ;;
  ;; FUNCTION : SET_H4 (h4값 입력)
  ;;
  (defun SET_H4(/ in value)              ;n2 edit_box에 입력이 들어왔을 때
    (setq in (get_tile "h4"))
    (setq value (atof in))
    (if (= value 0)
      (progn
        (do_error "h4" in)
        nil
      ) ;of THEN
      (progn
        (setq #H4 value)
        (set_tile "error" "")
        T
      ) ;of ELSE
    ) ;of IF
   ) ;of defun

  ;;
  ;; FUNCTION : SET_H5 (h5값 입력)
  ;;
  (defun SET_H5(/ n vlaue)              ;p2 edit_box에 입력이 들어왔을 때
    (setq in (get_tile "h5"))
    (setq value (atof in))
    (if (= value 0)
      (progn
        (do_error "h5" in)
        nil
      ) ;of THEN
      (progn
        (setq #H5 value)
        (set_tile "error" "")
        T
      ) ;of ELSE
    ) ;of IF
   ) ;of defun

  ;;
  ;; FUNCTION : SET_H6 (h6값 입력)
  ;;
  (defun SET_H6(/ n value)              ;e2 edit_box에 입력이 들어왔을 때
    (setq in (get_tile "h6"))
    (setq value (atof in))
    (if (= value 0)
      (progn
        (do_error "h6" in)
        nil
      ) ;of THEN
      (progn
        (setq #H6 value)
        (set_tile "error" "")
        T
      ) ;of ELSE
    ) ;of IF
   ) ;of defun

  ;;
  ;; FUNCTION : SET_B1 (b1값 입력)
  ;;
  (defun SET_B1(/ n value)              ;xs edit_box에 입력이 들어왔을 때
    (setq in (get_tile "b1"))
    (setq value (atof in))
    (if (= value 0)
      (progn
        (do_error "b1" in)
        nil
      ) ;of THEN
      (progn
        (setq #B1 value)
        (set_tile "error" "")
        T
      ) ;of ELSE
    ) ;of IF
   ) ;of defun

  ;;
  ;; FUNCTION : SET_B2 (b2값 입력)
  ;;
  (defun SET_B2(/ n value)              ;xs edit_box에 입력이 들어왔을 때
    (setq in (get_tile "b2"))
    (setq value (atof in))
    (if (= value 0)
      (progn
        (do_error "b2" in)
        nil
      ) ;of THEN
      (progn
        (setq #B2 value)
        (set_tile "error" "")
        T
      ) ;of ELSE
    ) ;of IF
   ) ;of defun

  ;;
  ;; FUNCTION : SET_B3 (b3값 입력)
  ;;
  (defun SET_B3(/ n value)              ;xs edit_box에 입력이 들어왔을 때
    (setq in (get_tile "b3"))
    (setq value (atof in))
    (if (= value 0)
      (progn
        (do_error "b3" in)
        nil
      ) ;of THEN
      (progn
        (setq #B3 value)
        (set_tile "error" "")
        T
      ) ;of ELSE
    ) ;of IF
   ) ;of defun

  ;;
  ;; FUNCTION : SET_B4 (b4값 입력)
  ;;
  (defun SET_B4(/ n value)              ;xs edit_box에 입력이 들어왔을 때
    (setq in (get_tile "b4"))
    (setq value (atof in))
    (if (= value 0)
      (progn
        (do_error "b4" in)
        nil
      ) ;of THEN
      (progn
        (setq #B4 value)
        (set_tile "error" "")
        T
      ) ;of ELSE
    ) ;of IF
   ) ;of defun
  ;;
  ;; FUNCTION : SET_B5 (b5값 입력)
  ;;
  (defun SET_B5(/ n value)              ;xs edit_box에 입력이 들어왔을 때
    (setq in (get_tile "b5"))
    (setq value (atof in))
    (if (= value 0)
      (progn
        (do_error "b5" in)
        nil
      ) ;of THEN
      (progn
        (setq #B5 value)
        (set_tile "error" "")
        T
      ) ;of ELSE
    ) ;of IF
   ) ;of defun

  ;;
  ;; FUNCTION : SET_B6 (b6값 입력)
  ;;
  (defun SET_B6(/ n value)              ;xs edit_box에 입력이 들어왔을 때
    (setq in (get_tile "b6"))
    (setq value (atof in))
    (if (= value 0)
      (progn
        (do_error "b6" in)
        nil
      ) ;of THEN
      (progn
        (setq #B6 value)
        (set_tile "error" "")
        T
      ) ;of ELSE
    ) ;of IF
   ) ;of defun

  ;;
  ;; FUNCTION : SET_YS (ys값 입력)
  ;;
  (defun SET_YS(/ n value)              ;ys edit_box에 입력이 들어왔을 때
    (setq in (get_tile "ys"))
    (setq value (atof in))
    (if (= value 0)
      (progn
        (do_error "ys" in)
        nil
      ) ;of THEN
      (progn
        (setq #YS value)
        (set_tile "error" "")
        T
      ) ;of ELSE
    ) ;of IF
   ) ;of defun

  ;;
  ;;dim옵션을 선택했을 경우
  ;;
  (defun set-dim( / in )
    (mode_tile "dim" 2)
    (setq in (get_tile "dim"))
    (setq #DIM in)
  ) ;of defun SET-GRD

  ;;;
  ;;; ok버튼을 누렀을 때
  ;;;
  (defun do_accept()           ;dialog box를 끝내기 전에 모든 입력 데이타 확인
    (if (and (set_h1) (set_h2) (set_h3) (set_h4) (set_h5) (set_h6)
             (set_b1) (set_b2) (set_b3) (set_b4) (set_b5) (set_b6)
             (set_ys))
      (done_dialog)
    ) ;of IF
  ) ;of defun


  ;;;
  ;;; 잘못된 값이 입력뻍을 때
  ;;;
  (defun do_error(tile value)
    (set_tile "error" "Invalid input")            ;error massage창에 에러표시

    (cond
      ((or (= tile "h1") (= tile "h2") (= tile "h3")
           (= tile "h4") (= tile "h5") (= tile "h6"))
        (progn
          (set_tile  tile value)
          (mode_tile tile 2)
        )
      )
      ((or (= tile "b1") (= tile "b2") (= tile "b3")
           (= tile "b4") (= tile "b5") (= tile "b6") (= tile "ys"))
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
  ;;; MAIN PROGRAM
  ;;;

  (push-env)                                            ;환경변수 대피

  (abut_dialog )                                        ;dialog로 입력받음

  (setq ipnt (getpoint "\nPick Insert point: "))        ;첫점 입력

  (f_abut ipnt #H1 #H2 #H3 #H4 #H5 #H6 #B1 #B2 #B3 #B4 #B5 #B6 #YS #DIM)

  (pop-env)                                             ;환경변수 되돌림

  (princ)

) ;of defun


;***********************************
; Function : F_ABUT
;            ABUT drawing
;            Suk-Jong Yi
;            96/7/22
;***********************************
(defun F_ABUT( IP H1 H2 H3 H4 H5 H6
                  B1 B2 B3 B4 B5 B6 YS DIM
/               )

  (setq ix (car ip)
        iy (cadr ip))

  (princ h1)

  (setq h1 (* h1 ys 1000)  b1 (* b1 1000)
        h2 (* h2 ys 1000)  b2 (* b2 1000)
        h3 (* h3 ys 1000)  b3 (* b3 1000)
        h4 (* h4 ys 1000)  b4 (* b4 1000)
        h5 (* h5 ys 1000)  b5 (* b5 1000)
        h6 (* h6 ys 1000)  b6 (* b6 1000)
        bh (* 100 ys)
        h7 (- (+ h1 h2) (+ h4 h5 h6))
        b7 (- (+ b3 b5) (+ b1 b2))
        ht (+ h1 h2 h3)
        bt (+ b4 b5 b6))

  (setq p1 ip
        p2 (list (+ ix b2) iy)
        p3 (list (car p2) (- iy h4))
        p4 (list (+ ix b2 b7) (cadr p3))
        p5 (list (car p4) (- (cadr p4) h5))
        p6 (list (- (car p5) b3) (- (cadr p5) h6))
        p7 (list (car p6) (- (cadr p6) h7))
        p8 (list (+ (car p7) b6) (cadr p7))
        p9 (list (car p8) (- (cadr p8) h3))
        p10 (list (- (car p9) bt) (cadr p9))
        p11 (list (car p10) (+ (cadr p10) h3))
        p12 (list (+ (car p10) b4) (cadr p11))
        p13 (list (car p12) (+ (cadr p12) h2))
        p14 (list (+ (car p13) b1) (cadr p13))
        bp1 (list (- (car p10) 100) (cadr p10))
        bp2 (list (+ (car p9) 100) (cadr bp1))
        bp3 (list (car bp2) (- (cadr bp2) bh))
        bp4 (list (car bp1) (cadr bp3)))

  (command "LINE" p1 p2 p3 p4 p5 p6 p7 p8 p9 "")
  (command "LINE" bp1 bp2 bp3 bp4 "C")
  (command "LINE" p10 p11 p12 p13 p14 p1 "")

  (if (= DIM "1")
    (progn
      (setq pp (f_dh (list (car p12) (cadr ip)) b1 1 1 nil))
      (setq pp (f_dh p1 b2 1 1 nil))
      (setq pp (f_dh pp b7 1 1 nil))
      (setq pp (f_dh (list (car p6) (cadr p5)) b3 1 -1 nil))
      (setq pp (f_dh p11 b4 1 1 nil))
      (setq pp (f_dh pp b5 1 1 nil))
      (f_dh pp b6 1 1 nil)
      (f_dh p10 bt 1 -1 nil)
      (setq pp (f_dv p9 h3 1 1 nil))
      (setq pp (f_dv pp h7 1 1 nil))
      (setq pp (f_dv pp h6 1 1 nil))
      (setq pp (f_dv pp h5 1 1 nil))
      (f_dv pp h4 1 1 nil)
      (f_dv p9 ht 1 2 nil)
      (setq pp (f_dv p10 h3 1 -1 nil))
      (setq pp (f_dv pp h2 1 -1 nil))
      (f_dv pp h1 1 -1 nil)

    ) ;of progn
  ) ;of if
);of defun F_ABUT

;******************************************
; Program : F_DH
;           Function Dimension Horizontal
;           Jong-Suk Yi
;           96/6/29
;******************************************
; 수평치수선을 함수로 처리해준다.
; 넘어오는 변수
;        SP : 시작점
;       DST : 거리
;         N : 반복갯수
;        UD : Up/DOWN (절대값은 LEVEL)
; 돌려주는 값 - 끝점 좌표
;******************************************

(defun F_DH(SP DST N UD TXT1
/  sp dst n ud txt1
   th       dim_gap     ds      sgn     dy      next    ep      dxy
   dx       txt         divl    divn    txtlen  dtxt1   dtxt2   dtxt1p
   dtxt2p
)

  (setq th (getvar "DIMTXT")                                ;글자 크기는 치수크기
        dim_gap 10.0)                                       ;글자 크기 지정

  (setq ds (getvar "DIMSCALE"))                             ;scale factor

  (if (> ud 0)                                              ;위 아래
    (setq sgn 1)
    (setq sgn -1)
  ) ;of if

  (setq dy (* ds (+ 20 (* dim_gap (- (abs ud) 1)))))        ;치수선 위치 계산 (절대값)

  (setq next (* dst n))                                     ;시작점에서 끝점까지 거리

  (setq ep (list (+ (car sp) next) (cadr sp)))              ;ep 위치계산

  (setq dxy (list (car ep) (+ (cadr ep) (* dy sgn)) 0.0))  ;치수선 위치

  (setq dx (distance sp ep))                          ;거리 계산

  (if (< dx 1000.0)
    (setq txt (rtos dx 2 0))                          ;1000미만일 때
    (setq txt (rtos (* dx 0.001) 2 3))                ;1000이상일 때
  ) ;of if(dx < 1000)

  (if (> n 1)                                           ;골뱅이 옵션일 경우
    (progn
      (setq divl dst)                                   ;나누는 길이 입력
      (setq divn (rtos n 2 0))                          ;나눈 갯수 계산
      (if (< divl 1000.)
        (setq divl (rtos divl 2 0))                   ;1000미만일 때
        (setq divl (rtos (* 0.001 divl) 2 3))) ;of if ;1000이상일 때
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
      (if (= txt1 nil) (setq txt1 txt))                  ;리턴입력시 옛 text를 씀
      (command "DIM1" "HOR" sp ep dxy txt1)             ;DIM명령 내림
    ) ;of progn ELSE
  ) ;of if

  ep
) ;defun


;*********************************
; Function : F_DV
;           Fuction Dimension Vertical
;           Jong-Suk Yi
;           96/7/1
;*********************************

(defun F_DV(SP DST N LR TXT1
/ sp dst n lr txt1
  th        dim_gap     ds      sgn     dx      next    ep
  dxy       dy          txt     divl    divn    txtlen  dtxt1
  dtxt2     dtxt1p      dtxt2p
)

  (setq th (getvar "DIMTXT")                          ;text크기 =dimtxt
        dim_gap 10.0)                                 ;치수선 간격
  (setq ds (getvar "DIMSCALE"))                       ;scale factor

  (if (> lr 0)                                        ;왼쪽/오른쪽
    (setq sgn 1)
    (setq sgn -1)
  ) ;of if

  (setq dx (* ds (+ 20 (* dim_gap (- (abs lr) 1)))))

  (setq next (* dst n))                                 ;끝점까지 거리

  (setq ep (list (car sp) (+ (cadr sp) next)))          ;수정된 끝점

  (setq dxy (list (+ (car ep) (* dx sgn)) (car ep) 0.0))  ;치수선이 놓일 위치

  (setq dy (distance sp ep))                          ;두 점의 거리

  (if (< dy 1000.0)
    (setq txt (rtos dy 2 0))                          ;1000미만일 때
    (setq txt (rtos (* dy 0.001) 2 3))                ;1000이상일 때
  ) ;of if(dy < 1000)

  (if (> n 1)
    (progn
      (setq divl dst)                                   ;나누는 길이 입력
      (setq divn (rtos n 2 0))                          ;나눈 갯수계산
      (if (< divl 1000.)
        (setq divl (rtos divl 2 0))                   ;나누는 길이가 1000미만시
        (setq divl (rtos (* divl 0.001) 2 3))) ;of if           ;나누는 길이가 1000이상시
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
      (if (= txt1 nil) (setq txt1 txt))                    ;리턴입력시 옛 text를 씀
      (command "DIM1" "VER" sp ep dxy txt1)               ;DIM명령 내림
    ) ;of progn ELSE
  ) ;of if
  ep
) ;defun
