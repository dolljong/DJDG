;**********************************
; Program : SPLICE
;           SPLICE draw
;           By Jong-Suk Yi
;           1995/7/19
;**********************************

(defun C:SPLICE (/                ;Main program
                ipnt    2pnt    ang
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
  ;; Function: SPLICE_DIALOG (Dialog box로 입력받기)
  ;;
  (defun SPLICE_DIALOG (/
                       dcl_id
  )
    (setq dcl_id (load_dialog "DJDG"))                ;dialog호출
    (if (not (new_dialog "SPLICE" dcl_id)) (exit))

    (start_image "dd_splice")                           ;image 보이기
    (slide_image
                 0 0
                 (dimx_tile "dd_splice") (dimy_tile "dd_splice")
                 "djdg(ddsplice)"
    )
    (end_image)

    (if (= #N1 nil) (setq #N1 2))                       ;초기치 설정
    (if (= #P1 nil) (setq #P1 85))
    (if (= #E1 nil) (setq #E1 60))
    (if (= #N2 nil) (setq #N2 2))
    (if (= #P2 nil) (setq #P2 85))
    (if (= #E2 nil) (setq #E2 60))
    (if (= #G nil) (setq  #G 100))

    (set_tile "n1" (itoa #N1))                          ;초기치로 edit box setting
    (set_tile "p1" (rtos #P1 2 1))
    (set_tile "e1" (rtos #E1 2 1))
    (set_tile "n2" (itoa #N2))
    (set_tile "p2" (rtos #P2 2 1))
    (set_tile "e2" (rtos #E2 2 1))
    (set_tile  "g" (rtos #G 2 1))
    (set_tile "total1" (rtos (+ #G (* #N1 #P1) (* 2 #E1)) 2 1))
    (set_tile "total2" (rtos (+ (* #N2 #P2) (* 2 #E2)) 2 1))
    (set_tile "dist" (rtos #DST 2 1))

    (action_tile "n1" "(set_n1)")                       ;dialog box Action
    (action_tile "p1" "(set_p1)")
    (action_tile "e1" "(set_e1)")
    (action_tile "n2" "(set_n2)")
    (action_tile "p2" "(set_p2)")
    (action_tile "e2" "(set_e2)")
    (action_tile "g" "(set_g)")
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
        (set_tile "total1" (rtos (+ #G (* #n1 #p1) (* 2 #E1)) 2 1))
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
        (set_tile "total1" (rtos (+ #G (* #n1 #p1) (* 2 #E1)) 2 1))
        T
      ) ;of ELSE
    ) ;of IF
   ) ;of defun

  ;;
  ;; FUNCTION : SET_E1 (e1값 입력)
  ;;
  (defun SET_E1(/ in value)              ;e1 edit_box에 입력이 들어왔을 때
    (setq in (get_tile "e1"))
    (setq value (atoi in))
    (if (= vlaue 0)
      (progn
        (do_error "e1" in)
        nil
      ) ;of THEN
      (progn
        (setq #E1 value)
        (set_tile "error" "")
        (set_tile "total1" (rtos (+ #G (* #n1 #p1) (* 2 #E1)) 2 1))
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
        (set_tile "total2" (rtos (+ (* #n2 #p2) (* 2 #E2)) 2 1))
        T
      ) ;of ELSE
    ) ;of IF
   ) ;of defun

  ;;
  ;; FUNCTION : SET_P2 (P2값 입력)
  ;;
  (defun SET_P2(/ n vlaue)              ;p2 edit_box에 입력이 들어왔을 때
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
        (set_tile "total2" (rtos (+ (* #n2 #p2) (* 2 #E2)) 2 1))
        T
      ) ;of ELSE
    ) ;of IF
   ) ;of defun

  ;;
  ;; FUNCTION : SET_E2 (e2값 입력)
  ;;
  (defun SET_E2(/ n value)              ;e2 edit_box에 입력이 들어왔을 때
    (setq in (get_tile "e2"))
    (setq value (atoi in))
    (if (= value 0)
      (progn
        (do_error "e2" in)
        nil
      ) ;of THEN
      (progn
        (setq #E2 value)
        (set_tile "error" "")
        (set_tile "total2" (rtos (+ (* #n2 #p2) (* 2 #E2)) 2 1))
        T
      ) ;of ELSE
    ) ;of IF
   ) ;of defun

  ;;
  ;; FUNCTION : SET_G (g값 입력)
  ;;
  (defun SET_G(/ n value)              ;g  edit_box에 입력이 들어왔을 때
    (setq in (get_tile "g"))
    (setq value (atoi in))
    (if (= value 0)
      (progn
        (do_error "g" in)
        nil
      ) ;of THEN
      (progn
        (setq #G value)
        (set_tile "error" "")
        (set_tile "total1" (rtos (+ #G (* #n1 #p1) (* 2 #E1)) 2 1))
        T
      ) ;of ELSE
    ) ;of IF
   ) ;of defun

  ;;;
  ;;; ok버튼을 누렀을 때
  ;;;
  (defun do_accept()           ;dialog box를 끝내기 전에 모든 입력 데이타 확인
    (if (and (set_n1) (set_p1) (set_e1) (set_n2) (set_p2) (set_e2))
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
      ((or (= tile "p1") (= tile "e1") (= tile "p2") (= tile "e2") (= tile "g"))
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
  ;;; splice그리기
  ;;;
  (defun draw_splice ( ipnt ang num1 pitch1 end1 num2 pitch2 end2 gap /
                     ipnt     ang     num1    pitch1
                     end1     num2    pitch2  end2    gap
  )

    (setq tot1 (+ (/ gap 2.0) (* num1 pitch1) end1)  ;splice 판 전체 길이 구함
          tot2 (+ (* num2 pitch2) (* end2 2.0))
    )

    (setq count_lr 0)                                ;양쪽 대칭 구조물 flag

    (repeat 2                                           ;왼쪽 오른쪽 두번 그림

      (setq ang90 (+ ang (* pi 0.5) (* count_lr -1 pi)))   ;왼쪽,오른쪽 각 계산

      (setq pur (polar ipnt ang90 tot1)                    ;splice판
            plr (polar pur  ang   tot2)
            pll (polar  ipnt ang   tot2)
      )
      (command "LINE" ipnt pur plr pll "")            ;splice판 그리기

      (setq oldc (getvar "CECOLOR"))                  ;현재색 대피
      (setvar "CECOLOR" "GREEN")                      ;녹색으로 setting

      (setq count1 0)                                 ;bolt선 그리기
      (repeat (1+ num1)                                 ;수직선
        (setq bolt_pnt (polar (polar ipnt ang90 (+ (* gap 0.5) (* count1 pitch1)))
                              ang end2)
        )
        (command "LINE" bolt_pnt (polar bolt_pnt ang (* num2 pitch2)) "")
        (setq count1 (1+ count1))
      ) ;of repeat

      (setq count2 0)
      (repeat (1+ num2)                                 ;수평선
        (setq bolt_pnt (polar (polar ipnt ang (+ end2 (* count2 pitch2)))
                              ang90 (* 0.5 gap))
        )
        (command "LINE" bolt_pnt (polar bolt_pnt ang90 (* num1 pitch1)) "")
        (setq count2 (1+ count2))
      ) ;of repeat

      (setvar "CECOLOR" oldc)                           ;색깔복귀


      (setq count1 0)                                ;; 볼트구멍 그리기

      (repeat (+ num1 1)
        (setq bolt_pnt (polar (polar ipnt ang90 (+ (* gap 0.5) (* count1 pitch1)))
                        ang end2)
        )
        (setq count2 0)
        (repeat (+ num2 1)
          (command "CIRCLE"                               ;원그리기
                   (polar bolt_pnt ang (* count2 pitch2))   ;원 중심
                   "11"                                     ;원 반지름
          )
          (setq count2 (1+ count2))
        ) ;of repeat                                 ;수직으로 반복
        (setq count1 (1+ count1))
      ) ;of repeat                                   ;수평으로 반복
      (setq count_lr (1+ count_lr))
    ) ;of repeat 2                                   ;왼쪽, 오른쪽 반복
  ) ;of defun function draw_splice


  ;;
  ;; MAIN ROUTINE
  ;;

  (setq oer *error* *error* seterr)

  (push-env)                                          ;환경변수 대피

  (graphscr)
  (setq ipnt (getpoint "\nPick first point: "))       ;첫점 입력
  (setq 2pnt (getpoint ipnt "\npick second point: ")) ;둘째점 입력

  (setq ang (angle ipnt 2pnt)                         ;두점의 각 구함
        #DST (distance ipnt 2pnt)                     ;두점의 거리 구함
  )

  (splice_dialog )                                    ;dialog로 입력받음

  (setq ipnt (polar (mid-point ipnt 2pnt) (+ ang pi)  ;splice시작점 구함
                    (* 0.5 (+ (* #N2 #P2) (* 2 #E2))))

  )

  (draw_splice IPNT ANG #N1 #P1 #E1 #N2 #P2 #E2 #G)   ;splice그리기 함수 call

  (pop-env)                                           ;환경변수 복귀

  (setq *error* oer seterr nil)

  (princ)

) ;of defun



