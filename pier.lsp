;************************************
; Program : PIER
;           draw PIER
;           Suk-Jong Yi
;           96/7/18
;************************************
; 교각 일반도 그리기
;************************************

(defun C:PIER(/
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
        (if (not (new_dialog "PIER" dcl_id)) (exit))

        (start_image "dd_pier")                           ;image 보이기
        (princ (dimx_tile "dd_pier"))
        (princ (dimy_tile "dd_pier"))
        (slide_image
                     0 0
                     (dimx_tile "dd_pier") (dimy_tile "dd_pier")
                     "djdg(ddpier)"
        )
        (end_image)

        (if (= #BC nil) (setq #BC 2.5))                       ;초기치 설정
        (if (= #C1 nil) (setq #C1 2.5))                       ;초기치 설정
        (if (= #C2 nil) (setq #C2 1.5))
        (if (= #PD nil) (setq #PD 2.5))
        (if (= #PL nil) (setq #PL 5.0))
        (if (= #FB nil) (setq #FB 8.0))
        (if (= #FH nil) (setq #FH 2.0))
        (if (= #YS nil) (setq #YS 1))
        (if (= #SCOPING nil) (setq #SCOPING 3))
        (if (= #RCOPING nil) (setq #RCOPING "0"))

        (set_tile "bc"  (rtos #BC 2 3))                      ;초기치로 edit box setting
        (set_tile "c1"  (rtos #C1 2 3))                      ;초기치로 edit box setting
        (set_tile "c2"  (rtos #C2 2 3))
        (set_tile "pd"  (rtos #PD 2 3))
        (set_tile "pl"  (rtos #PL 2 3))
        (set_tile "fb"  (rtos #FB 2 3))
        (set_tile "fh"  (rtos #FH 2 3))
        (set_tile "ys"  (rtos #YS 2 3))
        (set_tile "rcoping" #RCOPING)
        (set_tile "scoping"  (rtos #SCOPING 2 3))

        (action_tile "bc" "(set_bc)")                       ;dialog box Action
        (action_tile "c1" "(set_c1)")                       ;dialog box Action
        (action_tile "c2" "(set_c2)")
        (action_tile "pd" "(set_pd)")
        (action_tile "pl" "(set_pl)")
        (action_tile "fb" "(set_fb)")
        (action_tile "fh" "(set_fh)")
        (action_tile "ys" "(set_ys)")
        (action_tile "rcoping" "(set_rcoping)")
        (action_tile "scoping" "(set_scoping)")
        (action_tile "dim" "(set_dim)")
        (action_tile "accept" "(do_accept)")
        (action_tile "cancel" "(do_cancel)")
        (mode_tile "c1" 2)
        (if (= #RCOPING "0")
          (mode_tile "scoping" 1)
          (mode_tile "scoping" 0)
        ) ;of if
        (start_dialog)
        (unload_dialog dcl_id)
      ) ;of defun SPLICE_DIALOG

      ;;
      ;; FUNCTION : SET_C1 (c1값 입력)
      ;;
      (defun SET_C1 (/ in value)              ;n1 edit_box에 입력이 들어왔을 때
        (setq in (get_tile "c1"))
        (setq value (atof in))                ;string을 값으로
        (if (= value 0)                            ;0이 입력되면
          (progn
            (do_error "c1" in)
            nil
          ) ;of THEN
          (progn
            (setq #C1 value)
            (set_tile "error" "")
            T
          ) ;of ELSE
        ) ;of IF
       ) ;of defun

      ;;
      ;; FUNCTION : SET_BC (bc값 입력)
      ;;
      (defun SET_BC (/ in value)              ;bc edit_box에 입력이 들어왔을 때
        (setq in (get_tile "bc"))
        (setq value (atof in))                ;string을 값으로
        (if (= value 0)                            ;0이 입력되면
          (progn
            (do_error "bc" in)
            nil
          ) ;of THEN
          (progn
            (setq #BC value)
            (set_tile "error" "")
            T
          ) ;of ELSE
        ) ;of IF
       ) ;of defun

      ;;
      ;; FUNCTION : SET_C2 (c2값 입력)
      ;;
      (defun SET_C2(/ in value)              ;p1 edit_box에 입력이 들어왔을 때
        (setq in (get_tile "c2"))
        (setq value (atof in))              ;string을 값으로
        (if (= value 0)                     ;값이 0인 경우
          (progn
            (do_error "c2" in)
            nil
          ) ;of THEN
          (progn
            (setq #C2 value)
            (set_tile "error" "")
            T
          ) ;of ELSE
        ) ;of IF
       ) ;of defun

      ;;
      ;; FUNCTION : SET_E1 (e1값 입력)
      ;;
      (defun SET_PD(/ in value)              ;e1 edit_box에 입력이 들어왔을 때
        (setq in (get_tile "pd"))
        (setq value (atof in))
        (if (= vlaue 0)
          (progn
            (do_error "pd" in)
            nil
          ) ;of THEN
          (progn
            (setq #PD value)
            (set_tile "error" "")
            T
          ) ;of ELSE
        ) ;of IF
       ) ;of defun

      ;;
      ;; FUNCTION : SET_PL2 (pl값 입력)
      ;;
      (defun SET_PL(/ in value)              ;n2 edit_box에 입력이 들어왔을 때
        (setq in (get_tile "pl"))
        (setq value (atof in))
        (if (= value 0)
          (progn
            (do_error "pl" in)
            nil
          ) ;of THEN
          (progn
            (setq #PL value)
            (set_tile "error" "")
            T
          ) ;of ELSE
        ) ;of IF
       ) ;of defun

      ;;
      ;; FUNCTION : SET_FB (FB값 입력)
      ;;
      (defun SET_FB(/ n vlaue)              ;p2 edit_box에 입력이 들어왔을 때
        (setq in (get_tile "fb"))
        (setq value (atof in))
        (if (= value 0)
          (progn
            (do_error "fb" in)
            nil
          ) ;of THEN
          (progn
            (setq #FB value)
            (set_tile "error" "")
            T
          ) ;of ELSE
        ) ;of IF
       ) ;of defun

      ;;
      ;; FUNCTION : SET_FH (e2값 입력)
      ;;
      (defun SET_FH(/ n value)              ;e2 edit_box에 입력이 들어왔을 때
        (setq in (get_tile "fh"))
        (setq value (atof in))
        (if (= value 0)
          (progn
            (do_error "fh" in)
            nil
          ) ;of THEN
          (progn
            (setq #FH value)
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
      ;; FUNCTION : SET_SCOPING (scoping값 입력)
      ;;
      (defun SET_SCOPING(/ n value)              ;scoping edit_box에 입력이 들어왔을 때
        (setq in (get_tile "scoping"))
        (setq value (atof in))
        (if (= value 0)
          (progn
            (do_error "scoping" in)
            nil
          ) ;of THEN
          (progn
            (setq #SCOPING value)
            (set_tile "error" "")
            T
          ) ;of ELSE
        ) ;of IF
       ) ;of defun

      ;;
      ;;round coping옵션을 선택했을 경우
      ;;
      (defun set_rcoping( / in )
        (setq in (get_tile "rcoping"))
        (if (= in "1")
          (mode_tile "scoping" 0)
          (mode_tile "scoping" 1)
        ) ;of if
        (setq #RCOPING in)
      ) ;of defun SET-RCOPING

      ;;
      ;;dim옵션을 선택했을 경우
      ;;
      (defun set_dim( / in )
        (mode_tile "dim" 2)
        (setq in (get_tile "dim"))
        (setq #DIM in)
      ) ;of defun SET-DIM

      ;;;
      ;;; ok버튼을 누렀을 때
      ;;;
      (defun do_accept()           ;dialog box를 끝내기 전에 모든 입력 데이타 확인
        (if (and (set_c1) (set_c2) (set_pd) (set_pl) (set_fb) (set_fh))
          (done_dialog)
        ) ;of IF
      ) ;of defun


      ;;;
      ;;; 잘못된 값이 입력됬을 때
      ;;;
      (defun do_error(tile value)
        (set_tile "error" "Invalid input")            ;error massage창에 에러표시
        (cond
          ((or (= tile "c1") (= tile "c2"))
            (progn
              (set_tile  tile value)
              (mode_tile tile 2)
            )
          )
          ((or (= tile "pd") (= tile "pl") (= tile "fb") (= tile "fh"))
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


      ;;
      ;; MAIN ROUTINE
      ;;

      (setq oer *error* *error* seterr)                 ;내장error루틴 가동

      (push-env)                                        ;환경변수 대피

      (setq ds (getvar "DIMSCALE"))                     ;스케일값

      (graphscr)

      (splice_dialog )                                    ;dialog로 입력받음

      (setq ipnt (getpoint "\nPick first point: "))       ;첫점 입력
      (setq ix (car ipnt)                                 ;삽입점 x좌표
            iy (cadr ipnt))                               ;삽입점 y좌표

      (pier_side ipnt #BC #C1 #C2 #PD #PL #FB #FH #YS #SCOPING)            ;pier그리기

      (pop-env)                                           ;환경변수 복귀

      (setq *error* oer seterr nil)

      (princ)

) ;of defun



;************************************
; Function : PIER_SIDE
;            draw PIER SIDE view
;            Suk-Jong Yi
;            96/7/18
;************************************
; pier의 측면도 그리기
; IP : Insert Point
; C1 : Coping 중앙의 높이
; C2 : Coping 끝단의 높이
; PD : POST DIA
; PL : POST LENGTH
; FB : Footing 폭
; FH : Footing 높이
; YS : Y-Scale
;************************************

(defun PIER_SIDE(IP BC C1 C2 PD PL FB FH YS SCOPING
/ ip bc c1 c2 pd pl fb fh ys scoping
)

  (setq BC (* BC 1000)
        C1 (* C1 YS 1000)
        C2 (* C2 YS 1000)
        PD (* PD 1000)
        PL (* PL YS 1000)
        FB (* FB 1000)
        FH (* FH YS 1000)
        BH (* 100 YS)              ;base Con'c 두께
        BE  100                    ;base Con'c 여유폭
        SCOPING (* SCOPING YS))

  (if (>= bc pd)                    ;코핑폭과 포스트지름중 큰값을 상면폭으로
    (setq bt bc)
    (setq bt pd)
  ) ;of if
  (command "LINE" (list (- (car ip) (/ bt 2.0)) (cadr ip))
                  (list (+ (car ip) (/ bt 2.0)) (cadr ip)) "")

  (if (= #RCOPING "1") (copingr ip BC c1 c2 SCOPING)  ;coping 그리기
                       (coping  ip BC c1 c2))
  (if (>= bc pd)
    (post (list ix (- iy c1)) pd pl)                  ;post 그리기
    (progn                                            ;post가 코핑보다 넓을 때
      ; 기둥선그리기
      (command "LINE" (list (- ix (/ pd 2.0)) iy)               ;기둥좌측선
                      (list (- ix (/ pd 2.0)) (- iy c1 pl)) "")
      (command "LINE" (list (+ ix (/ pd 2.0)) iy)               ;기둥우측선
                      (list (+ ix (/ pd 2.0)) (- iy c1 pl)) "")
      ;기둥중심선
      (setvar "CECOLOR" "RED")
      (setvar "CELTYPE" "CENTER")
      (command "LINE" (list ix (- iy c1))
                      (list ix (- iy c1 pl)) "")
      (setvar "CECOLOR" "BYLAYER")
      (setvar "CELTYPE" "BYLAYER")
      ;기둥라운드표현
      (setvar "CECOLOR" "GREEN")
      (setq xsgn 1)
      (repeat 2                                     ;좌우로 두번 실행
        (setq xsgn (* xsgn -1)
              dx   (/ pd 2.0)                       ;post의 반지름
              x    ix)
        (while (>= dx (* ds 0.5))
          (setq dx (/ dx 2.0)
                x (+ x (* dx xsgn)))
          (if (>= (abs (- ix x)) (/ bc 2.0))
            (setq y iy)
            (setq y (- iy c1))
          ) ;of if
          (setq lp1 (list x y)
                lp2 (list (car lp1) (- iy c1 pl)))
          (command "LINE" lp1 lp2 "")
        ) ;of while
      ) ;of repeat
      (setvar "CECOLOR" "BYLAYER")
    ) ;of progn
  ) ;of if
  ; 푸팅그리기
  (footing (list ix (- iy c1 pl)) FB FH BH BE)      ;footing 그리기

) ;of defun


;------------------------------------------------------
; Function : COPING
;            draw COPING
;            Yi Suk Jong
;            97/2/11
;---------------------------------------------------------
; COPING(IP BC H1 H2)
;        IP            : insert point
;           BC         : coping 폭
;              H1      : 가운데 높이
;                 H2   : 끝 높이
;---------------------------------------------------------
(defun COPING(IP BC H1 H2
              / IP BC H1 H2 ix iy bc2 p1 p2 p3 p4 oldc dy count y
)

  (setq ix (car ip)
        iy (cadr ip)
        bc2 (/ bc 2.0)                          ; 코핑폭의 반
        p1 (list (- ix bc2) iy)                ;상단좌측
        p2 (list (car p1) (- iy h1))           ;하단좌측
        p3 (list (+ ix bc2) (cadr p2))         ;하단우측
        p4 (list (car p3) iy))                 ;상단우측

  (command "LINE" p1 p2 p3 p4 "")

  (command "LINE" (list (car p1) (- iy h2))      ;코핑끝단선
                  (list (car p4) (- iy h2)) "")

  (setq oldc (getvar "CECOLOR"))
  (setvar "CECOLOR" "GREEN")
  (setq dy (/ (- h1 h2) 6)
        count 1)

  (repeat 5
    (setq y (- iy h2 (* count dy)))
    (command "LINE"
             (list (- ix bc2) y)
             (list (+ ix bc2) y) "")
    (setq count (1+ count))
  ) ;of repeat
  (setvar "CECOLOR" oldc)

) ;of defun

;------------------------------------------------------
; Function : COPINGR
;            draw COPING Round
;            Yi Suk Jong
;            97/2/11
;---------------------------------------------------------
; COPINGR(IP BC H1 H2 S)
;        IP            : insert point
;           BC         : coping 폭
;              H1      : 가운데 높이
;                 H2   : 끝 높이
;                    S : 코핑경사(L/H)
;---------------------------------------------------------
(defun COPINGR(IP BC H1 H2 S
             / IP BC H1 H2 S ix iy bc2 p1 p2 p3 p4 h3 ap1 ap2
               ap3 aent xsgn dx x lp1 lp11 lp2 dy count y
)

  (setq ix (car ip)
        iy (cadr ip)
        bc2 (/ bc 2.0)                          ;코핑폭의 반=라운드 R
        p1 (list (- ix bc2) iy)                 ;상단좌측
        p2 (list (car p1) (- iy h1))            ;하단좌측
        p3 (list (+ ix bc2) (cadr p2))          ;하단우측
        p4 (list (car p3) iy)                   ;상단우측
        h3 (/ bc2 s))                           ;코핑끝단 ~ round끝단

  (command "LINE" p1 p2 p3 p4 "")               ;코핑4각형

  (setq ap1 (list (+ ix bc2) (- iy h2 h3))      ;Arc점 1
        ap2 (list ix (- iy h2))                 ;Arc점 2
        ap3 (list (- ix bc2) (cadr ap1)))       ;Arc점 3
  (command "ARC" ap1 ap2 ap3)
  (setq aent (entget (entlast)))                ;Arc 정보

  (setvar "CELTYPE" "CENTER")
  (setvar "CECOLOR" "RED")
  (command "LINE" ip ap2 "")
  (setvar "CELTYPE" "BYLAYER")
  (setvar "CECOLOR" "BYLAYER")

  ;--------------------
  ; 코핑의 라운드 표현
  ;--------------------
  (setvar "CECOLOR" "GREEN")
  (setq xsgn 1)
  (repeat 2                                     ;좌우로 두번
    (setq xsgn (* xsgn -1)
          dx   bc2
          x    ix)
    (while (>= dx (* ds 0.5))
      (setq dx (/ dx 2.0)
            lp1 (list (+ x (* dx xsgn)) iy)
            lp11 (list (car lp1) (- (cadr lp1) 100.0))
            lp2 (car (cross aent lp1 lp11)))
      (setq x (car lp1))
      (command "LINE" lp1 lp2 "")
    ) ;of while
  ) ;of repeat

  ;------------------------------
  ; coping slop면 표현
  ;------------------------------

  (setq dy (/ (- h1 h2) 6)
        count 1)

  (repeat 5
    (setq y (- iy h2 (* count dy)))
    (if (> y (cadr ap1))
      (setq p1 (car (cross aent (list (car ap3) y) (list (car ap1) y)))
            p2 (cadr (cross aent (list (car ap3) y) (list (car ap1) y))))
      (setq p1 (list (- ix bc2) y)
            p2 (list (+ ix bc2) y))
    ) ;of if
    (command "LINE" p1 p2 "")
    (setq count (1+ count))
  ) ;of repeat
  (setvar "CECOLOR" "BYLAYER")

) ;of defun


;************************************
; Function : PIER_FRONT
;            draw PIER FRONT view
;            Suk-Jong Yi
;            96/7/18
;************************************
; pier의 정면도 그리기
; IP : Insert Point
; CL : Coping 길이
; C1 : Coping 중앙의 높이
; C2 : Coping 끝단의 높이
; PD : POST DIA
; PL : POST LENGTH
; FB : Footing 폭
; FH : Footing 높이
;************************************
(defun PIER_FRONT(IP CL C1 C2 PD PL FB FH
                / IP CL C1 C2 PD PL FB FH
                  ix iy cl2 pd2 )

  (setq ix (car ip)
        iy (cadr ip)
        cl2 (/ cl 2)
        pd2 (/ pd 2))

  (command "LINE"
           (list (- ix cl2) iy)
           (list (+ ix cl2) iy)
           (list (+ ix cl2) (- iy c2))
           (list (+ ix pd2) (- iy c1))
           (list (- ix pd2) (- iy c1))
           (list (- ix cl2) (- iy c2))
           "C")

  (post (list ix (- iy c1)) pd pl)   ;post 그리기
  (footing (list ix (- iy c1 pl)) FB FH BH BE)   ;footing 그리기

) ;of defun

;*********************************
; Function : FOOTING
;            draw FOOTING
;            Suk-Jong Yi
;            96/7/19
;*********************************
; footing을 그려준다
; IP : Insert Point
; B  : 폭
; H  : 높이
; BH : Base con'c 높이
; BE : Base con'c 폭
;*********************************
(defun FOOTING(IP B H BH BE
             / IP B H BH BE
               ix iy b2 y1 y2 )

  (setq ix (car ip)
        iy (cadr ip)
        b2 (/ b 2)
        y1 (- iy h)
        y2 (- iy h BH))

  (command "LINE"
           (list (- ix b2) y1)
           (list (- ix b2) iy)
           (list (+ ix b2) iy)
           (list (+ ix b2) y1) "")

  (command "LINE"
           (list (- ix b2 BE) y1)
           (list (+ ix b2 BE) y1)
           (list (+ ix b2 BE) y2)
           (list (- ix b2 BE) y2)
           "C")
) ;of defun

;***********************************
; Function : POST
;            draw POST
;            Suk-Jong Yi
;            96/7/18
;***********************************
(defun POST(IP PD PL
          / IP PD PL
            ix iy pd2 p1s p1e p2s p2e p3s p3e oldlt oldc)

  (setq ix (car ip)
        iy (cadr ip)
        pd2 (/ pd 2))

  (setq p1s (list (- ix pd2) iy)
        p1e (list (- ix pd2) (- iy pl))
        p2s (list (+ ix pd2) iy)
        p2e (list (+ ix pd2) (- iy pl))
        p3s (list ix iy)
        p3e (list ix (- iy pl)))

  (command "LINE" p1s p1e "")
  (command "LINE" p2s p2e "")
  (setq oldlt (getvar "CELTYPE"))
  (setq oldc (getvar "CECOLOR"))
  (setvar "CELTYPE" "CENTER")
  (setvar "CECOLOR" "RED")
  (command "LINE" p3s p3e "")
  (setvar "CELTYPE" oldlt)
  (setvar "CECOLOR" oldc)

  (hround p3s p3e p1s p1e)
  (hround p3s p3e p2s p2e)

  (princ "POST")
) ;of defun

;**************************************
; Function : HROUND
;            Half ROUNDing
;            Suk-Jong Yi
;            1995. 3. 16
;**************************************

(defun HROUND(p1s p1e p2s p2e /             ;반 라운딩을 해주는 Function
sdst edst lpnts lpnte ds p1s p1e oldclr     ;Argument는 두선의 시작점과 끝점
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
(setvar "CECOLOR" "GREEN")
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


;-----------------------------------------
; function : CROSS
;            CROSS point of line & circle
;-----------------------------------------
; CROSS(aent lent)
;       aent             : Arc entity정보
;            lent        : Line entity정보
;-----------------------------------------------
(defun CROSS(aent line_p1 line_p2 /
             a b r sa ea x1 x2 y1 y2 dx dy a1 b1 c1 ang1 ang2 crsp
)

  (setq a (car (cdr (assoc 10 aent))))      ;arc 원점의 x좌표
  (setq b (cadr (cdr (assoc 10 aent))))     ;arc 원점의 y좌표
  (setq r (cdr (assoc 40 aent)))            ;arc의 r
  (setq sa (cdr (assoc 50 aent)))           ;arc의 시작각
  (setq ea (cdr (assoc 51 aent)))           ;arc의 끝각

  (setq x1 (car line_p1)                    ;line의 첫점 x좌표
        x2 (car line_p2)                    ;line의 끝점 x좌표
        y1 (cadr line_p1)                   ;line의 첫점 y좌표
        y2 (cadr line_p2))                  ;line의 끝점 y좌표

  (setq dx (- x1 x2)                        ;x차
        dy (- y1 y2))                       ;y차

  (cond
    ((= dx 0)                               ;line이 y축에 평행한 경우
      (setq a1 1.0
            b1 (* -1.0 2.0 b)
            c1 (+ (- (expt (- x1 a) 2) (expt r 2)) (expt b 2))
            y1 (/ (+ (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1))
            y2 (/ (- (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1)))
    ) ;of dx=0
    ((= dy 0)                               ;line의 x축에 평행한경우
      (setq a1 1.0
            b1 (* -1.0 2.0 a)
            c1 (+ (- (expt (- y1 b) 2) (expt r 2)) (expt a 2))
            x1 (/ (+ (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1))
            x2 (/ (- (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1)))
    ) ;of dy=0
    ( T                                     ;line이 임의의 기울기를 가진 경우
      (setq c (/ (- y1 y2) (- x1 x2)))
      (setq d (- y2 (* c x2)))

      (setq a1 (+ 1 (* c c)))
      (setq b1 (+ (* 2 d c) (* -2 a) (* -2 b c)))
      (setq c1 (+ (* a a) (* b b) (* d d) (* -2 b d) (* -1 r r)))

      (setq x1 (/ (+ (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1)))
      (setq x2 (/ (- (* -1 b1) (sqrt (- (* b1 b1) (* 4 a1 c1)))) (* 2 a1)))
      (setq y1 (+ (* c x1) d))
      (setq y2 (+ (* c x2) d))
    ) ;of T
  ) ;of cond

  (setq ang1 (angle (list a b 0.0) (list x1 y1 0.0)))
  (setq ang2 (angle (list a b 0.0) (list x2 y2 0.0)))

  (setq crsp nil)
  (if (inang sa ea ang1)
    (setq crsp (append crsp (list (list x1 y1 0.0)))))

  (if (inang sa ea ang2)
    (setq crsp (append crsp (list (list x2 y2 0.0)))))

  crsp

) ;of defun

(defun inang(a1 a2 a3 /
  a1 a2 a3)

  (if (> a1 a2) (setq a2 (+ (* 2.0 pi) a2)))
  (if (and (>= a3 a1) (<= a3 a2))
    (- a2 a1)
    nil)

) ;of defun

