;******************************
; Program : RWALL
;           ReWALL drawing
;           Suk-Jong Yi
;           98/10/12
;******************************
; 옹벽 그리기
;******************************

(defun C:DDRWALL( /
  ds sc x1 n1 p1 x2 n2 p3 a1 a2 a3 a4 b1 b2 ipnt ix iy
  l1 h1 l2 lx ly pt1 pt2 pt3 pt4 pt5 pt6 pt7 pt8 pt9 pt10 count dpt
  pt11 pt12 pt13 pt14 sbx sby pt15 pt16 cnt scl2 pp vgap mx fpt1 fpt2
  mp ss wp1 wp2
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

;    (setq *error* oer seterr nil)

    (princ)
  ); of SETERR

  ;;
  ;; Function: RW_DIALOG (Dialog box로 입력받기)
  ;;
  (defun RW_DIALOG (/
        dcl_id
  )
    (setq dcl_id (load_dialog "DJDG"))                  ;dialog호출
    (if (not (new_dialog "RWALL" dcl_id)) (exit))

    (start_image "rwall")                                  ;image 보이기
    (slide_image
                 0 0
                 (dimx_tile "rwall") (dimy_tile "rwall")
                 "djdg(ddrwall)"
    )
    (end_image)

    (if (= #B1 nil) (setq #B1 0.8))
    (if (= #B2 nil) (setq #B2 0.7))                      ;초기치 설정
    (if (= #B3 nil) (setq #B3 0.3))
    (if (= #B4 nil) (setq #B4 2.8))
    (if (= #B5 nil) (setq #B5 0.3))
    (if (= #H1 nil) (setq #H1 6.3))
    (if (= #H2 nil) (setq #H2 0.2))
    (if (= #H3 nil) (setq #H3 0.5))
    (if (= #FSLOP nil) (setq #FSLOP "1"))
    (if (= #DIM nil) (setq #DIM "1"))
    (if (= #YSCALE nil) (setq #YSCALE 1.00))


    (set_tile "b1" (rtos #B1 2 3))
    (set_tile "b2" (rtos #B2 2 3))                  ;초기치로 edit box setting
    (set_tile "b3" (rtos #B3 2 3))
    (set_tile "b4" (rtos #B4 2 3))
    (set_tile "b5" (rtos #B5 2 3))
    (set_tile "h1" (rtos #H1 2 3))
    (set_tile "h2" (rtos #H2 2 3))
    (set_tile "h3" (rtos #H3 2 3))
    (set_tile "fslop" #FSLOP)
    (set_tile "dim" #DIM)
    (set_tile "yscale" (rtos #YSCALE 2 3))
    (set_tile "totalb" (strcat "총  폭: " (rtos (+ #b1 #b2 #b3 #b4) 2 3)))
    (set_tile "totalh" (strcat "총높이: " (rtos (+ #h1 #h2 #h3) 2 3)))


    (action_tile "b1"     "(set_val $key)")
    (action_tile "b2"     "(set_val $key)")               ;dialog box Action
    (action_tile "b3"     "(set_val $key)")
    (action_tile "b4"     "(set_val $key)")
    (action_tile "b5"     "(set_val $key)")
    (action_tile "h1"     "(set_val $key)")
    (action_tile "h2"     "(set_val $key)")
    (action_tile "h3"     "(set_val $key)")
    (action_tile "fslop"  "(set_val $key)")
    (action_tile "yscale" "(set_val $key)")
    (action_tile "dim"    "(set_val $key)")
    (action_tile "dfile"  "(set_dfile)")
    (action_tile "accept" "(do_accept)")
    (action_tile "cancel" "(do_cancel)")
    (mode_tile "b1" 2)
    (start_dialog)
    (unload_dialog dcl_id)
  ) ;of defun rw_dialog
  
  

  ;;;
  ;;; dialog box에서 값 입력받아 변수에 저장
  ;;;
  (defun SET_VAL (key / in value)              ;edit_box에 입력이 들어왔을 때
    (setq in (get_tile key))
    (cond
      ((= key "b1")
        (setq value (atof in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #B1 value)
            (set_tile "error" "")
            (set_tile "totalb" (strcat "     총  폭: " (rtos (+ #B1 #B2 #B3 #B4) 2 3)))
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=b1
      ((= key "b2")
        (setq value (atof in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #B2 value)
            (set_tile "error" "")
            (set_tile "totalb" (strcat "     총  폭: " (rtos (+ #B1 #B2 #B3 #B4) 2 3)))
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=b2
      ((= key "b3")
        (setq value (atof in))
        (setq #B3 value)
        (set_tile "error" "")
        (set_tile "totalb" (strcat "     총  폭: " (rtos (+ #B1 #B2 #B3 #B4) 2 3)))
        T
      ) ;of key=b3
      ((= key "b4")
        (setq value (atof in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #B4 value)
            (set_tile "error" "")
            (set_tile "totalb" (strcat "     총  폭: " (rtos (+ #B1 #B2 #B3 #B4) 2 3)))
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=b4
      ((= key "b5")
        (setq value (atof in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #B5 value)
            (set_tile "error" "")
;            (set_tile "totalb" (strcat "     총높이: " (rtos (+ #B1 #B2 #B3 #B4) 2 3)))
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=b5
      ((= key "h1")
        (setq value (atof in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #H1 value)
            (set_tile "error" "")
            (set_tile "totalh" (strcat "     총높이: " (rtos (+ #H1 #H2 #H3) 2 3)))
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=h1
      ((= key "h2")
        (setq value (atof in))
        (setq #H2 value)
        (set_tile "error" "")
        (set_tile "totalh" (strcat "     총높이: " (rtos (+ #H1 #H2 #H3) 2 3)))
        T
      ) ;of key=h2
      ((= key "h3")
        (setq value (atof in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #H3 value)
            (set_tile "error" "")
            (set_tile "totalh" (strcat "총높이: " (rtos (+ #H1 #H2 #H3) 2 3)))
            T
          ) ;of ELSE
        ) ;of if
      ) ;of key=h3
      ((= key "yscale")
        (setq value (atof in))
        (if (= value 0)
          (progn
            (do_error key in)
            nil
          ) ;of THEN
          (progn
            (setq #YSCALE value)
            (set_tile "error" "")
;            (if (/= #TSCALE 1.0) (set_tile "dim 1))
            T
          ) ;of ELSE
        ) ;of if
      ) ;sub cond
     ((= key "dim")
       (mode_tile "dim" 2)
       (setq in (get_tile "dim"))
       (setq #DIM in)
     ) ;of subcond
     ((= key "fslop")
       (mode_tile "fslop" 2)
       (setq in (get_tile "fslop"))
       (setq #FSLOP in)
     ) ;of subcond
    ) ;of cond
  ) ;of defun

  

  (defun set_dfile()
    (if (= #DFILE nil)                                  ;파일명이 nil이면 입력받음
      (setq #DFILE (getfiled "Data file" (getvar "DWGPREFIX") "" 2))
      (setq #DFILE (getfiled "Data file" (getpath #DFILE)     "" 2))
    )
    (if  (/= #DFILE nil)
      (progn
        (set_tile "path_name" #DFILE)
        (setq opf (open #DFILE "r"))                          ;file open
        (if opf
          (progn                                          ;file이 있는 경우
            (setq title (read-line opf))                  ;첫줄(타이틀)을 읽는다
            (setq ch (read-line opf))                     ;둘째줄(B)을 읽는다
            (setq inline (data-in ch))                    ;읽은 한줄을 ,기준으로 나눔
            (setq #B1 (atof (nth 0 inline))
                  #B2 (atof (nth 1 inline))
                  #B3 (atof (nth 2 inline))
                  #B4 (atof (nth 3 inline))
                  #B5 (atof (nth 4 inline)))
            (setq ch (read-line opf))                     ;둘째줄(B)을 읽는다
            (setq inline (data-in ch))                    ;읽은 한줄을 ,기준으로 나눔
            (setq #H1 (atof (nth 0 inline))
                  #H2 (atof (nth 1 inline))
                  #H3 (atof (nth 2 inline)))
            (close opf)                                           ;file close
          ) ;of progn
          (princ "\nFile not found")                          ;file이 없는 경우
        ) ;of if
        (set_tile "b1" (rtos #B1 2 3))
        (set_tile "b2" (rtos #B2 2 3))                  ;현재 값으로 edit box setting
        (set_tile "b3" (rtos #B3 2 3))
        (set_tile "b4" (rtos #B4 2 3))
        (set_tile "b5" (rtos #B5 2 3))
        (set_tile "h1" (rtos #H1 2 3))
        (set_tile "h2" (rtos #H2 2 3))
        (set_tile "h3" (rtos #H3 2 3))
      );progn
    );if
  );of defun

  

  ;;;
  ;;; ok버튼을 누렀을 때
  ;;;
  (defun do_accept()           ;dialog box를 끝내기 전에 모든 입력 데이타 확인
    (if (and (set_val "b1") (set_val "h1")
             (set_val "b2") (set_val "h2")
             (set_val "b3") (set_val "h3")
             (set_val "b4")
             (set_val "b5")
             (set_val "yscale"))
      (done_dialog)
    ) ;of IF
  ) ;of defun


  ;;;
  ;;; 잘못된 값이 입력뻍을 때
  ;;;
  (defun do_error(tile value)
    (set_tile "error" "Invalid input")            ;error massage창에 에러표시

    (if (or (= tile "b1") (= tile "b2") (= tile "b3")
            (= tile "b4") (= tile "b5") (= tile "h1")
            (= tile "h2") (= tile "h3")
            (= tile "yscale"))
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

  ;(push-env)                            ;환경변수 보관

  (setq ds (getvar "DIMSCALE"))         ;스케일값잡아내기

  (rw_dialog)                           ;dialog박스로 입력받기

 (setq p (getpoint "\ninsert point: ")) ;삽입점 입력 받음

 (f_rwall p
         #H1
         #H2
         #H3
         #B1
         #B2
         #B3
         #B4
         #B5
         (if (= #FSLOP "1") 0.02 0)    ;전면 1:0.02 slop여부
         #YSCALE                        ;yscale
         #DIM)                          ;dimension표기 여부

  (princ "O.K!")

  (pop-env)

) ;of defun


;---------------------------------
; PROGRAM : RWALL
;           draw ReWALL
;           Yi Suk jong
;           98/10/13
;---------------------------------
; 다이얼로그 박스나
; 해석프로그램 데이타 파일을 이용하여
; 옹벽단면도를 그려줌
;---------------------------------

(defun c:RWALL(
/ answ fn opf title ch inline B1 B2 B3 B4 H1 H2 H3)
;  (setq llist nil)                                      ;빈 line-list 만듬

  (initget "File Entity")
  (setq answ (getkword "\nFile/Entity <File>: "))

  (if (or (= answ nil) (= answ "File"))                 ;return입력이나 F입력시
    (progn
      (setq fn (getfiled "INPUT DATA" "" "DAT" 0))      ;file name입력
      (setq opf (open fn "r"))                          ;file open
      (if opf
        (progn                                          ;file이 있는 경우
          (setq title (read-line opf))                  ;첫줄(타이틀)을 읽는다
          (setq ch (read-line opf))                     ;둘째줄(B)을 읽는다
          (setq inline (data-in ch))                    ;읽은 한줄을 ,기준으로 나눔
          (setq B1 (atof (nth 0 inline))
                B2 (atof (nth 1 inline))
                B3 (atof (nth 2 inline))
                B4 (atof (nth 3 inline))
                B5 (atof (nth 4 inline)))
          (setq ch (read-line opf))                     ;둘째줄(B)을 읽는다
          (setq inline (data-in ch))                    ;읽은 한줄을 ,기준으로 나눔
          (setq H1 (atof (nth 0 inline))
                H2 (atof (nth 1 inline))
                H3 (atof (nth 2 inline)))
          (close opf)                                           ;file close
        ) ;of progn
        (princ "\nFile not found")                          ;file이 없는 경우
      ) ;of if
    ) ;of progn THEN
    (progn                                                  ;Entity로 입력할 경우
      (setq tent (ssget '((0 . "TEXT"))))                   ;text만 select
      (setq tn    (sslength tent)                           ;text갯수
            count               0)                          ;첫 text부터
      (repeat tn                                            ;text갯수만큼 반복
        (setq ch (cdr (assoc 1 (entget (ssname tent count)))))  ;text축출
        (princ (chr 13))                                        ;작업중메세지
        (princ (1+ count))
        (princ " Line Processing...")
        (setq inline (data-in ch))
        (setq lst (list                                     ;문자 data를 숫자 data로
                     (strcase (car (sp-trunc (nth 0 inline))))   ;철근번호
                     (strcase (car (sp-trunc (nth 1 inline))))   ;철근직경
                     (atof (nth 2 inline))                       ;철근길이
                     (atoi (nth 3 inline))))                     ;철근갯수
        (setq llist (append llist (list lst)))                   ;llist에 추가
        (setq count (1+ count))                                  ;다음 text로
      ) ;of repeat
    ) ;of progn ELSE
  ) ;of IF

  (setq FSLOP (getreal "\n벽체전면 slop <0.02>: "))           ;벽체전면 slop
  (if (= FSLOP nil) (setq FSLOP 0.02))

  (setq YS (getreal "\nY방향 SCALE <1.0>: "))                 ;Y방향 SCALE
  (if (= YS nil) (setq YS 1.0))

  (initget "Yes No")
  (setq DIM (getkword "\n치수선을 기입하시겠습니까? <Y/n>: ")) ;치수선기입여부
  (if (or (= DIM nil) (= DIM "Yes")) (setq DIM "1") (setq DIM "0"))

 (setq p (getpoint "\ninsert point: "))             ;삽입점(벽체전면상단)

 (f_rwall p H1 H2 H3 B1 B2 B3 B4 B5 FSLOP YS DIM)   ;옹벽그리기 함수 콜

);defun

;-------------------
; test용 프로그램
;-------------------

(defun c:rw()
 (setq p (getpoint "\ninsert point: "))
 (f_rwall p
         6.3 ;H1
         0.2 ;H2
         0.5 ;H3
         0.8 ;B1
         0.7 ;B2
         0.0 ;B3
         3.1 ;B4
         0.3 ;B5
         0.02 ;FSLOP
         1.0
         "1")
) ;defun

;***********************************
; Function : F_RWALL
;            RWALL drawing
;            Suk-Jong Yi
;            98/10/12
;***********************************
(defun F_RWALL( IP H1 H2 H3
                   B1 B2 B3 B4 B5 FSLOP YS DIM
/               )

  (setq ix (car ip)
        iy (cadr ip))

  (setq h1 (* h1 ys 1000)  b1 (* b1 1000)
        h2 (* h2 ys 1000)  b2 (* b2 1000)
        h3 (* h3 ys 1000)  b3 (* b3 1000)
        bh (* 100 ys)      b4 (* b4 1000)
                           b5 (* b5 1000)
        ht (+ h1 h2 h3)
        bt (+ b1 b2 b3 b4))

  (setq p1 ip                                           ;벽체전면상단
        p2 (list (- ix (/ (* h1 fslop) ys)) (- iy h1))         ;벽체전면하단
        p3 (list (- (car p2) b1)     (- (cadr p2) h2))  ;앞굽전면상단
        p4 (list (car p3)            (- (cadr p3) h3))  ;앞굽전면하단
        p5 (list (+ (car p3) bt)     (cadr p4))         ;뒷굽후면하단
        p6 (list (car p5)            (cadr p3))         ;뒷굽후면상단
        p7 (list (- (car p6) b4)     (cadr p2))         ;헌치하단
        p8 (list (- (car p7) b3)     (+ (cadr p7) b3))  ;헌치상단
        p9 (list (+ ix b5)           iy)                ;벽체배면상단

        bp1 (list (- (car p4) 100) (cadr p4))           ;기초앞굽상단
        bp2 (list (car bp1) (- (cadr bp1) bh))          ;기초앞굽하단
        bp3 (list (+ (car p5) 100) (cadr bp2))          ;기초뒷굽상단
        bp4 (list (car bp3) (+ (cadr bp3) bh))          ;기초뒷굽하단
  ) ;setq

  (command "LINE" p1 p2 p3 p4 p5 p6 p7 p8 p9 ip "")     ;옹벽본체
  (command "LINE" p4 bp1 bp2 bp3 bp4 p5 "")             ;기초콘크리트

  ;;;;; 치수선 처리
  (if (= DIM "1")
    (progn
      (if (> Fslop 0)                                    ;벽체전면기울기선
        (setq pp (f_dh (list (car p2) (cadr ip))
                       (- (car p1) (car p2)) 1 1 nil))
        (setq pp ip)
      );if
      (setq pp (f_dh pp b5 1 1 nil))                     ;벽체상단 두께
      (setq pp (f_dh pp (- (car p8) (car p9)) 1 1 nil))  ;벽체배면 기울기두께

      (setq pp (f_dh (list (car p3) (cadr p2)) b1 1 1 nil)) ;앞굽
      (setq pp (f_dh pp b2 1 1 nil))                        ;벽체두께
      (if (> b3 0)                                          ;헌치
        (setq pp (f_dh pp b3 1 1 nil))
      );if
      (setq pp (f_dh pp b4 1 1 nil))                         ;뒷굽

      (f_dh p4 bt 1 -1 nil)                                  ;기초전체

      (setq pp (f_dv p4 h3 1 -1 nil))                        ;앞굽판두께
      (if (> h2 0)                                           ;앞굽경사높이
        (setq pp (f_dv pp h2 1 -1 nil))
      );if
      (setq pp (f_dv pp h1 1 -1 nil))                        ;벽체높이
      (setq pp (f_dv p4 ht 1 -2 nil))                        ;벽체전체높이

 ;     (setq pp (f_dv pp h5 1 1 nil))
 ;     (f_dv pp h4 1 1 nil)
 ;     (f_dv p9 ht 1 2 nil)
 ;     (setq pp (f_dv p10 h3 1 -1 nil))
 ;     (setq pp (f_dv pp h2 1 -1 nil))
 ;     (f_dv pp h1 1 -1 nil)
 ;
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

  (setq th (getvar "DIMTXT")
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

;*******************************************************************
;     Function : DATA-IN
;                DATA file IN
;                Jong-Suk Yi
;                1995. 2. 8
;******************************************************************
; 이 함수는 ,로 불리된 data를 나누어 한개의 list에 묶어준다.
; 이때 형변환 없이 모든 data는 문자열로 return된다.
;******************************************************************

(defun DATA-IN(arg1
/             srt    strl    count    num    strt    nchr    subs
              lst    rslt
)
;(setq oer *error* *error* seterr)   ;Store AutoLISP error routine
   (setq str arg1)                              ;넘어온 문자열
   (setq strl (strlen arg1))                    ;넘어온 문자열의 길이
   (setq count 1)
   (setq num 1)
   (setq strt 1)                                ;추출시작 위치
   (setq nchr 1)                                ;추출문자 갯수
   (repeat (+ strl 1)
      (setq subs (substr str count 1))          ;문자 한개
      (if (or (= subs ",") (= subs ""))         ;현재 문자가 ,이거나 끝일때
         (progn
            (setq lst (substr str strt (- nchr 1)))    ;시작위치부터
            (if (= rslt nil)
               (setq rslt (list lst))                  ;돌림값이 비었을때
               (setq rslt (append rslt (list lst)))    ;돌림값에다 추가
            ) ;of if
            (setq nchr 0)                       ;추출갯수 다시 0으로
            (setq strt (1+ count))              ;다음 추출시작을 다음문자로
         ) ;of progn
         nil
      ) ;of if
      (setq count (1+ count))                   ;다음 문자로
      (setq num (1+ num))                       ;
      (setq nchr (1+ nchr))                     ;문자 갯수 한개 증가
   ) ;of repeat
   (setq arg1 rslt)                             ;돌림값 돌림
;(setq *error* oer seterr nil)                  ; Restore previous error handler
) ;of defun STRLOC


;**************************************************************************
; Function : SP-TRUNC
;            SPace TRUNCation
;            By Suk-Jong Yi
;            1995/6/1
;**************************************************************************
; 입력문자열의 앞,뒤에 있는 빈칸을 짤라낸다.
; 리턴값은
; (짤라낸 문자열,
;  첫 문자 나오는 위치,
;  마지막 문자 나오는 위치,
;  숫자인가?)
;***************************************************************************

(defun SP-TRUNC(txt
/               txtl        frntn       backn       txt1
)

(setq txtl (strlen txt))
(setq frntn 1)
(while (= (substr txt frntn 1) " ") (setq frntn (+ frntn 1)))
(if (<= frntn txtl)
  (progn
    (setq backn txtl)
    (while (= (substr txt backn 1) " ")
     (setq backn (- backn 1))
    )
    (setq txt1 (substr txt frntn (- backn frntn -1)))
    (list txt1 frntn backn (is-num txt1))
  ) ;progn
) ;of if

);of defun


;**************************************************************************
; Function : GETPATH
;            GET PATH
;            By Suk-Jong Yi
;            1999/3/6
;**************************************************************************
; 어떤 Fullpath filename에서 path만 축출해낸다.
; 넘어오는값  : fullpath file name
; 리턴값은    : path명
;***************************************************************************

(defun GETPATH(fpath / num_c count )
  (setq num_c (strlen fpath))                       ;글자수
  (setq count num_c)                                ;맨마지박 글자부터
  (while (and (> count 1)
              (/= (substr fpath count 1) "\\"))
    (setq count (1- count))
  ); while

  (substr fpath 1 count)

);defun
