;****************************************************
; Program: WELD.LSP
;          WELD mark
;          Yi Suk Jong
;          97/2/29
;****************************************************

(defun C:WELD(/
                p1 p2 dx pt1 pt2 pt3
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
  ;; Function: WELD_DIA (Dialog box로 입력받기)
  ;;
  (defun WELD_DIA (/
        dcl_id
  )
    (setq dcl_id (load_dialog "DJDG.DCL"))                  ;dialog호출
    (if (not (new_dialog "WELD" dcl_id)) (exit))

    (start_image "fu_weld")                                  ;image 보이기
    (slide_image  0 0
                  (dimx_tile "fu_weld") (dimy_tile "fu_weld")
                  "djdg(fuweld)")
    (end_image)

    (start_image "fl_weld")                                  ;image 보이기
    (slide_image  0 0
                  (dimx_tile "fl_weld") (dimy_tile "fl_weld")
                  "djdg(flweld)")
    (end_image)

    (start_image "fb_weld")                                  ;image 보이기
    (slide_image  0 0
                   (dimx_tile "fb_weld") (dimy_tile "fb_weld")
                  "djdg(fbweld)")
    (end_image)

    (start_image "ku_weld")                                  ;image 보이기
    (slide_image  0 0
                  (dimx_tile "ku_weld") (dimy_tile "ku_weld")
                  "djdg(kuweld)")
    (end_image)

    (start_image "kl_weld")                                  ;image 보이기
    (slide_image  0 0
                  (dimx_tile "kl_weld") (dimy_tile "kl_weld")
                  "djdg(klweld)")
    (end_image)

    (start_image "kb_weld")                                  ;image 보이기
    (slide_image  0 0
                  (dimx_tile "kb_weld") (dimy_tile "kb_weld")
                  "djdg(kbweld)")
    (end_image)

    (start_image "vu_weld")                                  ;image 보이기
    (slide_image  0 0
                  (dimx_tile "vu_weld") (dimy_tile "vu_weld")
                  "djdg(vuweld)")
    (end_image)

    (start_image "vl_weld")                                  ;image 보이기
    (slide_image  0 0
                  (dimx_tile "vl_weld") (dimy_tile "vl_weld")
                  "djdg(vlweld)")
    (end_image)

    (start_image "vb_weld")                                  ;image 보이기
    (slide_image  0 0
                  (dimx_tile "vb_weld") (dimy_tile "vb_weld")
                  "djdg(vbweld)")
    (end_image)

;-------------------
; 초기값설정
;-------------------
;   (if (= WSIZE nil) (setq WSIZE 0))
   (setq wsize "0")
   (if (= groot nil) (setq groot "0"))
   (if (= gangle nil) (setq gangle "0"))
   (setq field "0")
   (setq round "0")

;---------------------------
; dialog box 초기화
;---------------------------
   (set_tile "wsize" wsize)
   (set_tile "groot"  groot)
   (set_tile "gangle" gangle)
   (set_tile "field" field)
   (set_tile "round" round)

   (action_tile "fu_weld" "(set_f)(setq wt 1)")
   (action_tile "fl_weld" "(set_f)(setq wt 2)")
   (action_tile "fb_weld" "(set_f)(setq wt 3)")
   (action_tile "ku_weld" "(set_kv)(setq wt 4)")
   (action_tile "kl_weld" "(set_kv)(setq wt 5)")
   (action_tile "kb_weld" "(set_kv)(setq wt 6)")
   (action_tile "vu_weld" "(set_kv)(setq wt 7)")
   (action_tile "vl_weld" "(set_kv)(setq wt 8)")
   (action_tile "vb_weld" "(set_kv)(setq wt 9)")
   (action_tile "wsize"   "(setq WSIZE $VALUE)")
   (action_tile "groot"   "(setq groot $VALUE)")
   (action_tile "gangle"  "(setq gangle $VALUE)")
   (action_tile "round"   "(setq ROUND $VALUE)")
   (action_tile "field"   "(setq FIELD $VALUE)")
   (action_tile "accept"  "(done_dialog)")
   (action_tile "cancel"  "(exit)")
;   (mode_tile "fl_weld" 2)
    (start_dialog)
    (unload_dialog dcl_id)
  ) ;of defun SPLICE_DIALOG

  (defun set_f()    ;필렛용접일때 루트간격, 그루브각도 unable
    (mode_tile "groot" 1)
    (mode_tile "gangle" 1)
  ) ;of defun

  (defun set_kv()    ;k,v용접일때 루트간격, 그루브각도 enable
    (mode_tile "groot" 0)
    (mode_tile "gangle" 0)
  ) ;of defun


  ;;;-------------------------
  ;;; MAIN ROUTINE
  ;;;-------------------------

  (setq oer *error* *error* seterr)             ;에러발생시

  (push-env)                                    ;환경변수대피

  (setq ds (getvar "DIMSCALE"))

  (setq p1 (getpoint "\nPick first base line point: "))
  (setq p2 (getpoint p1 "\nPick second base line point: "))

  (setq dx (- (car p2) (car p1)))                       ;x방향

  (if (< dx 0)
    (setq p2 (list (- (car p1) (* 12 ds)) (cadr p1)))   ;기선의 길이=12mm
    (setq p2 (list (+ (car p1) (* 12 ds)) (cadr p1)))
  ) ;of if


  (weld_dia)

  (setvar "CECOLOR" "RED")                      ;빨간색으로 바꿈
  (command "LINE" p1 p2 "")                     ;기선

  (if (= round "1")                             ;온둘레용접
    (command "CIRCLE" p1 (* 1.5 ds))
  ) ;of if

  (if (= field "1")                             ;현장용접인경우
    (progn
      (setq pt1 (list (car p1) (+ (cadr p1) (* 5.0 ds))))
      (setq pt2 (list (+ (car p1) (* ds 2.0)) (- (cadr pt1) (* 1.0 ds))))
      (setq pt3 (list (car pt1) (- (cadr pt1) (* 1.0 ds))))
      (command "LINE" p1 pt1 "")
      (command "SOLID" pt1 pt2 pt3 "" "")
      (command "CIRCLE" p1 (* 1.5 ds))
    ) ;of progn
  ) ;of if

  (cond                                         ;용접기호그리기
    ((= wt 1) (fweld p1 p2 wsize  1) (wlead p1))
    ((= wt 2) (fweld p1 p2 wsize -1) (wlead p1))
    ((= wt 3)
      (fweld p1 p2   "0"  1)
      (fweld p1 p2 wsize -1)
      (wlead p1))
    ((= wt 4) (kweld p1 p2 wsize groot gangle  1) (wlead p1))
    ((= wt 5) (kweld p1 p2 wsize groot gangle -1) (wlead p1))
    ((= wt 6)
      (kweld p1 p2 "0"   "0"      "0"  1)
      (kweld p1 p2 wsize groot gangle -1)
      (wlead p1))
    ((= wt 7) (vweld p1 p2 wsize groot gangle  1) (wlead p1))
    ((= wt 8) (vweld p1 p2 wsize groot gangle -1) (wlead p1))
    ((= wt 9)
      (vweld p1 p2   "0"  "0"  "0"     1)
      (vweld p1 p2 wsize groot gangle -1)
      (wlead p1))
  ) ;of cond

  (setvar "CECOLOR" "BYLAYER")
  (pop-env)

) ;of defun

;-----------------------------
; function : FWELD
;            fillet weld
;-----------------------------
; P1   : 시작점
; P2   : 끝  점
; SIZE : weld size
; UL   : Upper / Lower (1/-1)
;-----------------------------

(defun fweld( P1 P2 SIZE UL /
              P1 P2 SIZE UL b mx pt1 pt2 pt3 tp)

  (setq b (* 3 ds))                         ;욕접기호 폭 3mm
  (if (< (car p1) (car p2))
    (setq mx (+ (car p1) (* 7 ds)))
    (setq mx (+ (car p2) (* 7 ds)))
  ) ;of if
  (setq pt1 (list (- mx (* 0.5 b)) (cadr p1))
        pt2 (list (car pt1) (+ (cadr p1) (* b ul)))
        pt3 (list (+ mx (* 0.5 b)) (cadr p1))
        tp  (list (- mx (* 4.0 ds)) (+ (cadr p1) (* ul 2.5 ds))))

  (command "LINE" pt1 pt2 pt3 "")
  (setvar "CECOLOR" "BYLAYER")
  (if (/= size "0")
    (command "TEXT" "M" tp (* 2.5 ds) "0.0" size)
  ) ;
  (setvar "CECOLOR" "RED")
) ;of defun

;-----------------------------
; function : KWELD
;            K weld
;-----------------------------
; P1   : 시작점
; P2   : 끝  점
; SIZE : weld size
; UL   : Upper / Lower (1/-1)
;-----------------------------

(defun kweld( P1 P2 SIZE ROOT ANG UL /
              P1 P2 SIZE ROOT ANG UL b mx pt1 pt2 pt3 pt rp ap)

  (setq b (* 3 ds))
  (if (< (car p1) (car p2))
    (setq mx (+ (car p1) (* 7 ds)))
    (setq mx (+ (car p2) (* 7 ds)))
  ) ;of if
  (setq pt1 (list (- mx (* 0.5 b)) (cadr p1))
        pt2 (list (car pt1) (+ (cadr p1) (* b ul)))
        pt3 (list (+ mx (* 0.5 b)) (cadr pt2))
        tp  (list (- mx (* 4.0 ds)) (+ (cadr p1) (* ul 2.5 ds)))  ; size위치
        rp  (list mx (+ (cadr p1) (* ul 3.5 ds))))   ;root간격위치

  (if (= root "0")
    (setq ap (list mx (+ (cadr p1) (* ul 3.5 ds))))  ;groove각 위치
    (setq ap (list mx (+ (cadr rp) (* ul 3.5 ds))))
  ) ;of if

  (command "LINE" pt2 pt1 pt3 "")
  (setvar "CECOLOR" "BYLAYER")
  (if (/= size "0")
    (command "TEXT" "M" tp (* 2.5 ds) "0.0" size)
  ) ; ofif
  (if (/= root "0")
    (command "TEXT" "M" rp (* 2.5 ds) "0.0" root)
  )
  (if (/= ang "0")
    (command "TEXT" "M" ap (* 2.5 ds) "0.0" (strcat ang "%%d"))
  )

  (setvar "CECOLOR" "RED")

) of defun

;-----------------------------
; function : VWELD
;            V weld
;-----------------------------
; P1   : 시작점
; P2   : 끝  점
; SIZE : weld size
; UL   : Upper / Lower (1/-1)
;-----------------------------

(defun vweld( P1 P2 SIZE root ang UL)

  (setq b (* 3 ds))
  (setq mx (* 0.5 (+ (car p1) (car p2)))
        pt1 (list mx (cadr p1))
        pt2 (polar pt1 (* 4 (/ pi 6)) (* b ul))
        pt3 (polar pt1 (* 2 (/ pi 6)) (* b ul))
        tp  (list (- mx (* 4.0 ds)) (+ (cadr p1) (* ul 2.5 ds)))    ;치수위치
        rp  (list mx (+ (cadr p1) (* ul 3.5 ds))))              ;root간격위치

  (if (= root "0")
    (setq ap (list mx (+ (cadr p1) (* ul 3.5 ds))))  ;groove각 위치
    (setq ap (list mx (+ (cadr rp) (* ul 3.5 ds))))
  ) ;of if

  (command "LINE" pt2 pt1 pt3 "")
  (setvar "CECOLOR" "BYLAYER")
  (if (/= size "0")
    (command "TEXT" "M" tp (* 2.5 ds) "0.0" size)
  ) ; ofif
  (if (/= root "0")
    (command "TEXT" "M" rp (* 2.5 ds) "0.0" root)
  )
  (if (/= ang "0")
    (command "TEXT" "M" ap (* 2.5 ds) "0.0" (strcat ang "%%d"))
  )
  (setvar "CECOLOR" "RED")

) of defun


;------------------
; function : WLEAD
;------------------
(defun WLEAD( P1 /
              P1 pt1 pt2
            )

  (setq pt1 (getpoint P1 "\nPick first point: "))
  (command "LINE" P1 pt1 "")

  (setq pt2 (getpoint pt1 "\nPick second point: "))
  (if (= pt2 nil)
    (arrow pt1 p1)                          ;화살표그림
    (progn
      (command "LINE" P1 pt1 pt2 "")
      (arrow pt2 pt1)
    ) ;of progn
  ) ;of if

  (setq pt1 (getpoint P1 "\nPick first point: "))
  (if (/= pt1 nil)
    (progn
      (command "LINE" P1 pt1 "")
      (setq pt2 (getpoint pt1 "\nPick second point: "))
      (if (= pt2 nil)
        (arrow pt1 p1)
        (progn
          (command "LINE" P1 pt1 pt2 "")
          (arrow pt2 pt1)
        ) ;of progn
      ) ;of if
    ) ;of progn
  ) ;of if
) ;of defun


;-----------------------------------
; Function : ARROW
;-----------------------------------
; ARROW( P1 P2 )
;        P1      : 화살표 끝점
;           P2   : 방향 선택점
;-----------------------------------

(defun ARROW( P1 P2 / P1 P2 pt1 pt2 )
  (setq ang (angle p1 p2)                                   ;두점이 이루는 각
        w4  (which4 ang))                                   ;몇사분면인가?
  (if (or (= w4 1) (= w4 4))                                ;1,4분면일경우
    (setq ys (* 0.8 ds))                                      ;y-scale =  1
    (setq ys (* -0.8 ds))                                     ;y-scale = -1
  ) ;of if

  (setq pt1 (polar p1  ang (* 3 ds))
        pt2 (polar pt1 (+ ang (* 0.5 pi)) ys))

  (command "SOLID" p1 pt1 pt2 "" "")

) ;of defun

