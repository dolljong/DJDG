;**********************************************
; Program : DDSCL
;           Dynamic Dialog box SCaLe setting
;           Suk-Jong Yi
;           1995. 3. 19,3.28, 7/21
;**********************************************
;도면 초기화 작업
;**********************************************

(defun C:DDSCL(/
               ds   lmin
)

  ;;;
  ;;;내장 에러루틴 정의
  ;;;
  (defun SETERR(s)
  ;If an error (CTRL-C) occurs when this command is active.
    (if (/= s "Function cancelled")
      (if (= s "quit / exit abort")
        (princ "*cancel*")
        (princ (strcat "\nError " s))
      ) ;of if
    ); of If
  ;Restore previous error handler
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)                 ;에러핸들러 지정

  (push-env)                                        ;환경변수 대피
  (setq #BDR "border0")                             ;전역변수 초기화
  (setq #GRD "0")

  (scl-dialog)                                      ;Dialog Box 실행

  (setq ds (list_scl #SCLF))                              ;popup입력을 scale값으로

  (setq lmin (list (* 790 ds) (* 565 ds)))    ;도면 한계값지정
  (command "LIMITS" "0,0" lmin)
  (command "ZOOM" "A")                        ;zoom all
  (setvar "LTSCALE" (* ds 10.0))              ;ltscale조정

  (setvar "DIMSCALE" ds)                        ;dimscale 조정
  (setvar "TEXTSIZE" (* 2.5 ds))                ;text크기 2.5mm

  (if (= #grd "1") (command "GRID" "1000"))         ;#GRD가 "1"이면 Grid ON
  (if (or (= #BDR "border0") (= #BDR "border"))     ;도곽 Xref
    (command "XREF" "" (strcat (prefix) "Blocks/" #BDR) "0,0" ds "" "")
  ) ;of IF


;  (setq *error* oer seterr nil)         ;에러핸들러를 이전의 것으로 다시 지정

  (princ)

) ;of defun DDSCL

;;
;; dialog box를 이용한 입력
;;
(defun SCL-DIALOG(/
                  dcl_id
)

  (setq dcl_id (load_dialog "DJDG"))                 ;ddscl.dcl load
  (if (not (new_dialog "scl" dcl_id)) (exit))         ;ddscl.dcl 안의 scl지정

  (set_tile "scl_f" "6") (setq #SCLF "6")
;  (setq #USERSCL "100")

  (action_tile "scl_f" "(set-sclf)")                  ;scale facter list box
  (action_tile "user_scl" "(set-user)")               ;user 입력 box
  (action_tile "border0" "(setq #BDR \"border0\")")   ;border0 선택 radio_btn
  (action_tile "border1" "(setq #BDR \"border\")")    ;border0 선택 radio_btn
  (action_tile "no_border" "(setq #BDR \"nobdr\")")   ;border없음   radio_btn
  (action_tile "grd" "(set-grd)")                     ;grid ON/OFF toggle
;  (action_tile "accept" "(do-accept)")                ;OK button
  (action_tile "accept" "(done_dialog)")                ;OK button
  (action_tile "cancel" "(do-cancel)")                ;CALCEL button
  (start_dialog)
  (unload_dialog dcl_id)
  (princ)
) ;of defun SCL-DIALOG

;;
;; POPUP LIST에 입력이 들어왔을 경우
;;
(defun set-sclf(/ in)
  (setq in (get_tile "scl_f"))
  (setq #SCLF in)
  (if (= in "12")
    (mode_tile "user_scl" 2)
    (set_tile "user_scl" (itoa (list_scl #SCLF)))
  ) ;of IF
) ;of defun

;;
;; popup_list 값을 scale 값으로..
;;
(defun list_scl(lst / lst)
  (cond
    ((= lst "0") 10)
    ((= lst "1") 20)
    ((= lst "2") 25)
    ((= lst "3") 30)
    ((= lst "4") 40)
    ((= lst "5") 50)
    ((= lst "6") 100)
    ((= lst "7") 200)
    ((= lst "8") 250)
    ((= lst "9") 300)
    ((= lst "10") 400)
    ((= lst "11") 500)
    ((= lst "12") (atof #USERSCL))
    (T nil)
  ) ;of COND
) ;of defun LIST_SCL

;;
;; User scale factor
;;
(defun set-user( / in )
  (set_tile "scl_f" "12")
  (setq #SCLF "12")
  (setq in (get_tile "user_scl"))
;  (if (<= (atoi in) 0)
  (if (<= (atof in) 0)
    (progn
      (do-error)
      (mode_tile "user_scl" 2)
      nil
    ) ;of THEN
    (progn
      (setq #USERSCL in)
      (set_tile "error" "")
      T
    ) ;of ELSE
  ) ;of IF
) ;of defun SET-USER



;;
;;grid옵션을 선택했을 경우
;;
(defun set-grd( / in )
  (mode_tile "grd" 2)
  (setq in (get_tile "grd"))
  (setq #GRD in)
) ;of defun SET-GRD

;;
;;ok를 선택했을 경우
;;
(defun do-accept()
  (if (set-sclf) (done_dialog))
) ;of defun DO-ACCEPT

;;
;;error 발생시 수행되는 함수 (call back 함수 아님)
;;
(defun do-error()
  (set_tile "error" "Invalid SCALE factor.")
  (mode_tile "scl_f" 2)
) ;of DO-ERROR

;;
;;cancel을 선택했을 경우
;;
(defun do-cancel()
  (done_dialog)
  (exit)
) ;of DO-CALCEL
