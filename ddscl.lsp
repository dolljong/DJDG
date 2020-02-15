;**********************************************  
; Program : DDSCL  
;           Dynamic Dialog box SCaLe setting  
;           Suk-Jong Yi  
;           1995. 3. 19,3.28, 7/21  
;**********************************************  
;도면 초기화
;**********************************************  
  
(defun C:DDSCL(/  
               ds   lmin  
)  
  
  ;;;  
  ;;;내장 에러루鉀 몹의  
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
  
  (setq oer *error* *error* seterr)                 ;에러핸들러 빨몹  
  
  (push-env)                                        ;錤경변수 대區  
  (setq #BDR nil)
  ;  (setq #BDR "border")                             ;멨역변수 점기譏  
  (setq LFAC (getvar "dimlfac"))
  (cond
    ((= LFAC 1000) (setq #GRD "1"))
    (T  (setq #GRD "0"))
  );cond  
;  (setq #GRD "0")  
  
  (scl-dialog)                                      ;Dialog Box   행  
  
  (setq ds #USERSCL)                              ;popup럼력을 scale값으로  

  (if (= #GRD "1")				; 1unit = 1m인 경우
    (progn
      (setq ds (/ #USERSCL 1000))
      (setvar "DIMLFAC" 1000)
    );progn
    (setvar "DIMLFAC" 1)
  );if
  
  (setq lmin (list (* 790 ds) (* 565 ds)))    ;도면 한계값빨몹  
  (command "LIMITS" "0,0" lmin)  
  (command "ZOOM" "A")                        ;zoom all  
  (setvar "LTSCALE" (* ds 10.0))              ;ltscale묀몹  
  
  (setvar "DIMSCALE" ds)                        ;dimscale 묀몹  
  (setvar "TEXTSIZE" (* 2.5 ds))                ;text크기 2.5mm  
  
;  (if (= #grd "1") (command "GRID" "1000"))         ;#GRD가 "1"래면 Grid ON  
  
  (if (/= #BDR nil)     ;도곽 Xref  
    (if (= #XREF 1)  
      (command "XREF" ""  #BDR "0,0" ds "" "")  
      (command "INSERT" #BDR "0,0" ds "" "")  
    ) ;of if  
  ) ;of IF  
  
  (if (= (tblsearch "LTYPE" "HIDDEN") nil)         ;만랭 Hidden type 댜으면 load  
    (command "LINETYPE" "L" "HIDDEN" "ACAD" "")  
  ) ;of if  
  
  (if (= (tblsearch "LTYPE" "CENTER") nil)         ;만랭 Hidden type 댜으면 load  
    (command "LINETYPE" "L" "CENTER" "ACAD" "")  
  ) ;of if  
  
  
  (setq *error* oer seterr nil)         ;에러핸들러를 래멨의 것으로 다   빨몹  
  
  (princ)  
  
) ;of defun DDSCL  
  
;;  
;; dialog box를 래용한 럼력  
;;  
(defun SCL-DIALOG(/  
                  dcl_id )  
  
  (setq dcl_id (load_dialog "DJDG"))                 ;ddscl.dcl load  
  (if (not (new_dialog "scl" dcl_id)) (exit))         ;ddscl.dcl 안의 scl빨몹  
  
  
  ;(set_tile "user_scl" (rtos 100 2 0))
  (if (= 1000 (getvar "dimlfac"))    ; 1unit = 1m인 경우
    (set_tile "grd" "1")
    (set_tile "grd" "0")
  );if
  
  (set_tile "user_scl" (rtos (* (getvar "dimscale") (getvar "dimlfac")) 2 0))

;  (set_tile "grd" #GRD)
  
  (action_tile "user_scl" "(set-user)")               ;user 럼력 box  
  (action_tile "x_border" "(getfile 1)")                ;border у택 radio_btn  
  (action_tile "b_border" "(getfile 0)")                ;border у택 radio_btn  
  (action_tile "n_border" "(no_bdr)")   ;border댜음   radio_btn  
  (action_tile "grd" "(set-grd)")                     ;grid ON/OFF toggle  
  (action_tile "accept" "(do-accept)")                ;OK button  
;  (action_tile "accept" "(done_dialog)")                ;OK button  
  (action_tile "cancel" "(do-cancel)")                ;CALCEL button  
  (mode_tile "user_scl" 2)  
  (start_dialog)  
  (unload_dialog dcl_id)  
  (princ)  
) ;of defun SCL-DIALOG  
  
  
;;  
;; User scale factor  
;;  
(defun set-user( / in )  
;  (set_tile "scl_f" "12")  
;  (setq #SCLF "12")  
  (setq in (get_tile "user_scl"))  
  (if (<= (atof in) 0)  
    (progn  
      (do-error)  
      (mode_tile "user_scl" 2)  
      nil  
    ) ;of THEN  
    (progn  
      (setq #USERSCL (atof in))  
      (set_tile "error" "")  
      T  
    ) ;of ELSE  
  ) ;of IF
) ;of defun SET-USER  
  
  
(defun getfile(XB / bn)  
  (if (= xb 1) (setq #XREF 1) (setq #XREF 0))  
  (setq #BDR (getfiled "BORDER FILE" (strcat (prefix) "blocks\\border") "DWG" 8))  
  (if (= #BDR nil) (setq #BDR (strcat (prefix) "BLOCKS\\BORDER")))  
  (setq bn (strcase (substr (bdr_name #BDR) 1 6)))             ;Boder name  
  (if (/= bn "BORDER") (alert "Can't use ALLPLOT"))  
  (set_tile "bdr_file" #BDR)  
) ;of defun  
  
(defun no_bdr( )  
  (setq #BDR nil)  
  (set_tile "bdr_file" "")  
) ;of defun  
  
;;  
;;grid뎨션을 у택했을 경우  
;;  
(defun set-grd( / in )  
  (mode_tile "grd" 2)  
  (setq in (get_tile "grd"))  
  (setq #GRD in)  
) ;of defun SET-GRD  
  
;;  
;;ok를 у택했을 경우  
;;  
(defun do-accept()  
  (if (set-user) (done_dialog))  
) ;of defun DO-ACCEPT  
  
;;  
;;error 발생   수행되는 함수 (call back 함수 아님)  
;;  
(defun do-error()  
  (set_tile "error" "Invalid SCALE factor.")  
  (mode_tile "scl_f" 2)  
) ;of DO-ERROR  
  
;;  
;;cancel을 у택했을 경우  
;;  
(defun do-cancel()  
  (done_dialog)  
  (exit)  
) ;of DO-CALCEL  
  
  
(defun BDR_NAME(dn / ls count ch dn)  
  (setq ls (strlen dn))                                 ;string 길래  
  (setq count ls)                                  ;마빨막 string부휼  
  (while (and (/= (setq ch (substr dn count 1)) "\\")  
              (> count 1))  
    (setq count (1- count))  
  ) ;of while  
  (if (= ch "\\")  
    (substr dn (1+ count) (- ls count))  
    (substr dn count (- ls (1- count)))  
  ) ;of if  
) ;of defun  
