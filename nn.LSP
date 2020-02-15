;***************************************************
; Program : NT
;           New Text
;           By Suk-Jong Yi
;           1995/8/10, 8/14
;***************************************************
; NEWTEXT명령을 대화식으로 처리

(defun C:NN( /
             s_ent     ent     ent_type    txt     new_ent
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
;  (setq oer *error* *error* seterr)

  (push-env)                                      ; 환경변수 대피

  (setq s_ent (entsel "\nSelect DIM or TEXT: "))  ; 바꿀 DIM 또는 TEXT선택

  (while (/= s_ent nil)                           ; 반복 수행
    (setq ent (entget (car s_ent)))               ; entity정보
    (setq ent_type (cdr (assoc 0 ent)))           ; entity type
    (cond
      ((= ent_type "TEXT")                        ; entity가 TEXT인 경우
       (setq txt (cdr (assoc 1 ent)))             ; 옛 TEXT 구하기
       (NT_DIALOG txt)                            ; Dialog Box통하여 text입력
       (entmod (subst (cons 1 new_txt) (assoc 1 ent) ent)) ;text 갱신
      )
      ((= ent_type "DIMENSION")                   ; entity가 DIM인 경우
       (setq txt (cdr (assoc 1 ent)))             ; 옛 TEXT구하기
       (NT_DIALOG txt)                            ; Dialog Box통하여 text입력
       (command "DIM1" "NEWTEXT" new_txt s_ent ""); DIM 갱신
      )
    ) ;of COND
    (setq s_ent (entsel "\nSelect DIM or TEXT: "))
  ) ;of while

  (pop-env)                                       ;환경변수 복귀

  (setq *error* oer seterr nil)
  (princ)

) ;of defun C:NT

;;;
;;; new text입력 루틴
;;;
(defun NT_DIALOG ( old_txt / dcl_id old_txt)

  (setq dcl_id (load_dialog "NN"))
  (if (not (new_dialog "txtedit" dcl_id)) (exit))

  (setq spc nil)

  (set_tile "text_edit" old_txt)

  (action_tile "text_edit" "(set_txt)")
  (action_tile "special" "(spctext)")

  (action_tile "accept" "(set_txt) (done_dialog)")
  (action_tile "cancel" "(set_txt)")
  (mode_tile "text_edit" 2)
  (start_dialog)
  (unload_dialog dcl_id)

) ;of defun NT_DIALOG

(defun SET_TXT( / in )
  (setq in (get_tile "text_edit"))
  (setq new_txt in)
  (setq old_txt in)
) ; of defun SET_TXT

(defun spctext()

  (if (not (new_dialog "txteditspc" dcl_id)) (exit))

    (set_tile "spc_code" "")

    (start_image "img11")                                  ;image 보이기
    (slide_image  0 0
                  (dimx_tile "img11") (dimy_tile "img11")
                  "djdg(fuweld)")
    (end_image)

    (start_image "img12")                                  ;image 보이기
    (slide_image  0 0
                  (dimx_tile "img12") (dimy_tile "img12")
                  "djdg(fuweld)")
    (end_image)


  (action_tile "text_edit" "(set_txt)")

  (action_tile "img11" "(spccode $key)")
  (action_tile "img12" "(spccode $key)")

  (action_tile "accept" "(done_dialog)")
  (action_tile "cancel" "(done_dialog)")

;  (mode_tile "spc_code" 2)
  (start_dialog)
  (unload_dialog dcl_id)

);eefun

(defun spccode(key / in )
  (cond
    ((= key "img11") (set_tile "spc_code" "%%131"))
    ((= key "img12") (set_tile "spc_code" "%%132"))
  );cond
) ; of defun ADD_TXT



