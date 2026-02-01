;****************************************************
; Program: DJDGLOGO.LSP
;          VIEW DJDG LOGO
;          Yi Suk Jong
;          97/1/23
;****************************************************

(defun C:DJDGLOGO(/
)

  ;;
  ;; Function: SB_DIALOG (Dialog box로 입력받기)
  ;;
  (defun LOGO_DIALOG (/
        dcl_id
  )
    (setq dcl_id (load_dialog "DJDG"))                  ;dialog호출
    (if (not (new_dialog "DJDGLOGO" dcl_id)) (exit))

    (start_image "djdglogo")                                  ;image 보이기
    (slide_image
                 0 0
                 (dimx_tile "djdglogo") (dimy_tile "djdglogo")
                 "djdg(djdglogo)"
    )
    (end_image)

    (action_tile "accept" "(done_dialog)")
    (mode_tile "accept" 2)
    (start_dialog)
    (unload_dialog dcl_id)
  ) ;of defun SPLICE_DIALOG

  ;;;
  ;;; main routine
  ;;;

  (logo_dialog)
  (princ)

) ;of defun
