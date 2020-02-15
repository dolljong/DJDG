;***************************************************
; Program : NT
;           New Text
;           By Suk-Jong Yi
;           1995/8/10, 8/14
;***************************************************
; NEWTEXT명령을 대화식으로 처리

(defun C:NT( /
             s_ent     ent     ent_type    txt     new_ent
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

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
      ((= ent_type "INSERT")                   	; entity가 att block insert인 경우.
       (setq blkn (car s_ent))   		;block name
       (setq edatresult (edat s_ent))		;edat함수결과 --> (tag value)
       (if (/= edatresult nil)
	 (progn
           (setq txt (cadr edatresult))             ; 옛 TEXT구하기.
           (setq tag (car edatresult))		;tag
           (NT_DIALOG txt)                          ; Dialog Box통하여 text입력.
           (att_wrt blkn (list (list tag (strcase new_txt))))  ;attribute 갱신.
	);progn 
;       (command "DIM1" "NEWTEXT" new_txt s_ent ""); DIM 갱신.
      );if
     );subcond 
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

  (setq dcl_id (load_dialog "DJDG"))
  (if (not (new_dialog "txtedit" dcl_id)) (exit))

  (set_tile "text_edit" old_txt)

  (action_tile "text_edit" "(set_txt)")
  (action_tile "accept" "(done_dialog)")
  (action_tile "cancel" "(set_txt)")
  (mode_tile "text_edit" 2)
  (start_dialog)
  (unload_dialog dcl_id)

) ;of defun NT_DIALOG

(defun SET_TXT( / in )
  (setq in (get_tile "text_edit"))
  (setq new_txt in)
) ; of defun SET_TXT



;***************************************************
; function : att_wrt
;
;***************************************************
(defun att_wrt (blk_nm lst2 / rpt tag val eea_lst chk blk)
;
  (setq rpt 0)
  (repeat (length lst2)
    (setq tag   (car  (nth rpt lst2))
          val   (cadr (nth rpt lst2))
          blk   blk_nm
          chk   t)
    (while chk
      (setq blk   (entnext blk)
            eea_lst (entget  blk) ) 
      (if (= (cdr (assoc 2 eea_lst)) tag)
        (progn
          (setq eea_lst (subst (cons 1 val) (assoc 1 eea_lst) eea_lst))
          (entmod  eea_lst)
          (setq chk nil)
        )
      ) ; if
    ) ;while
    (setq rpt (+ rpt 1))
  ) ;repeat

  (entupd blk_nm)

) ; defun


(princ)


