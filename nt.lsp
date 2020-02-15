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
           ($att_wrt blkn (list (list tag (strcase new_txt))))  ;attribute 갱신.
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



;
; ----------------------------------------------------------[ J/C/A/D ]----
;                                                 Programmed by Jeong H.C. 
;                                                  Last Updated  00/ 9/ 4  
; =========================================================================
;
 (defun c:ad4 ( /  ss_get _BName _DName _a1 _a2)
;
  (setq olderr *error* *error* errchck)
  (setvar "cmdecho"  0)
;
  (setvar "Attreq" 0)
;  (setq bn (strcase (getstring "\nBlock name: ")))
  (setq bn "KHCBLKA1-B")
;  (setq tn (strcase (getstring "\nTag name: ")))
;  (setq tn "TD_DWGNO")
  (setq tn "TB_CSCOP")    
;  (setq _a2 (strcase (getstring "\nNew Value: ")))
  (setq _a2 "건설공사 (제2공구)") 
;
; -------------------------------------------------------------------------
  (setq &u_r5 (getvar "USERR5"))
;
  (setq ss_get nil
;        ss_get (ssget "X" '((-4 . "<AND") (0  . "INSERT")
;                             (2 . "NO") (-4 .   "AND>")) ) ) ; 2. "도각파일명"<---여기no랑...
        ss_get (ssget "X" (list '(-4 . "<AND")
				'(0  . "INSERT")
				 (cons 2 bn)
                                '(-4 .   "AND>")))) ; 2. "도각파일명"<---여기no랑...

	;
  (if (> (sslength ss_get) 0)
    (progn
      (setq _BName (ssname ss_get 0)
            _DName (getvar "DWGNAME")
            _a1    (strlen _DName)
	    )
;            _a2    (substr _DName 1 (- _a1 4)) ) ;
;
;      ($att_wrt _BName (list (list "NO." (strcase _a2))) ) ; ..."NO." ; Attribute Tag  <--여기no.이거를 바꾸면 도면번호가 도곽안에 쓰져
      ($att_wrt _BName (list (list tn (strcase _a2))) ) ; ..."NO." ; Attribute Tag  <--여기no.이거를 바꾸면 도면번호가 도곽안에 쓰여져      
    ) ; p
  ) ; i
;
  (princ)
           ;
) ; de_fun
;
; =========================================================================
;
;
(defun $att_wrt (#blk_nm #_lst2 / #_rpt #_tag #_val eea_lst #_chk #_blk)
;
  (setq #_rpt 0)
  (repeat (length #_lst2)
    (setq #_tag   (car  (nth #_rpt #_lst2))
          #_val   (cadr (nth #_rpt #_lst2))
          #_blk   #blk_nm
          #_chk   t)
    (while #_chk
      (setq #_blk   (entnext #_blk)
            eea_lst (entget  #_blk) ) ;
      (if (= (cdr (assoc 2 eea_lst)) #_tag)
        (progn
          (setq eea_lst (subst (cons 1 #_val) (assoc 1 eea_lst) eea_lst))
          (entmod  eea_lst)
          (setq #_chk nil)
        ) ; p
      ) ; i
    ) ; w
    (setq #_rpt (+ #_rpt 1))
  ) ; r
;
  (entupd #blk_nm)
;
) ; de_fun
;
;

(princ)


