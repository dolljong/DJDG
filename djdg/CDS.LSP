;*************************************
; Program : CDS
;           Change Dimension Scale
;           By Yi Suk-Jong
;           1998/3/10
;*************************************
; 치수선의 scale을 바꾸어줌

(defun C:CDS (/
                ss_ent     ans     old_tvp     old_tad    new_tvp
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

;  (setq oer *error* *error* seterr)         ;내장에러루틴가동

  (push-env)                                ;환경변수 대피

  (setq ss_filter (list '(0 . "DIMENSION")))   ;filter지정
  (setq ss_ent (ssget ss_filter))               ;DIM entity선택
  (setq dsc (getreal "\nEnter Scale: "))

;  (setq old_tvp (getvar "DIMTVP")           ; 옛 DIM변수 대피
;        old_tad (getvar "DIMTAD"))
  (command "DIMEDIT" "H" ss_ent "")         ;선택된 dimension을 home으로

  (setq ndim (sslength ss_ent))             ;dimension 갯수
  (setq count 0)

  (repeat ndim                              ;반복 수행
    (setq dim_ent (entget (ssname ss_ent count)))  ;dim정보
    (setq dline_point (cdr (assoc 10 dim_ent)))   ;치수선점
    (setq dend_point (cdr (assoc 14 dim_ent)))    ;치수선지정점(definition point)
    (setq dim_angle (angle dend_point dline_point))     ;각도구하기
    (setq dim_dist (distance dend_point dline_point))   ;거리구하기
    (setq new_dline_point
          (polar dend_point dim_angle (* dim_dist dsc)));새 치수선점 구하기
    (setq ass10 (assoc 10 dim_ent))                     ;assoc 10
    (setq newass10 (cons (car ass10) new_dline_point))  ;새점
    (setq new_ent (subst newass10 ass10 dim_ent))       ;
    (entmod new_ent)                                    ;수정하기
    (setq count (1+ count))                 ;다음치수선으로
  ) ;of repeat

  (pop-env)                                ;환경변수 복귀

  (princ)

;  (setq *error* oer seterr nil)

) ;of defun
