;*************************************
; Program : DUD
;           Dimension Up Down
;           By Suk-Jong Yi
;           1995/8/14
;*************************************
; 치수선의 치수를 위/아래로 조정

(defun C:DUD (/
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

  (setq oer *error* *error* seterr)

  (push-env)                               ;환경변수 대피

  (setq ss_ent (ssget))                    ;DIM entity선택

  (setq old_tvp (getvar "DIMTVP")        ; 옛 DIM변수 대피
        old_tad (getvar "DIMTAD"))

  (while (/= ss_ent nil)                   ;반복 수행
    (initget "Up Down")                    ;
    (setq ans (getkword "\nUp/<Down>: "))    ; Up/Down 선택
    (if (= ans nil) (setq ans "Down"))      ; 리턴 입력시 Down으로 간주
    (cond
      ((= ans "Up") (setq new_tvp 1))      ; UP/Down에 따라 DIMTVP값 설정
      ((= ans "Down") (setq new_tvp -1))
    ) ;of cond

    (setvar "DIMTAD" 0)                    ; 새 DIM변수 설정
    (setvar "DIMTVP" new_tvp)
    (command "DIM1" "UPDATE" ss_ent "")    ; UPDATE명령 수행
    (setvar "DIMTVP" old_tvp)              ; 옛 DIM변수 복귀
    (setvar "DIMTAD" old_tad)
    (setq ss_ent (ssget))                  ; 다음 selection set 선택
  ) ;of while

  (pop-env)                                ;환경변수 복귀
  (princ)

  (setq *error* oer seterr nil)

) ;of defun
