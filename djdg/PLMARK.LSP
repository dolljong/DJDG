;*************************************
; Program : PLMARK
;           PLate MARKing
;           Suk-Jong Yi
;           1995. 3. 19
;*************************************

(defun C:PLMARK(/
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

(push-env)                    ;환경변수값 대피

(setq oldclr (getvar "CECOLOR"))

(setq ds (getvar "DIMSCALE")  ;dimension 스케일 값 구함
      th (getvar "DIMTXT"))   ;text크기는 dimtxt

(setq mp (getpoint "\nPick middle point: "))   ;text의 중간점 입력
(setq ang (getangle mp "\nEnter angle: "))   ;끝점 입력
(setq txt (getstring T "\nEnter text: "))      ;text 입력
(setq txtl (* th (strlen txt) ds))            ;텍스트이 길이 구함
(setq pnt1 (polar mp (+ ang pi) (+ (/ txtl 2.0) (* ds th)))) ;마크-1 삽입점
(setq pnt2 (polar mp    ang     (+ (/ txtl 2.0) (* ds th)))) ;마크-2 삽입점

(setvar "CECOLOR" "3")
(command "INSERT" (strcat (prefix) "blocks/plmark") pnt1 ds "" (rtod ang))
                                                                ;마크-1 삽입
(command "INSERT" (strcat (prefix) "blocks/plmark") pnt2
                   (* ds -1) "" (rtod ang))                     ;마크-2 삽입

(setvar "CECOLOR" oldclr)
(command "TEXT" "j" "M" mp (* ds th) (rtod ang) txt)               ;텍스트 쓰기

(pop-env)                                      ;대피 환경변수 복귀
  (setq *error* oer seterr nil)
(princ)
) ;of defun
