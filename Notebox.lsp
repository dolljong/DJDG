;**********************************
; Program : NOTEBOX
;           insert NOTE mark and draw NOTE BOX
;           By Suk-Jong Yi
;           1999/1/28
;*************************************

(defun C:NOTEBOX(
/                gap_mark r_fillet t_shadow ds
                 p0 p1 p2 p3 p4 p5 p6 p2_1 p3_1 p4_1 p5_1)

  (defun SETERR(s)                                          ;내장에러루틴 정의
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)                                                ;환경변수 대피

  ;---초기값성정
  (setq gap_mark 2                  ;note mark와 box의 간격
        r_fillet 3                  ;우측하단 fillet의 반지름
        t_shadow 1)                 ;box우측과 좌측의 음영 두께

  (setq ds (getvar "DIMSCALE"))     ;dimscale값 얻음

  (setq gap_mark (* gap_mark ds)             ;초기값에 dimscale값 적용
        r_fillet (* r_fillet ds)
        t_shadow (* t_shadow ds)
        t_shadow2 (* t_shadow 0.5))          ;음영두께의 1/2

  (setq p1 (getpoint "\nPick Upper_left corner: "))     ;상자의 좌측상단점입력
  (setq p6 (getcorner p1 "\nPick Lower_right corner: "))   ;상자의 우측하단점입력

  (setq p1x (car p1)                ;삽입점 x좌표
        p1y (cadr p1)               ;삽입점 y좌표
        p6x (car p6)                ;우측하단 x좌표
        p6y (cadr p6))              ;우측하단 y좌표

  (setq p0 (list p1x              (+ p1y gap_mark) 0.0)
        p2 (list p1x              p6y              0.0)     ;좌측하단
        p3 (list (- p6x r_fillet) p6y              0.0)     ;우측하단-1
        p4 (list p6x              (+ p6y r_fillet) 0.0)     ;우측하단-2
        p5 (list p6x              p1y              0.0)     ;우측상단
        p2_1 (list (+ p1x t_shadow2) (- p6y t_shadow2) 0.0) ;음영좌측하단
        p3_1 (list (- p6x r_fillet) (- p6y t_shadow2) 0.0)  ;음영우측하단-1
        p4_1 (list (+ p6x t_shadow2) (+ p6y r_fillet) 0.0)  ;음영우측하단-2
        p5_1 (list (+ p6x t_shadow2) (- p1y t_shadow2) 0.0));음영우측상단

  (command "PLINE" p1 p2 p3 "A" p4 "L" p5 "C")
  (command "PLINE" p2_1 "W" t_shadow "" p3_1 "A" p4_1 "L" p5_1 "W" "0" "" "")
  (command "INSERT" (strcat (prefix) "blocks/note") p0 ds "" "0")

  (pop-env)                                                 ;환경변수 복귀

  (setq *error* oer seterr nil)                             ;에러루틴 복귀

  (prin1)

) ;of defun

