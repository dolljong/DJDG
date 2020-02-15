;**********************************
; Program : JUNG1
;           중앙분리대 그리기
;           By Suk-Jong Yi
;           1998/5/7
;*************************************

(defun C:JUNG1(/ p1)

  (defun SETERR(s)                                          ;내장에러루틴 정의
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)                         ;내장에러루틴 가동

  (push-env)                                                ;환경변수 대피

  (setq p1 (getpoint "\nPick insert point: "))              ;삽입정 입력받음

  (command "INSERT" (strcat (prefix) "BLOCKS/jung1")        ;block 이름
                    p1                                      ;insert점
                    "" "" "0.0")                            ;scale, angle

  (pop-env)                                                 ;환경변수 복귀

  (setq *error* oer seterr nil)                             ;에러루틴 복귀

  (prin1)

) ;of defun

