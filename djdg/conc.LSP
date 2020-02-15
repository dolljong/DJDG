;**********************************
; Program : CONC
;           insert CONCrete mark
;           By Suk-Jong Yi
;           1998/2/3
;*************************************

(defun C:CONC(/
                p1      p2      p3      intp        blkdir
)

  (defun SETERR(s)                                          ;내장에러루틴 정의
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)                                                ;환경변수 대피

  (setq p1 (getpoint "\nPick insert point: "))

  (setvar "PDMODE" 0)

  (command "INSERT" (strcat (prefix) "BLOCKS/CONC")         ;block 이름
                    p1                                      ;insert뜬찼
                    (getvar "DIMSCALE") "" "0.0")           ;scale, angle

  (pop-env)                                                 ;환경변수 복귀

  (setq *error* oer seterr nil)                             ;에러루틴 복귀

  (prin1)

) ;of defun

