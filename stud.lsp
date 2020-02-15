;**********************************
; program : stud
;           insert stud bock
;           By Yi Seok Jong (dolljong)
;           2004/05/11
;--------------------------------------

(defun C:STUD(/ p1 ls p2 lent spnt epnt ang vang vangdgr)

  (defun SETERR(s)                                          ;내장에러루틴 정의
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)                         ;내장에러루틴 가동

  (setq ls (entsel "\nSelect a line: "))
  (setq p1 (getpoint "\nPick insert point: "))
  (setq p2 (getpoint "\nPick Side: "))

  (setq lent (entget (car ls)))
  (setq spnt (cdr (assoc 10 lent))    ;start point  
	epnt (cdr (assoc 11 lent)))   ;ent point
  (setq ang (angle spnt epnt))
  (setq vang (v_angle spnt epnt p2))
  (setq vangdgr (rtod vang))
  
  (push-os) 
  (command "INSERT" (strcat (prefix) "BLOCKS/stud22")        ;block 이름
                    p1                                      ;insert점
                    "" "" vangdgr)                            ;scale, angle
  (pop-os)
  
  (setq *error* oer seterr nil)                             ;에러루틴 복귀

  (prin1 "DJDG STUD")

) ;of defun

