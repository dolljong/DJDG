;**********************************
; program : stud
;           insert stud bock
;           By Yi Seok Jong (dolljong)
;           2004/05/11
;--------------------------------------

(defun C:STUD(/ p1 ls p2 lent spnt epnt ang vang vangdgr)

  (defun SETERR(s)                                          ;���忡����ƾ ����
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)                         ;���忡����ƾ ����

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
  (command "INSERT" (strcat (prefix) "BLOCKS/stud22")        ;block �̸�
                    p1                                      ;insert��
                    "" "" vangdgr)                            ;scale, angle
  (pop-os)
  
  (setq *error* oer seterr nil)                             ;������ƾ ����

  (prin1 "DJDG STUD")

) ;of defun

