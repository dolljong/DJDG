;**********************************
; Program : WATERM
;           WATER Mark
;           By Suk-Jong Yi
;           98/10/14, 95/7/7
;*************************************

(defun C:WATERM(/
                p1      p2      p3      intp        blkdir
)

  (defun SETERR(s)                                      ;���忡����ƾ ����
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)                     ;���忡����ƾ ����

  (push-env)                                            ;ȯ�溯�� ����

  (setq p1 (getpoint "\nFirst point or press ENTER to select: "))       ;������
  (if (= p1 nil)
    (setq ent (entget (car (entsel "\nSelect line: ")))
          p1 (cdr (assoc 10 ent))
          p2 (cdr (assoc 11 ent)))
    (setq p2 (getpoint p1 "\nPick point second point: "))   ;����
  );if

  (setq oldc (getvar "CECOLOR"))
  (setvar "CECOLOR" "GREEN")
  (command "LINE" p1 (list (car p2) (cadr p1) 0.0) "")
  (setvar "CECOLOR" oldc)

  (setq p3 (getpoint "\nPick insert point: "))

  (setq blkdir (strcat (prefix) "BLOCKS/ELM"))

  (command "INSERT" (strcat (prefix) "BLOCKS/WATERM")         ;block ����
                    (list (car p3) (cadr p1) 0.0)             ;insert��á
                    (getvar "DIMSCALE") "" "0.0")             ;scale, angle

  (pop-env)                                                     ;ȯ�溯�� ����

  (setq *error* oer seterr nil)                                 ;���忡����ƾ ����

  (prin1)

) ;of defun
