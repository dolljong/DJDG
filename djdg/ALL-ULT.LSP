;**********************************
; Program : ALL-ULT
;           Allowable-Ulimate
;           By Suk-Jong Yi
;           1996/8/21
;*************************************

(defun C:ALL-ULT(/
                p1      p2      p3      intp        blkdir
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)                                            ;환경변수 대피

  (setq ds (getvar "DIMSCALE"))
  (initget "Allow Ultim")
  (setq ans (getkword "\nAllowable/Ultimate: "))
  (setq p1 (getpoint "\nPick upper right corner: "))

  (if (= ans "Allow")
    (command "INSERT" (strcat (prefix) "BLOCKS/ALLOW") p1 ds "" "0.0")
    (command "INSERT" (strcat (prefix) "BLOCKS/ULTIM") p1 ds "" "0.0")
  ) ;of if

  (pop-env)                                                     ;환경변수 복귀
  (setq *error* oer seterr nil)
  (prin1)

) ;of defun

