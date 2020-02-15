;************************************
; Program : DIVL
;           DIVIDE Line
;           By Suk-Jong Yi
;           95/7/7
;************************************
; 주어진 두점사이를 등분하여 Point 찍어준다.

(defun C:DIVL(/
sp ep dst ang div_n blk_name
ans rot_ang delta_dst count pnt
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)
(push-env)

(setq sp (getpoint "\nPick start point: "))
(setq ep (getpoint sp "\nPick end point: "))
(setq dst (distance sp ep))
(setq ang (angle sp ep))

(initget "Block")
(setq div_n (getint "\n<Number of segments/Block: "))
(if (= div_n "Block")
  (progn
    (setq blk_name (getstring "\nBlock name: "))
    (while (= (tblsearch "BLOCK" blk_name) nil)
      (prompt (strcat "\nBlock " blk_name " Not found"))
      (setq blk_name (getstring "\nBlock name to insert: "))
    ) ;of while
    (initget "Yes No")
    (setq ans (getkword "\nAlign block with object? <Y>: "))
    (if (= ans nil) (setq ans "Yes"))
    (if (= ans "Yes") (setq rot_ang ang)
                      (setq rot_ang 0.0)
    ) ;of if
    (setq div_n (getint "\nNumber of segments: "))
    (setq delta_dst (/ dst div_n))
    (setq count 1)
    (repeat (1- div_n)
      (setq pnt (polar sp ang (* count delta_dst)))
      (command "INSERT" blk_name pnt "" "" (rtod rot_ang))
      (setq count (1+ count))
    ) ;of repeat
  ) ;of progn THEN
  (progn
    (setq delta_dst (/ dst div_n))
    (setq count 1)
    (repeat (1- div_n)
      (setq pnt (polar sp ang (* count delta_dst)))
      (command "POINT" pnt)
      (setq count (1+ count))
    ) ;of repeat
  ) ;of progn ELSE
) ;of if

(princ "\nTotal length= ")
(princ dst)
(princ "\nSegment length= ")
(princ delta_dst)

(pop-env)

  (setq *error* oer seterr nil)
(princ)
) ;of defun

