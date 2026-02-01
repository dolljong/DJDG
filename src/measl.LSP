;************************************
; Program : MEASL
;           LEASure Line
;           By Suk-Jong Yi
;           95/7/7
;************************************
; 주어진 두점사이를 일정한 거리로 Point 찍어준다.

(defun C:MEASL(/
sp ep dst ang delta_dst blk_name
div_n ans rot_ang count
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
(setq delta_dst (getint "\n<Segment length/Block: "))
(if (= delta_dst "Block")
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
    (setq delta_dst (getint "\nSegment length: "))
    (setq div_n (fix (/ dst delta_dst)))
    (setq count 1)
    (repeat div_n
      (setq pnt (polar sp ang (* count delta_dst)))
      (command "INSERT" blk_name pnt "" "" (rtod rot_ang))
      (setq count (1+ count))
    ) ;of repeat
  ) ;of progn THEN
  (progn
    (setq div_n (fix (/ dst delta_dst)))
    (setq count 1)
    (repeat div_n
      (setq pnt (polar sp ang (* count delta_dst)))
      (command "POINT" pnt)
      (setq count (1+ count))
    ) ;of repeat
  ) ;of progn ELSE
) ;of if

(princ "\nTotal length= ")
(princ dst)
(princ "\nNumber of segment= ")
(princ div_n)

(pop-env)

  (setq *error* oer seterr nil)
(princ)
) ;of defun


