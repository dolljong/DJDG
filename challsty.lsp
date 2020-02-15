;---------------
; command : challsty
;  change all stylelist
;---------------
(defun c:challsty(/
		    fontname stylelist sty h )
  (setq fontname (getstring "\nEnter Font name: "))
  (setq stylelist nil)
  (setq sty (tblnext "STYLE" T))
  (while sty
    (setq stylelist (append stylelist (list (cdr (assoc 2 sty)))))
    (setq sty (tblnext "STYLE"))
  )  
  (foreach h stylelist
    (command "STYLE" h fontname "" "" "" "" "")
  );foreach  
);defun  