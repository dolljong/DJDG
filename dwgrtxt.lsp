(defun c:dwgrtxt ()

  (princ (getvar "extmax"))
  
  (setq texth_factor 15)
   
  (delrtext)  ;erase exist rtext

  (command "zoom" "e" )
  (princ (getvar "extmax"))

  
;  (setq ipnt (getpoint "pick point"))

  (setq extmax (getvar "extmax")
	extmin (getvar "extmin")
	extx1 (car extmin)
	exty1 (cadr extmin)
	extx2 (car extmax)
	exty2 (cadr extmax)
	hlen (- extx2 extx1)
	vlen (- exty2 exty1)
	text-size (/ (max hlen vlen) texth_factor)
	insertion-point (list extx1 (+ exty2 (* 2 text-size)) 0))
  
  
  (setq layer "0"
;	insertion-point ipnt
;	text-size 10
	text-style "standard"
	rotation-angle-in-radians 0
	diesel-expression "$(getvar, \"dwgname\")"
	)
  
(if (not (member "rtext.arx" (arx))) (arxload "RText.arx" nil))

   (entmake
    (list '(0 . "RTEXT")
     '(100 . "AcDbEntity")
     '(100 . "RText")
     (cons 8 layer)
     (cons 10 insertion-point)
     (cons 40 text-size)
     (cons 7 text-style)
     (cons 50 rotation-angle-in-radians)
     (cons 1 diesel-expression)
     '(70 . 1) ;1 IS FOR DIESEL EXPRESSIONS
    )
   )
  
;  (setq header (list '(0 . "RTEXT") '(100 . "RText") (cons 10 ipnt) '(50 . 0.0) '(210 0.0 0.0 1.0) '(40 . 0.758146) '(7 . "GHS") '(70 . 1) '(1 . "$(getvar, \"dwgname\")")))
;  (setq header (list '(0 . "RTEXT") '(100 . "RText") '(10 3.31689 6.28686 0.0) '(50 . 0.0) '(210 0.0 0.0 1.0) '(40 . 0.758146) '(7 . "GHS") '(70 . 1) '(1 . "$(getvar, \"dwgname\")")))
  
  	
  (entmake header)

  (command "zoom" "e" )

)

(defun delrtext( / ss_lst)
  (setq f_list (list (cons 0 "RTEXT")))    ;filter list
  (setq ss_lst (ssget "X" f_list))                      ;entity º±≈√
  (if ss_lst
    (command "erase" ss_lst "")
  ); of if  
)  
	