;*******************************************
;     Program :  Number & Coordinate
;                          By Suk-Jong Yi
;                          1995. 2. 3.
;*******************************************

;*************
; Main program
;*************

(DEFUN C:NUMCOOR()
  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

(setq lp (getvar "LUPREC"))

(setvar "cmdecho" 0)
(setq sn (getint "Start Number <1>: "))
(if (= sn nil) (setq sn 1))
(setq txts (getvar "textsize"))
(princ "Text hight <")
(princ txts)
(setq txth (getdist ">: "))
(terpri)
(if (= nil txth) (setq txth txts))
(setq sf (getreal "Scale factor <1.0>: "))                  ;scale factor·³·Â
(if (= sf nil) (setq sf 1.0))
(setq lc (getpoint "Pick location of Coordinate list: "))
(terpri)
(setq num sn)

;*** Question file writing
(initget "Yes No")
(setq ans (getkword "Make file?(Yes/No): "))
(if (= ans "Yes")
    (progn
; get file name
;        (setq fn (getstring "Enter file name :"))
        (setq fn (getfiled "Open data file" "" "dat" 1))

;*** check exist file
        (if (findfile fn)
            (progn
              (princ "\nFile allready exist, ")
              (initget 1 "OverWrite Append")
              (setq ansOA (getkword "OverWrite / Append: "))
              (cond
                 ((= ansOA "OverWrite") (setq opf (open fn "w")))
                 ((= ansOA "Append") (setq opf (open fn "a")))
              ); of cond
             ); of progn
;*** open write file (if file isn't exist)
        (setq opf (open fn "a"))
        ); of if
    ) ; End prog
    nil
) ; End if

(setq count 1)
(princ "\nPoint# ")
(princ num)

(while

;*** get node point
   (setq pnt (getpoint " Pick point: "))
   (setq pntx (car pnt))
   (setq pnty (cadr pnt))

;*** integer to number
   (setq txt (itoa num))

;*** get number text position
   (setq pnt1x (+ pntx txth))
   (setq pnt1y (+ pnty txth))
   (setq pnt1 (list pnt1x pnt1y))

;*** draw number text
   (command "text" "j" "m" pnt1 txth "" TXT)
   (terpri)

;*** get coord text
   (setq pxy (strcat txt "(" (rtos (* pntx sf) 2 lp)
                         "," (rtos (* pnty sf) 2 lp) ")"))
   ;(princ pxyf opf)

   (if (= ans "Yes")
      (progn
   ; Write File (ans = "Yes")
;*** get coord text to write file
   (setq pxyf (strcat txt "," (rtos (* pntx sf) 2 lp)
                          "," (rtos (* pnty sf) 2 lp)))
   (write-line pxyf opf)
   ;(princ "coord= ")
   ;(princ pxyf)
      ) ; End then
   ) ; End if

;*** get coord text position
   (setq cpy (- (cadr lc) (* count (* 2.0 txth))))
   (setq cpx (car lc))
   (setq cpxy (list cpx cpy))

;*** draw coord text
   (command "text" cpxy txth "" pxy)
   (terpri)
   (setq num (+ num 1))
   (setq count (+ count 1))
   (princ "Point# ")
   (princ num)
) ; of while

(if (= ans "Yes")
   (close opf)
) ;End if

  (setq *error* oer seterr nil)
(princ)
);;; End of Program

