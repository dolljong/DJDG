;***************************************
;    program : EPLS
;              End Point of Lines
;              Suk-Jong Yi
;              1995. 2. 11
;***************************************

(defun C:EPL()

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)
; get file name
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

;*** Select lines
(princ "\nSelect lines: ")
(setq entss (ssget))
(setq entn (sslength entss))

;*** input Upper point / Lower point
(initget "Upper Lower")
(setq ans (getkword "Upper/Lower point"))

;(setq opf (open (getstring "Enter file name") "a"))

;*** Get file name
(if opf
   (progn
      (setq entidx 0)
      (repeat entn
         (setq entlst (entget (ssname entss entidx)))
         (setq x1 (cadr (assoc 10 entlst))
               y1 (caddr (assoc 10 entlst))
               x2 (cadr (assoc 11 entlst))
               y2 (caddr (assoc 11 entlst)))
         (cond
            ((= ans "Upper")
               (if (> y1 y2)
                  (progn
                     (setq yp y1)
                     (setq xp x1)
                  )
                  (progn
                     (setq yp y2)
                     (setq xp x2)
                  ) ;
               ) ;of if y1>y2
            ) ;of ans=Upper
            ((= ans "Lower")
               (if (< y1 y2)
                  (progn
                     (setq yp y1)
                     (setq xp x1)
                  )
                  (progn
                     (setq yp y2)
                     (setq xp x2)
                  )
               ) ;of if y1<y2
            ) ;of ans=Lower
         ) ;of cond
         (setq prec (getvar "LUPREC"))
         (setq idxs (itoa (1+ entidx))
            xps  (rtos xp 2 prec)
            yps  (rtos yp 2 prec))
         (setq wls (strcat idxs "," xps "," yps))
         (write-line wls opf)
         (setq entidx (1+ entidx))
      ) ;of repeat entn
      (close opf)
   ); of progn
(princ "File not found")
) ;of if open
  (setq *error* oer seterr nil)
) ;of defun EPL


