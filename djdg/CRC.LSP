;****************************************
;*    CRC
;*              Circle Radius Change
;*              By Suk-Jong Yi
;*              1995. 2. 4
;****************************************

(defun C:CRC()
  (defun SETERR(s)                      ;내장에러루틴 정의
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)         ;내장에러루틴 가동

   (initget "Radius All Select")
   (setq ans (getkword "\nRadius/All/<Select>: "))
;   (setq cr (getreal "Enter Radius of Circle <All Circle>: "))
   (cond
       ((or (= ans nil) (= ans "Select"))
           (princ "\nSelect circle: ")
           (setq ss1 (ssget))
       ) ;of ans=nil
       ((= ans "All")
           (setq ss1 (ssget "X" '((0 . "CIRCLE")))) ; All circle
       ) ;of ans=All
       ((= ans "Radius")
           (progn                                   ; Radius circle
               (setq cr (getreal "Enter radius: "))
               (setq sslst (list (cons 40 cr) '(0 . "CIRCLE")))
               (setq ss1 (ssget "X" sslst))
           ) ;of progn
       ) ;of ans="Radius"
   ); of cond
   (setq num (sslength ss1))
   (princ num)
   (princ " found")
   (setq ncr (getreal "\nEnter New Radius: "))
   (setq index 0)
   (setq cnum 0)
   (repeat num
       (setq entl (entget (ssname ss1 index)))
       (setq index (1+ index))
       (setq ass (assoc 0 entl))
       (if (= "CIRCLE" (cdr ass))
           (progn
               (setq cnum (1+ cnum))
               (setq ass1 (assoc 40 entl))
               (setq co (cons (car ass1) ncr))
               (setq entl1 (subst co ass1 entl))
               (entmod entl1)
               (princ)
           ) ;of progn
       ) ;of if
   ) ;of repeat
   (princ cnum)
   (princ " Modified")
  (setq *error* oer seterr nil)
   (princ)
) ;of defun

