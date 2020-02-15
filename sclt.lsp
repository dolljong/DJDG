;****************************************
;*    CTH
;*              Change Text Height
;*              By Suk-Jong Yi
;               98/12/16: Scale option 추가
;               98/8/26 : MTEXT 인식가능하도록 수정
;*              95/2/5
;****************************************

(defun C:sclt()

;;;  (defun SETERR(s)                          ;내장에러루틴 정의
;;;    (if (/= s "Function cancelled")
;;;        (princ (strcat "\nError: " s))
;;;    ); of If
;;;    (setq *error* oer seterr nil)
;;;    (princ)
;;;  ); of SETERR
;;;
;;;  (setq oer *error* *error* seterr)         ;내장에러루틴 가동

  (setq scl 0.5)  ;scale factor
  
;   (initget "Height All Select")
;   (setq ans (getkword "\nHeight/All/<Select>: "))
   (setq ss1 (ssget "X" '((-4 . "<AND")
                         (0 . "TEXT")
                         (8 . "APART")
                         (-4 . "AND>")))) ; All text
   (setq num (sslength ss1))
   (setq index 0)
   (repeat num
     (setq en (entget (ssname ss1 index )))
     (setq pnt (cdr (assoc 10 en)))
     (if (= index 0)
       (setq llpnt pnt)
       
       (if (< (cadr pnt) (cadr llpnt))
	 (setq llpnt pnt))
     );if  
     (setq index (1+ index))
   );repeat  

   (command "scale" ss1 "" llpnt  scl)

;;;  (setq *error* oer seterr nil)

   (princ)

) ;of defun

