;*************************************
; Program : BEARING
;           Draw Bearing
;           By Suk-Jong Yi
;           2004/3/2
;*************************************
; Draw Bearing
;*************************************

(defun C:BEARING(/
            l1      l2      dnum    l1s     l1e     l2s     l2e     dst1
            dst2    temp    angs    ange    dt1     dt2     oldc    count
            dls     dle
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

;  (push-env)   

  (setq l1 (entget (car (entsel "\nSelect upper line: "))))
  (setq l2 (entget (car (entsel "\nSelect lower line: "))))
  (setq ipt (getpoint "\nPick insert point: "))
  (setq b_bearing (getreal "\nWide of bearing?: "))
  (setq t_bearing (getreal "\nThickness of bearing?: "))
  (setq t_solepl (getreal "\nThickness  of Sole Plate?: "))

  (setq r_cbearing 0.5) ;ratio of pad
  (setq tupb (* (- 1 r_cbearing) 0.5 t_bearing))   ;thickness of upper plate
  (setq tcb (* r_cbearing t_bearing))    ;thickness of center pad of bearing
  
  
  (setq ln1s (cdr (assoc 10 l1)))    ;
  (setq ln1e (cdr (assoc 11 l1)))    ;
  (setq ln2s (cdr (assoc 10 l2)))    ;
  (setq ln2e (cdr (assoc 11 l2)))    ;

  (setq xsl (- (car ipt) (/ b_bearing 2) 50))  ;x-coord of left side of sole plate
  (setq xsr (+ (car ipt) (/ b_bearing 2) 50))  ;x-coord of right side of sole plate

  (setq us1 (inters ln1s ln1e (list xsl (cadr ipt)) (list xsl (- (cadr ipt) 1000)) nil))  ;upper sole left
  (setq us2 (inters ln1s ln1e (list xsr (cadr ipt)) (list xsr (- (cadr ipt) 1000)) nil))  ;upper sole right

  (if (< (car us1) (car us2))
    (setq usl us1
	  usr us2)
    (setq usl us2
	  usr us1)
  );if

  (setq lsl (list (car usl) (- (cadr usl) t_solepl)))
  (setq lsr (polar lsl 0 (+ b_bearing 100)))

  (push-os)
  (command "pline" usl lsl lsr usr "")
  (pop-os)

  (setq uupl (polar lsl 0 50))   ;upper upper plate left
  (setq uupr (polar uupl 0 b_bearing)) ;upper upper plate right
  (setq lupl (polar uupl (* pi (/ 270.0 180.0)) tupb)) ;lower upper plate left
  (setq lupr (polar lupl 0 b_bearing));lower upper plate right
  
  (push-os)
  (command "pline" uupl lupl lupr uupr  "")
  (pop-os)
  
  (setq updl (polar lupl 0 tupb));upper pad left
  (setq updr (polar lupr pi tupb));upper pad right
  (setq lpdl (polar updl (dtor 270.0) tcb));upper pad right
  (setq lpdr (polar updr (dtor 270.0) tcb));upper pad right
  
  (push-os)
  (command "line" updl lpdl "")
  (command "line" updr lpdr "")
  (pop-os)
  
  (setq ulpl (polar lupl (dtor 270.0) tcb));upper lower plate left
  (setq ulpr (polar lupr (dtor 270.0) tcb));upper lower plate right
  (setq llpl (polar ulpl (dtor 270.0) tupb));lower lower plate left
  (setq llpr (polar ulpr (dtor 270.0) tupb));lower lower plate right

  (push-os)
  (command "pline" llpl ulpl ulpr llpr  "")
  (pop-os)
  ;; mortar

  (setq uml (polar llpl pi 50))
  (setq umr (polar llpr 0 50))
  (setq lml (list (- (car uml) 50) (- (cadr uml) 50)))
  (setq lmr (list (+ (car umr) 50) (- (cadr uml) 50)))

  (push-os)
  (command "pline" lml uml umr lmr  "")
  (pop-os)
  ;; bad block

  (setq lpt (polar llpl (dtor 225.0) 100))
  (setq rpt (polar llpr (dtor 315.0) 100))
  (setq lcp (inters llpl lpt ln2s ln2e nil));left cross point 
  (setq rcp (inters llpr rpt ln2s ln2e nil));left cross point 

  (setq ubl (list (car lcp) (cadr lml)))
  (setq ubr (list (car rcp) (cadr lmr)))
  
  (push-os)
  (command "pline" lcp ubl ubr rcp  "")
  (pop-os)

;  (pop-env)                                             
  (setq *error* oer seterr nil)
  (prin1)

) ;of defun

