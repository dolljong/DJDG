;
; ----------------------------------------------------------[ J/C/A/D ]----
;                                                 Programmed by Jeong H.C. 
;                                                  Last Updated  00/ 9/ 4  
; =========================================================================
;
 (defun c:ad4 ( /  ss_get _BName _DName _a1 _a2)
;
  (setq olderr *error* *error* errchck)
  (setvar "cmdecho"  0)
;
  (setvar "Attreq" 0)
;  (setq bn (strcase (getstring "\nBlock name: ")))
  (setq bn "KHCBLKA1-B")
;  (setq tn (strcase (getstring "\nTag name: ")))
;  (setq tn "TD_DWGNO")
  (setq tn "TB_CSCOP")    
;  (setq _a2 (strcase (getstring "\nNew Value: ")))
  (setq _a2 "건설공사 (제2공구)") 
;
; -------------------------------------------------------------------------
  (setq &u_r5 (getvar "USERR5"))
;
  (setq ss_get nil
;        ss_get (ssget "X" '((-4 . "<AND") (0  . "INSERT")
;                             (2 . "NO") (-4 .   "AND>")) ) ) ; 2. "도각파일명"<---여기no랑...
        ss_get (ssget "X" (list '(-4 . "<AND")
				'(0  . "INSERT")
				 (cons 2 bn)
                                '(-4 .   "AND>")))) ; 2. "도각파일명"<---여기no랑...

	;
  (if (> (sslength ss_get) 0)
    (progn
      (setq _BName (ssname ss_get 0)
            _DName (getvar "DWGNAME")
            _a1    (strlen _DName)
	    )
;            _a2    (substr _DName 1 (- _a1 4)) ) ;
;
;      ($att_wrt _BName (list (list "NO." (strcase _a2))) ) ; ..."NO." ; Attribute Tag  <--여기no.이거를 바꾸면 도면번호가 도곽안에 쓰져
      ($att_wrt _BName (list (list tn (strcase _a2))) ) ; ..."NO." ; Attribute Tag  <--여기no.이거를 바꾸면 도면번호가 도곽안에 쓰여져      
    ) ; p
  ) ; i
;
  (princ)
           ;
) ; de_fun
;
; =========================================================================
;
;
(defun $att_wrt (#blk_nm #_lst2 / #_rpt #_tag #_val eea_lst #_chk #_blk)
;
  (setq #_rpt 0)
  (repeat (length #_lst2)
    (setq #_tag   (car  (nth #_rpt #_lst2))
          #_val   (cadr (nth #_rpt #_lst2))
          #_blk   #blk_nm
          #_chk   t)
    (while #_chk
      (setq #_blk   (entnext #_blk)
            eea_lst (entget  #_blk) ) ;
      (if (= (cdr (assoc 2 eea_lst)) #_tag)
        (progn
          (setq eea_lst (subst (cons 1 #_val) (assoc 1 eea_lst) eea_lst))
          (entmod  eea_lst)
          (setq #_chk nil)
        ) ; p
      ) ; i
    ) ; w
    (setq #_rpt (+ #_rpt 1))
  ) ; r
;
  (entupd #blk_nm)
;
) ; de_fun
;
;

(princ)
;------------------------
; function : djdg_gettag
;            get tag
;            Yi Suk Jong
;            05/04/15
;-----------------------
; 엔티티 선택정보를 주면 선택한 점의 tag를 돌려준다.
;-----------------------
; argument : etsel : (entsel)의 return값.
; return : (tag value)
(defun edat(etsel / )
  
;  (setq sel (entsel "\nSelect Att: "))  ;att선택.
  (setq return nil)
  (setq spnt (cadr etsel))  ;selection point
  (setq ename (car etsel))  ;entity name
  (while (= (cdr (assoc 0 (entget (setq ename (entnext ename))))) "ATTRIB")
    (setq einf (entget ename))
    (setq ipnt (cdr (assoc 10 einf)))
    (setq txt (cdr (assoc 1 einf)))  ;text정보.
    (setq tbox (textbox einf))
    (setq tbox1 (car tbox)
	  tbox2 (cadr tbox))
    (setq pnt2 (list (+ (car ipnt) (car tbox2))
		     (+ (cadr ipnt) (cadr tbox2))))
    (setq xspnt (car spnt)
	  yspnt (cadr spnt))
    (setq tag (cdr (assoc 2 einf)))
;    (command "circle" (list xspnt yspnt))
    (if (and (and (<= xspnt (car pnt2)) (>= xspnt (car ipnt)))
	     (and (<= yspnt (cadr pnt2)) (>= yspnt (cadr ipnt))))
      (progn
;	(command "rectang" ipnt pnt2)
	(setq return (list (cdr (assoc 2 einf)) (cdr (assoc 1 einf))))
      );progn
    );  
  );while
  return
);defun  
