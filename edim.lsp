;------------------------------------
; PROGRAM : EDIM
;           Entity DIMension
;           Yi Suk Jong
;           04/04/03
;------------------------------------
;
(defun c:EDIM(
	      / entlst dtype defpnt plist sorted_plist npnt )
  (princ "\nSelect entity: ")
  (setq entlst (ssget))                           ;마킹대상 entity택
  
  (initget "Midpoint Endpoint")
  (setq msg (strcat "\nSelect Line entity Type [Midpoint/Endpoint] <" (if #lnend "Endpoint>: " "Midpoint>: ")))
  (setq kw (getkword msg))
  (if (= kw "Midpoint") (setq #lnend nil) (setq #lnend T))
  (initget "Horizontal Vertical Aligned")
  (setq dtype (getkword "\nSelect Dim Type [Horizontal/Vertical/Aligned]: "))
  (setq defpnt (getpoint "\nPick definition point: "))
  (setq plist (djdg_getpoints entlst #lnend))
  (if (= dtype "Vertical")
    (setq sorted_plist (vl-sort plist '(lambda (s1 s2) (< (cadr s1) (cadr s2)))))
    (setq sorted_plist (vl-sort plist '(lambda (s1 s2) (< (car s1) (car s2)))))
  );if
  (setq npnt (length sorted_plist))
  (cond
    ((= dtype "Horizontal")
       (hor_edim sorted_plist defpnt)
    ); dtype = "Horizontal"
    ((= dtype "Vertical")
       (ver_edim sorted_plist defpnt)
    ); dtype = "Horizontal"
    ((= dtype "Aligned")
       (ali_edim sorted_plist defpnt)
    ); dtype = "Horizontal"
    
  );cond
  (princ)
  
);defun

	

; -------------------------------------
; function : getLwVert
; LwPolyline의 Vertex를 척아
; 인수: vlist  : vertext list
;       tmpctr : 접근할 vertext 번호 0,1,2
; -------------------------------------

;;;  (defun getLwVert (vlist tmpctr / count tmp)
;;;
;;;    (setq count 0)					;첫 vertex 찾아감
;;;    (while (/= (car (nth count vlist)) 10)
;;;        (setq count (+ count 1))
;;;    )
;;;    (if (= tmpctr (cdr (assoc 90 vlist)))
;;;        (progn
;;;        (setq ctr 0)
;;;        (setq tmpctr 0)
;;;        )
;;;    )
;;;    (setq tmp (nth (+ count (* tmpctr 4)) vlist))
;;;    (setq tmp (append tmp (list(cdr (assoc 38 vlist)))))
;;;    (setq pt1 (trans (cdr tmp) (cdr (assoc -1 vlist)) 1))
;;;    (setq pt1 pt1)
;;;  ) ;of defun


;--------------------------------------------------------
; function : hor_edim
;            horizontal entity dim
;             Yi Suk Jong
;            04/04/03
;--------------------------------------------------------
; argument :
;    -  plist : point list
;    -  defpnt : definition point
; return : Draw horizontal dimension
;--------------------------------------------------------

(defun hor_edim(sorted_plist defpnt
		/ ipnt sgn index n dst dstext odstext
		)
      (setq ipnt (list (car (nth 0 sorted_plist)) (cadr defpnt) 0))
      (if (>= (cadr defpnt) (cadr (nth 0 sorted_plist)))  ; y of defpnt > y of first pnt
	(setq sgn 1)
	(setq sgn -1)
      );if	
      (setq index 1)
      (setq n 1)   ; 똑같은 거리 반복 갯수
      (repeat (1- npnt)
	(setq dst (- (car (nth index sorted_plist)) (car (nth (1- index) sorted_plist))))
	(setq dstext (rtos dst 2 0))
	(if (> index 1)              ;두번째부터
	  (progn
	    (if (/= dstext odstext)         ;앞거리하고 다르거나 마지막이면
	      (progn
	        (setq ipnt (f_dh ipnt odst n sgn nil))   ;draw horizontal dimension
		(setq n 1)
	      );progn
	      (progn
		(setq n (1+ n))
	      );progn
	    );if  
	  );progn    
	);if
	(setq odstext dstext odst dst)  
	(setq index (1+ index))
      );repeat
      (setq ipnt (f_dh ipnt odst n sgn nil))   ;draw horizontal dimension
);defun

;--------------------------------------------------------
; function : ver_edim
;            vertical endity dim
;             Yi Suk Jong
;            04/04/03
;--------------------------------------------------------
; argument :
;    -  plist : point list
;    -  defpnt : definition point
; return : Draw vertical dimension
;--------------------------------------------------------

(defun ver_edim(sorted_plist defpnt
		/ ipnt sgn index n dst dstext odstext 
		)
      (setq ipnt (list  (car defpnt) (cadr (nth 0 sorted_plist)) 0))
      (if (>= (car defpnt) (car (nth 0 sorted_plist)) )        ;x of first point > x of defpnt
	(setq sgn 1)
	(setq sgn -1)
      );of if	
      (setq index 1)
      (setq n 1)   ; 똑같은 거리 반복 갯수
      (repeat (1- npnt)
	(setq dst (- (cadr (nth index sorted_plist)) (cadr (nth (1- index) sorted_plist))))
	(setq dstext (rtos dst 2 0))
	(if (> index 1)              ;두번째부터
	  (progn
	    (if (/= dstext odstext)         ;앞거리하고 다르거나 마지막이면
	      (progn
	        (setq ipnt (f_dv ipnt odst n sgn nil))   ;draw horizontal dimension
		(setq n 1)
	      );progn
	      (progn
		(setq n (1+ n))
	      );progn
	    );if  
	  );progn    
	);if
	(setq odstext dstext odst dst)  
	(setq index (1+ index))
      );repeat
      (setq ipnt (f_dv ipnt odst n sgn nil))   ;draw horizontal dimension
      ;(command "circle" ipnt 1 )
);defun


;--------------------------------------------------------
; function : ali_edim
;            Aligned endity dim
;             Yi Suk Jong
;            04/04/03
;--------------------------------------------------------
; argument :
;    -  plist : point list
;    -  defpnt : definition point
; return : Draw vertical dimension
;--------------------------------------------------------

(defun ali_edim(sorted_plist defpnt
		/
		)
  
      (setq npnt (length sorted_plist))

      (setq pnt1 (nth 0 sorted_plist)
	    pnt2 (nth (1- npnt) sorted_plist))

      (setq ang (angle pnt1 pnt2))
  
      (setq xpnt (inters pnt1 pnt2 defpnt (polar defpnt (+ ang (* 0.5 pi)) 100) nil))
      (setq xang (angle xpnt defpnt))
      (setq ipnt (polar pnt1 xang (distance xpnt defpnt)))

      (setq ang1 (angle pnt1 defpnt))

      (if (minusp (dang ang ang1)) (setq sgn -1) (setq sgn 1))
  
      (setq index 1)
      (setq n 1)   ; 똑같은 거리 반복 갯수
      (repeat (1- npnt)
	(setq p1 (nth (1- index) sorted_plist)
	      p2 (nth index sorted_plist))
	
	(setq dst (distance (inters pnt1 pnt2 p1 (polar p1 (+ ang (* 0.5 pi)) 100) nil)
			    (inters pnt1 pnt2 p2 (polar p2 (+ ang (* 0.5 pi)) 100) nil)))
	(setq dstext (rtos dst 2 0))
	
	(if (> index 1)              ;두번째부터
	  (progn
	    (if (/= dstext odstext)         ;앞거리하고 다르거나 마지막이면
	      (progn
	        (setq ipnt (f_da ipnt ang odst n sgn nil))   ;draw horizontal dimension
		(setq n 1)
	      );progn
	      (progn
		(setq n (1+ n))
	      );progn
	    );if  
	  );progn    
	);if
	(setq odstext dstext odst dst)  
	(setq index (1+ index))
      );repeat
      (setq ipnt (f_da ipnt ang odst n sgn nil))   ;draw horizontal dimension
      ;(command "circle" ipnt 1 )
);defun



;------------------------
; function : djdg_grpoint
;  	주어진 point들에 grdraw로 point를 그린다.
;	Yi Suk Jong
;	06/1/2
;------------------------
; (djdg_grpoint '((x1 y1) (x2 y2) ... ) option)
; option : "add" "remove" "Auto" Auto인 경우 가장가까운점을 끄고 킨다.
; 기본데이터 : 
;-----------------------
(defun djdg_grpoint( / )
  (princ)
);defun