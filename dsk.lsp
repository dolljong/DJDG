;--------------------------------------
; program : DSK
;           Dimension text for Skew
;           Yi Suk Jong
;           00/7/12
;--------------------------------------
; 치수선이나 숫자의 skew거리를 ()에 표시해준다.
;--------------------------------------
(defun c:DSK(
/ ds th oldskew skew fskew ssdim ndim index plist diment dimtype
  dimtxt assoc10 assoc11 assoc50 sktext tang txtpnt10 txtpnt11
  disto assoc13 assoc14  ang w4 )
  
  (setq ds (getvar "dimscale")
        th (getvar "dimtxt"))

  (setq oldskew skew)
  (if (= skew nil)
    (setq skew (getreal "\nSkew(degree): "))    ;get skew degree
    (progn
      (princ "\nSkew(dgree)<")(princ skew)(setq skew (getreal ">: "))
    );progn
  )  

  (if (= skew nil) (setq skew oldskew))
  
  (setq fskew (/ 1 (sin (dtor skew))))        ;get skew factor
  
  (setq ssdim (ssget '((-4 . "<OR") (0 . "DIMENSION") (0 . "TEXT") (-4 . "OR>"))))
  
  (setq ndim (sslength ssdim))
  
  (setq index 0)
  
  (repeat ndim
    (setq diment (entget (ssname  ssdim index)))
    (setq dimtype (cdr (assoc 0 diment)))    ;entity type
    (cond 
      ((= dimtype "TEXT")
        (setq dimtxt (cdr (assoc 1 diment)))    ;dimsion text
        (setq assoc10 (cdr (assoc 10 diment)))  ;text insert point
        (setq assoc11 (cdr (assoc 11 diment)))  ;text insert point       
        (setq assoc50 (cdr (assoc 50 diment)))  ;text angle
   
        (setq sktext (scl_mdimtxt dimtxt fskew))
       
        (setq tang assoc50
	      txtpnt10 (polar assoc10 (- tang (* 0.5 pi)) (* th ds 2.0))
	      txtpnt11 (polar assoc11 (- tang (* 0.5 pi)) (* th ds 2.0)))

        (setq newdiment (subst (cons 1 sktext) (assoc 1 diment) diment))
        (setq newdiment (subst (cons 10 txtpnt10) (assoc 10 newdiment) newdiment))
        (setq newdiment (subst (cons 11 txtpnt11) (assoc 11 newdiment) newdiment))

	(entmake newdiment)      
      );subcond
      ((= dimtype "DIMENSION")
        (setq disto (cdr (assoc 42 diment)))    ;default distant
        (setq dimtxt (cdr (assoc 1 diment)))    ;dimsion text
        (setq assoc13 (cdr (assoc 13 diment)))  ;definition point-1
        (setq assoc14 (cdr (assoc 14 diment)))  ;definition point-2 
        (setq assoc10 (cdr (assoc 10 diment)))  ;dimension line point
        (setq assoc11 (cdr (assoc 11 diment)))  ;dimension text point    
        (setq ang (angle assoc14 assoc10))

        (setq sktext (scl_mdimtxt dimtxt fskew) )
        (setq w4 (which4 ang))
        (cond                                       
          ((or (= w4 1) (= w4 2)) (setq tang (+ ang (* -0.5 pi)) tgapsgn -1))
          ((or (= w4 3) (= w4 4)) (setq tang (+ ang (* 0.5 pi)) tgapsgn 0))
        ) ;of cond
    
        (setq txtpnt (polar assoc11 (+ ang (* tgapsgn pi)) (* 2 th ds)))
        (command "TEXT" "M" txtpnt (* ds th) (rtod tang) sktext)
      );subcond	
    );cond	

    (setq index (1+ index))
  );repeat

  (princ)
  
);defun



;----------------------------------
; function : str_position
;            Yi Suk Jong
;            00/7/15
;----------------------------------
; str1 : long string
; str2 : short string
;----------------------------------
(defun str_position(str1 str2 / str1 str2 len1 len2 count )

  (setq len1 (strlen str1)
	len2 (strlen str2))
  (setq count 1)
  (while (and (/= (substr str1 count len2) str2) (<= count (- len1 len2 -1)))
    (setq count (1+ count))
  ); repeat  
 (if (> count (- len1 len2 -1)) nil count)
);defun str_position

;-----------------------------------
;function : div_dimtxt
;           Yi Suk JOng
;           00/7/15
;----------------------------------
; str1 : dim string
;다양한 형태의 dimension text를 잘라서 list로 만들어준다. 
;단순한 1.000치수인 경우에는 1.000을 리턴, 10@100인경우에
;는 (10 100)을 리턴,10@100=1.000 인경우엔 (10 100 1.000)을
;리턴해준다
;----------------------------------------------------------
(defun div_dimtxt(dimstr / dimstrlen golpos eqpos)
  (setq dimstrlen (strlen dimstr)
        golpos (str_position dimstr "@")
	eqpos (str_position dimstr "="))
  
  (cond
    ((and (= golpos nil) (= eqpos nil))   ;1.000
      (list dimstr)
    )
    ((and (/= golpos nil) (= eqpos nil))  ;10@100
     (list (substr dimstr 1 (1- golpos))
	   (substr dimstr (1+ golpos) (- dimstrlen golpos)))
    )
    ((and (/= golpos nil) (/= eqpos nil))  ;10@100=1.000
     (list (substr dimstr 1 (1- golpos))
	   (substr dimstr (1+ golpos) (- eqpos golpos 1))
           (substr dimstr (1+ eqpos) (- dimstrlen eqpos )))
    ) 
    ((and (= golpos nil) (/= eqpos nil))  ;=1.000
     (list dimstr)
    ) 
    
  );cond  
  
);defun div_dimtxt

;--------------------------
; function : scl_dimtxt
;            Yi Suk JOng
;            00/7/15
;--------------------------
;scl_dimtxt(dimstr scl) : dimension text는 1.0m이하는 0.900으로 표기하지 않고 900으로
;                    표시하므로 이것을 인식하여 숫자로 인식한다음 일정한 숫자를 곱해 
;                    다시 text로 만들어준다.     
(defun scl_dimtxt(dimstr scl / dimstr scl len)
  (if (= (str_position dimstr ".") nil)
    (setq len (* (atof dimstr) 0.001 scl))
    (setq len (* (atof dimstr) scl))
  );if
  (if (>= len 1.000)
    (rtos len 2 3)
    (rtos (* len 1000) 2 0)
  );if  
	  
);defun


;--------------------------
; function : scl_mdimtxt
;            Yi Suk JOng
;            00/7/18
;--------------------------
;scl_mdimtxt(dimtxt scl) : dsk.lsp : 10@100=1.000과 같은 하나의 숫자가 아닌 여러개의 숫자로 이루
;                                    어진 dimension text의 scl 을 곱한 text를 리턴해준다.
;--------------------------
(defun scl_mdimtxt(dimtxt scl / dimtxt sktext divtxt divn )
        (if (or (= dimtxt "<>") (= dimtxt ""))
	    (setq sktext (scl_dimtxt (rtos disto) scl))
            (progn
	      (setq divtxt (div_dimtxt dimtxt)
                    divn (length divtxt))
	      (cond
	        ((= divn 1)
	          (setq divtxt (car divtxt))
	          (if (= (substr divtxt 1 1) "=")
	              (setq sktext (strcat "=" (scl_dimtxt (substr divtxt 2 (1- (strlen divtxt))) scl)))
		      (setq sktext (scl_dimtxt divtxt scl))	
	          );if  
	        );subcond
	        ((= divn 2)
	          (setq sktext (strcat (car divtxt) "@" (scl_dimtxt (cadr divtxt) scl)))
	        );sub cond
	        ((= divn 3)
	          (setq sktext (strcat (car divtxt) "@"
				     (scl_dimtxt (cadr divtxt) scl) "="
				     (scl_dimtxt (caddr divtxt) scl)))
	       
	        );sub con  
	      );cond  
	    );progn  
        );if
    (setq sktext (strcat "(" sktext ")"))
);defun


;--------------------------
; Program : DDR
;           R이다른 호의 길이를 만들어준다. 예를 들어 외측웹 치수를 이용해서 내측웹 길이를 표시
;           Dim Different Radius
;		Yi Suk Jong
;		06/08/03
;--------------------------
; 원본 텍스트들을 선택한다.
; 다이얼로그박스를 통해 옵션을 입력한다.
;   - Prefix, postfix
;   - 원본 Radius (직접입력하거나 엔티티를 선택할 수 있게 한다)
;   - delta R(-,+가 있으면 delta로 인식, 없으면 타겟R로 인식) 엔티티 선택가능하도록 
;   - function (+ , - 등을 입력)
;-----------------------------------
(defun c:ddr ( / ds dh fskew ssdim ndim index diment dimtype dimtxt assoc10 assoc11 assoc50
	         sktext tang txtpnt10 txtpnt11 newdiment disto assoc13 assoc14 ang w4 txtpnt)
  
  (setq ds (getvar "dimscale")
        th (getvar "dimtxt"))
  
  ; 초기화
  ;  (setq #DDR_pref "("
  ;	#DDR_postf ")"
  ;	#DDR_OR 100000.0
  ;	#DDR_DRS "300.0"  ;delta R(string)
  ;	#DDR_DRV 300		;delta R(value)
  ;	#DDR_DL -100)     ;Delta L
  
  ; Prefix와 postfix가 설정되지 않은 경우 "("와 ")"로 설정.
  (if (= #DDR_pref nil) (setq #DDR_pref "("))
  (if (= #DDR_postf nil) (setq #DDR_postf ")"))

  
  (DDR-dialog)		   ;dialog box로 입력받음.
  
  	 
  (setq fskew (/ #DDR_DRV #DDR_OR))        ;get  factor 
  
  (setq ssdim (ssget '((-4 . "<OR") (0 . "DIMENSION") (0 . "TEXT") (-4 . "OR>"))))
  
  (setq ndim (sslength ssdim))
  
  (setq index 0)
  
  (repeat ndim
    (setq diment (entget (ssname  ssdim index)))
    (setq dimtype (cdr (assoc 0 diment)))    ;entity type
    (cond 
      ((= dimtype "TEXT")
        (setq dimtxt (cdr (assoc 1 diment)))    ;dimsion text
        (setq assoc10 (cdr (assoc 10 diment)))  ;text insert point
        (setq assoc11 (cdr (assoc 11 diment)))  ;text insert point       
        (setq assoc50 (cdr (assoc 50 diment)))  ;text angle
        (setq th (cdr (assoc 40 diment)))       ; text height 
   
        (if (and (= (substr dimtxt 1 1) "(") (= (substr dimtxt (strlen dimtxt) 1) ")"))
	  (setq dimtxt (substr dimtxt 2 (- (strlen sktext) 2)));()제거한 text
	);if	 
        (setq sktext (scl_mdimtxt dimtxt fskew))
        (setq sktext (strcat #DDR_pref (substr sktext 2 (- (strlen sktext) 2)) #DDR_postf))
       
        (setq tang assoc50
	      txtpnt10 (polar assoc10 (- tang (* 0.5 pi)) (* th ds 2.0))
	      txtpnt11 (polar assoc11 (- tang (* 0.5 pi)) (* th ds 2.0)))

        (setq newdiment (subst (cons 1 sktext) (assoc 1 diment) diment))
        (setq newdiment (subst (cons 10 txtpnt10) (assoc 10 newdiment) newdiment))
        (setq newdiment (subst (cons 11 txtpnt11) (assoc 11 newdiment) newdiment))

	(entmake newdiment)      
      );subcond
      ((= dimtype "DIMENSION")
        (setq disto (cdr (assoc 42 diment)))    ;default distant
        (setq dimtxt (cdr (assoc 1 diment)))    ;dimsion text
        (setq assoc13 (cdr (assoc 13 diment)))  ;definition point-1
        (setq assoc14 (cdr (assoc 14 diment)))  ;definition point-2 
        (setq assoc10 (cdr (assoc 10 diment)))  ;dimension line point
        (setq assoc11 (cdr (assoc 11 diment)))  ;dimension text point    
        (setq ang (angle assoc14 assoc10))

        (if (and (= (substr dimtxt 1 1) "(") (= (substr dimtxt (strlen dimtxt) 1) ")"))
	  (setq dimtxt (substr dimtxt 2 (- (strlen sktext) 2)));()제거한 text
	);if	 
        (setq sktext (scl_mdimtxt dimtxt fskew) )
        (setq sktext (strcat #DDR_pref (substr sktext 2 (- (strlen sktext) 2)) #DDR_postf))
       
        (setq w4 (which4 ang))
        (cond                                       
          ((or (= w4 1) (= w4 2)) (setq tang (+ ang (* -0.5 pi)) tgapsgn -1))
          ((or (= w4 3) (= w4 4)) (setq tang (+ ang (* 0.5 pi)) tgapsgn 0))
        ) ;of cond
    
        (setq txtpnt (polar assoc11 (+ ang (* tgapsgn pi)) (* 2 th ds)))
        (command "TEXT" "M" txtpnt (* ds th) (rtod tang) sktext)
      );subcond	
    );cond	

    (setq index (1+ index))
  );repeat

  (princ)

);defun ddr



(defun DDR-dialog( / what_next dcl_id )
  (setq WHAT_NEXT 4)
  (while (< 1 WHAT_NEXT)
  (setq dcl_id (load_dialog "DJDG"))                 ;ddscl.dcl load  
  (if (not (new_dialog "DDR" dcl_id)) (exit))         ;ddscl.dcl 안의 DDR
  
  ; 초기화

;  (setq #laylst nil)
;  (setq #laylst (append #laylst (list "All")))
;  (setq lay (tblnext "layer" T))
;  (setq #laylst (append #laylst (list (cdr (assoc 2 lay)))))
;  (while (setq lay (tblnext "layer"))
;    (setq #laylst (append #laylst (list (cdr (assoc 2 lay)))))
;  );while
  
  
;  (start_list "laylst")
;  (mapcar 'add_list #laylst)
;  (end_list)

  (if (= nil #DDR_OR) (set_tile "sr" "")
    		     (set_tile "sr" (rtos #DDR_OR 2 3))) ;원본 반지름 초기화
  (if (= nil #DDR_DRS) (set_tile "tr" "")
    		     (set_tile "tr"  #DDR_DRS )) 	;target 반지름(string값임) 초기화
  (if (= nil #DDR_pref) (set_tile "pref" "")
    		     (set_tile "pref"  #DDR_pref )) 	;Prefix 초기화
  (if (= nil #DDR_postf) (set_tile "postf" "")
    		     (set_tile "postf"  #DDR_postf )) 	;Postfix 초기화

    
  (action_tile "sr" "(set_val $key)")
  (action_tile "tr" "(set_val $key)")
  (action_tile "pref" "(set_val $key)")
  (action_tile "postf" "(set_val $key)")
  (action_tile "slsa" "(done_dialog 2)")                     
  (action_tile "slta" "(done_dialog 3)")
  (action_tile "accept" "(done_dialog)")
  (action_tile "cancel" "(exit)")                    
;  (action_tile "cancel" "(do-cancel)")                
  
  (mode_tile "th" 2)  
  (setq WHAT_NEXT (start_dialog))
  (if (= WHAT_NEXT 2)
    (get_arc 2); 
  );if
  (if (= WHAT_NEXT 3)
    (get_arc 3); 
  );if
    
 );while
  
;  (alert "OK")
  (unload_dialog dcl_id)  
  (princ)  

  
);defun


(defun get_arc( key / arc2 arc3 )
  (if (= key 2)
    (progn
      (setq arc2 (car (entsel "\nSelect Source ARc: ")))
      (setq #DDR_OR (cdr (assoc 40 (entget arc2))))
;      (set_tile "sr" (rtos #DDR_OR 2 3))
    );progn
    (progn
      (setq arc3 (car (entsel "\nSelect Target ARc: ")))
      (setq #DDR_DRV (cdr (assoc 40 (entget arc3)))) ;#DDR_DRV은 값으로 저장(정확한 값)-->실제 작업
      (setq #DDR_DRS (rtos #DDR_DRV 2 3)) ;#DDR_DRS은 string으로 저장(round된 값) 표시만 함.
;      (set_tile "tr" (rtos #DDR_TR 2 3))
    );progn      
  );if
);defun

;---------
; "tr" key 로 입력이 들어오면
;   > +,-가 앞에 있는 경우 : delta R로 인식하여 string으로 처리 #DDR_DRS에 저장
;			    #DDR_DRV에 원본 R값에 +,-처리를 해서 저장(값)
;   > 그냥 값인 경우: 목표R값으로 인식하여 #DDR_DRS에는 round된 string저장
;				          #DDR_DRV에는 값저장(round되지 않은 값)
(defun set_val(key / val )
  (setq val (atof (get_tile key)))
   (cond
     ((= key "sr") (setq #DDR_OR val))
     ((= key "tr")
       (setq #DDR_DRS (get_tile key))		;DRS은 string으로 일기
       (if (= (substr #DDR_DRS 1 1) "+")
         (setq #DDR_DRV (+ #DDR_OR (atof #DDR_DRS)))  ; 목표 R 값
         (if (= (substr #DDR_DRS 1 1) "-")
           (setq #DDR_DRV (- #DDR_OR (atof #DDR_DRS)))  ;목표 R
           (setq #DDR_DRV (atof #DDR_DRS))
         );if
       );if  
     );subcond  
     ((= key "pref") (setq #DDR_pref (get_tile key)))  ;string
     ((= key "postf") (setq #DDR_postf (get_tile key))) ;string     
   );cond
);defun

