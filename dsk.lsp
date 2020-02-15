;--------------------------------------
; program : DSK
;           Dimension text for Skew
;           Yi Suk Jong
;           00/7/12
;--------------------------------------
; ġ�����̳� ������ skew�Ÿ��� ()�� ǥ�����ش�.
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
;�پ��� ������ dimension text�� �߶� list�� ������ش�. 
;�ܼ��� 1.000ġ���� ��쿡�� 1.000�� ����, 10@100�ΰ�쿡
;�� (10 100)�� ����,10@100=1.000 �ΰ�쿣 (10 100 1.000)��
;�������ش�
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
;scl_dimtxt(dimstr scl) : dimension text�� 1.0m���ϴ� 0.900���� ǥ������ �ʰ� 900����
;                    ǥ���ϹǷ� �̰��� �ν��Ͽ� ���ڷ� �ν��Ѵ��� ������ ���ڸ� ���� 
;                    �ٽ� text�� ������ش�.     
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
;scl_mdimtxt(dimtxt scl) : dsk.lsp : 10@100=1.000�� ���� �ϳ��� ���ڰ� �ƴ� �������� ���ڷ� �̷�
;                                    ���� dimension text�� scl �� ���� text�� �������ش�.
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
;           R�̴ٸ� ȣ�� ���̸� ������ش�. ���� ��� ������ ġ���� �̿��ؼ� ������ ���̸� ǥ��
;           Dim Different Radius
;		Yi Suk Jong
;		06/08/03
;--------------------------
; ���� �ؽ�Ʈ���� �����Ѵ�.
; ���̾�α׹ڽ��� ���� �ɼ��� �Է��Ѵ�.
;   - Prefix, postfix
;   - ���� Radius (�����Է��ϰų� ��ƼƼ�� ������ �� �ְ� �Ѵ�)
;   - delta R(-,+�� ������ delta�� �ν�, ������ Ÿ��R�� �ν�) ��ƼƼ ���ð����ϵ��� 
;   - function (+ , - ���� �Է�)
;-----------------------------------
(defun c:ddr ( / ds dh fskew ssdim ndim index diment dimtype dimtxt assoc10 assoc11 assoc50
	         sktext tang txtpnt10 txtpnt11 newdiment disto assoc13 assoc14 ang w4 txtpnt)
  
  (setq ds (getvar "dimscale")
        th (getvar "dimtxt"))
  
  ; �ʱ�ȭ
  ;  (setq #DDR_pref "("
  ;	#DDR_postf ")"
  ;	#DDR_OR 100000.0
  ;	#DDR_DRS "300.0"  ;delta R(string)
  ;	#DDR_DRV 300		;delta R(value)
  ;	#DDR_DL -100)     ;Delta L
  
  ; Prefix�� postfix�� �������� ���� ��� "("�� ")"�� ����.
  (if (= #DDR_pref nil) (setq #DDR_pref "("))
  (if (= #DDR_postf nil) (setq #DDR_postf ")"))

  
  (DDR-dialog)		   ;dialog box�� �Է¹���.
  
  	 
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
	  (setq dimtxt (substr dimtxt 2 (- (strlen sktext) 2)));()������ text
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
	  (setq dimtxt (substr dimtxt 2 (- (strlen sktext) 2)));()������ text
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
  (if (not (new_dialog "DDR" dcl_id)) (exit))         ;ddscl.dcl ���� DDR
  
  ; �ʱ�ȭ

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
    		     (set_tile "sr" (rtos #DDR_OR 2 3))) ;���� ������ �ʱ�ȭ
  (if (= nil #DDR_DRS) (set_tile "tr" "")
    		     (set_tile "tr"  #DDR_DRS )) 	;target ������(string����) �ʱ�ȭ
  (if (= nil #DDR_pref) (set_tile "pref" "")
    		     (set_tile "pref"  #DDR_pref )) 	;Prefix �ʱ�ȭ
  (if (= nil #DDR_postf) (set_tile "postf" "")
    		     (set_tile "postf"  #DDR_postf )) 	;Postfix �ʱ�ȭ

    
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
      (setq #DDR_DRV (cdr (assoc 40 (entget arc3)))) ;#DDR_DRV�� ������ ����(��Ȯ�� ��)-->���� �۾�
      (setq #DDR_DRS (rtos #DDR_DRV 2 3)) ;#DDR_DRS�� string���� ����(round�� ��) ǥ�ø� ��.
;      (set_tile "tr" (rtos #DDR_TR 2 3))
    );progn      
  );if
);defun

;---------
; "tr" key �� �Է��� ������
;   > +,-�� �տ� �ִ� ��� : delta R�� �ν��Ͽ� string���� ó�� #DDR_DRS�� ����
;			    #DDR_DRV�� ���� R���� +,-ó���� �ؼ� ����(��)
;   > �׳� ���� ���: ��ǥR������ �ν��Ͽ� #DDR_DRS���� round�� string����
;				          #DDR_DRV���� ������(round���� ���� ��)
(defun set_val(key / val )
  (setq val (atof (get_tile key)))
   (cond
     ((= key "sr") (setq #DDR_OR val))
     ((= key "tr")
       (setq #DDR_DRS (get_tile key))		;DRS�� string���� �ϱ�
       (if (= (substr #DDR_DRS 1 1) "+")
         (setq #DDR_DRV (+ #DDR_OR (atof #DDR_DRS)))  ; ��ǥ R ��
         (if (= (substr #DDR_DRS 1 1) "-")
           (setq #DDR_DRV (- #DDR_OR (atof #DDR_DRS)))  ;��ǥ R
           (setq #DDR_DRV (atof #DDR_DRS))
         );if
       );if  
     );subcond  
     ((= key "pref") (setq #DDR_pref (get_tile key)))  ;string
     ((= key "postf") (setq #DDR_postf (get_tile key))) ;string     
   );cond
);defun

