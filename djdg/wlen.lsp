; Program: WLEN : ���� ���� ���ֱ�

; Program: WCPL : ������ ���� ���ֱ�(ȣ, Pline�� ������ ������ ���� ���̸� ���ش�ex)���򺸰������ )

; Program: WCPL2 : �� ��� �������� ������ ���� ���̸� �����ش�. ex) ũ�ν��� ���� ��..

;***********************************
; Program : WLEN
;           Write line LENGTH
;           By Suk-Jong Yi
;           95/5/10
;***********************************

(defun C:WLEN(/
              th        ent     dst     txt     pnt
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)

  (setq th (getvar "textsize"))
  (princ "\nPick text/Text height<")
  (princ th)
  (initget "Pick")
  (setq txth (getdist ">: "))
  (cond
      ((= txth nil) (setq txth th))
      ((= txth "Pick")
          (progn
              (setq txth (cdr (assoc 40 (entget (car (entsel "\nPick text"))))))
              (princ "\nText height is ")
              (princ txth)
          ) ;of progn
      ) ;of cond txth="Pick"
  ) ;of cond

  (if sf
    (progn
      (princ "\nScale factor <")(princ sf)
      (setq tmp (getreal ">: "))
      (if (/= tmp nil)
	(setq sf  tmp)
      );end if
    );then
    (setq sf (getreal "\nScale factor: "))
  );endif  

  
  (setq ang (getangold oldang "Text angle [Align]" "Align"))
  (setq oldang ang)

;;;  (if ang
;;;    (progn
;;;      (princ "\nText angle <")(princ ang)
;;;      (setq tmp (getangle ">: "))
;;;      (if (/= tmp nil)
;;;	(setq ang (rtod tmp))
;;;      );end if
;;;    );then
;;;    (setq ang (rtod (getangle "\nText angle :")))
;;;  );endif  
    
  
;  (setq sf (getreal "\nScale factor: "))
  (princ "\nSelect Line: ")
  (while (setq selent (entsel))
    (setq ent (entget (car selent)))
    (setq sp (cdr (assoc 10 ent))
	  ep (cdr (assoc 11 ent)))
    (setq dst (distance sp ep))
    (if (= ang "Align")
      (setq tang (rtod (ang4text sp ep)))
      (if (= ang nil) (setq tang 0) (setq tang (rtod ang)))
    );if  
    (setq txt (strcat "L=" (rtos (* dst sf) 2 (getvar "LUPREC"))))
    (setq pnt (getpoint "\nPick insert point"))
    (command "TEXT" "J" "M" pnt txth tang txt)
  ) ;of while

  (pop-env)

  (setq *error* oer seterr nil)
  (princ)

) ;of defun



;------------------------
; Program: WCPL : ������ ���� ���ֱ�(ȣ, Pline�� ������ ������ ���� ���̸� ���ش�ex)���򺸰������ )
; 	Yi Suk Jong
;	06/07/31
;------------------------
; ȣ,pline�Է¹ޱ�
; ���ؼ����Է¹ޱ�(���� layer�� ó��)
; ���ؼ��� pline�� ������ ã��
; ���������� pline���� �������������� �Ÿ����ϱ�
; �������� ���ؼ��� ���������������� �Ÿ��� �������� sorting�ϱ�
; ���������� ���� ����(���ر��� ���� �Ÿ��� skip)
;------------------------
; ��������(�ɼǰ�)
; #WC_TH : text Height
; #WC_Pref
; #WC_Layer
; #WC_Minusl
; #WC_Mind
; #WC_TextA
;-------------------
(defun c:WCPL( / pl ssfilter ss ety nss i llst len p1 p2 plst lp1 lp2
	         crspt objpl dstlst p dst sdstlst sp ep dstse text )
  ;--- �ʱ�ȭ
  ;
  
  (setq ;WC_TH 75
	;WC_Pref "12X320X"
	;WC_Layer "center"
	;WC_Minusl 50
	;WC_Mind 70
	;WC_TextA 1
	gapf 1
	)
  
  (WCPL-Dialog) 	;dialog�� �Է¹ޱ�
  
  ;--- ȣ, pline�Է� �ޱ�
  (setq pl (car (entsel "\nSelect Pline or Arc: ")))  ;pl entity name
  (if (= #WC_layer "All")
    (setq ssfilter (list (cons 0 "LINE")))
    (setq ssfilter (list (cons 0 "LINE") (cons 8 #WC_Layer)))
  );end  
;  (setq ss (ssget ssfilter))
;  (setq ss nil)
  (while (=  nil (setq ss (ssget ssfilter)))
    (princ "\nLINE�� �������ּ���")
   ; (setq ss (ssget ssfilter))
  );

  ;--- ���ؼ��� pline�� ������ ã��(���� layer���鸸 ó��)
  (setq ety (cdr (assoc 0 (entget pl)))) ;entity type
  (setq nss (sslength ss))  ;line�� ����
  (setq i 0)
  
  ;-- line list�����
  (setq llst nil)
  (repeat nss
    (setq len (entget (ssname ss i)))
    (setq p1 (cdr (assoc 10 len))
	   p2 (cdr (assoc 11 len)))
    (setq llst (append llst (list (list p1 p2))));line list
    (setq i (1+ i))
  );repeat
  
  (setq plst nil)
  (if (= ety "LWPOLYLINE")  		;pline�� ���
    (setq vlst (car (mk_vertlist pl)))  ;vertex list
  );
  (if (= ety "LINE")			;line�� ���
    (setq eline (entget pl)
	  lpt1 (cdr (assoc 10 eline))
	  lpt2 (cdr (assoc 11 eline)))
  ) ;if
  
  (setq i 0)
  (repeat nss
    (setq lp1 (nth 0 (nth i llst))
          lp2 (nth 1 (nth i llst)))
    (setq ang (angle lp1 lp2)				;extend 
	  dist (distance lp1 lp2)
          lp2ex (polar lp2 ang (* dist 0.5))   		;�������� ���� extend
	  lp1ex (polar lp1 (+ ang pi) (* dist 0.5))) 
    (cond
      ((= ety "LWPOLYLINE")
        (setq crspt (car (cp_line_pline lp1ex lp2ex vlst)))
      );subcond 
      ((= ety "ARC")
;        (setq crspt (cross (entget pl) lp1ex lp2ex))
       (setq crspt (car (crossent pl (ssname ss i) 2)))
      );subcond
      ((= ety "LINE")
        (setq crspt (inters lp1ex lp2ex lpt1 lpt2))
      ) 
    );cond
    (if (= crspt nil)
       (progn (alert "No Cross Point!") (exit))
       (setq plst (append  plst (list crspt)))
    );  
    (setq i (1+ i))
  );repeat	
    
  ; ���������� pline���� �������������� �Ÿ����ϱ�
  (setq objpl (vlax-ename->vla-object pl))   ; pline�� activex object
  (setq i 0)
  (setq dstlst nil)
  (repeat (length plst)
    (setq p (nth i plst))
    (if (= ety "LINE")
      (setq dst (distance p lpt1))			;LIne�� ��� getdistatpoint�� nil������ ��찡 �����Ƿ� ��������
      (setq dst (vlax-curve-getDistAtPoint objpl p)) ;�������� distance from start point
    );if	   
    
    (setq dstlst (append dstlst (list (list dst p))))
    (setq i (1+ i))
  );repeat

  ; �������� ���ؼ��� ���������������� �Ÿ��� �������� sorting�ϱ�  
  (setq sdstlst (vl-sort	dstlst
    				'(lambda (s1 s2)
    				(< (car  s1) (car s2)))))

  ; ���������� ���� ����(���ر��� ���� �Ÿ��� skip)
  (setq i 1)
  
  (repeat (1- (length sdstlst))
    (setq sp (cadr (nth (1- i) sdstlst))	;������ ��ǥ
	  ep (cadr (nth i      sdstlst)))	;���� ��ǥ
    (setq dstse (abs (- (car (nth (1- i) sdstlst)) (car (nth i sdstlst))))) ;������ �Ÿ�
    (if (> dstse #WC_Mind)			;�ּҰŸ����� ���츸 ����
      (progn
	(setq dstse (- dstse #WC_Minusl))	; �̰ݰŸ� ����
        (setq text (rto_dimtxt dstse))		; �������� �Ÿ��� text��
        (setq text (strcat #WC_pref text))	; prefix���ϱ�
	(if (= #WC_TextA 1)			;���� ����/������
          (djdg_wtxtonline sp ep text (* (getvar "dimscale") #WC_TH) (* gapf #wc_th)) ;�������� text����
	  (progn
	    (push-os)
	    (command "TEXT" "J" "M" (mid-point sp ep) (* (getvar "dimscale") #WC_TH) "0" text)
	    (pop-os)
	  );pron
	);if  
      );progn	
    );if ;(princ dstse)
    (setq i (1+ i))
  );repeat
  

); defun WCPL


(defun WCPL-DIALOG( /  
                  dcl_id lay )  
  
  (setq dcl_id (load_dialog "DJDG"))                 ;ddscl.dcl load  
  (if (not (new_dialog "WCPL" dcl_id)) (exit))         ;ddscl.dcl ���� WCPL
  
  ; �ʱ�ȭ
  ;; list���̾�α׿� layer list �ѷ��ֱ�
  (setq #laylst nil)
  (setq #laylst (append #laylst (list "All")))
  (setq lay (tblnext "layer" T))
  (setq #laylst (append #laylst (list (cdr (assoc 2 lay)))))
  (while (setq lay (tblnext "layer"))
    (setq #laylst (append #laylst (list (cdr (assoc 2 lay)))))
  );while
  
  
  (start_list "laylst")
  (mapcar 'add_list #laylst)
  (end_list)

  (if (= nil #WC_TH) (set_tile "th" (rtos (getvar "dimtxt") 2 1))
    		     (set_tile "th" (rtos #WC_TH 2 1))) ;text���� �ʱ�ȭ
  (if (= nil #WC_Pref) (set_tile "pref" "")
    		     (set_tile "pref" #WC_Pref)) ;Prefix �ʱ�ȭ
  
  (if (= #WC_layer nil) (set_tile "laylst" "0")
      (set_tile "laylst" (itoa (vl-position #WC_layer #laylst)))); layer list�ʱ�ȭ
    
  (if (= nil #WC_Minusl) (set_tile "minusl" "0")
    		     (set_tile "minusl" (rtos #WC_Minusl 2 3))) ;Minusl �ʱ�ȭ  
  (if (= nil #WC_Mind) (set_tile "mind" "0")
    		     (set_tile "mind" (rtos #WC_Mind 2 3))) ;Prefix �ʱ�ȭ  
  (if (= nil #WC_TextA) (set_tile "ta" "0")
    		     (set_tile "ta" (rtos #WC_TextA 2 0))) ;Prefix �ʱ�ȭ  
  
  (action_tile "th" "(set_val $key)")               
  (action_tile "pref" "(set_val $key)")           
  (action_tile "minusl" "(set_val $key)")           
  (action_tile "mind" "(set_val $key)")
  (action_tile "laylst" "(set_val $key)") ;layer setting
  (action_tile "ta" "(set_val $key)")                     
  (action_tile "accept" "(do-dialog)")                
  (action_tile "cancel" "(do-cancel)")                
  
  (mode_tile "th" 2)  
  (start_dialog)  
  (unload_dialog dcl_id)  
  (princ)  
) ;of defun WCPL-dialog

(defun set_val(key / val )
  (setq val (atof (get_tile key)))
;  (if (<= val 0.0)
;    (progn
;      (set_tile "error" "Invalid Input")
;      (mode_tile key 2)
;      (mode_tile key 3)
;    );progn
;    (progn
   (cond
     ((= key "th") (setq #WC_TH val))
     ((= key "pref") (setq #WC_Pref (get_tile key)))
     ((= key "minusl") (setq #WC_Minusl val))
     ((= key "mind") (setq #WC_Mind val))
     ((= key "ta") (setq #WC_TextA val))
     ((= key "laylst")
      (progn
	(setq #WC_layer (nth (fix val)  #laylst)) 
      );progn
     );sub cond 
   );cond
;      (set_tile "error" "")
;    );progn
;  );if
);defun

(defun do-dialog()    ;��� �� ���������� �ֱ�
  (set_val "th")
  (set_val "pref")
  (set_val "minusl")
  (set_val "mind")
  (set_val "ta")
  (set_val "laylst")
  (done_dialog) ;dialog����
  (unload_dialog dcl_id)
);defun

(defun do-cancel( )
  (unload_dialog dcl_id)
  (exit)
);defun




;------------------------
; Program: WCPL2 : �� ��� �����ϴ� ������ ���� ���ֱ�ex)ũ�ν��� ����)
; 	Yi Suk Jong
;	06/08/8
;------------------------
; ȣ,pline�� �Է¹ޱ�
; ���ؼ����Է¹ޱ�(���� layer�� ó��)
; ���ؼ��� pline�� �������� ã��(��� �������� ��� ������ �߻�)
; ���������� line���� �������������� �Ÿ����ϱ�(2���ϰ�� �ʿ����)
; �������� ���ؼ��� ���������������� �Ÿ��� �������� sorting�ϱ�(2���ϰ�� �ʿ����)
; ���������� ���� ����(���ر��� ���� �Ÿ��� skip)
;------------------------
; ��������(�ɼǰ�) - WCPL�� ȥ���� ���ϱ� ���� "WC2"�� ����
; #WC2_TH : text Height
; #WC2_Pref :prefix
; #WC2_Layer : ���ؼ��� ���ý� ������ layer
; #WC2_delta : ������ ���̿� ������ ��������
; #WC2_Mind : cut-off ���ر���
; #WC2_TextA : text������ �������� ���� ���ΰ�?
;-------------------
(defun c:WCPL2( / gapf ssplf sspl ssfilter index plilst sspln ety lpt1 lpt2 eni nss i
	         llst len p1 p2 plst pl eti plp1 plp2 crspt dstlst cp sdstlst idx idst idstxt pp1 pp2 )

  
  ;--- �ʱ�ȭ
  ;
  
;  (setq #WC2_TH 75
;  	#WC2_Pref "12X320X"
;	#WC2_Layer "center"
;	#WC2_delta 50
;	#WC2_Mind 70
;	#WC2_TextA 1
	
;	)

  (setq gapf 1)		; line���� text���� gap
  
  (WCPL2-Dialog) 	;dialog�� �Է¹ޱ�
  
  ;--- ȣ, pline �ΰ��̻� . �Է� �ޱ�
  (princ "\nPline, Arc, line�� �ΰ��̻� �����ϼ���: ")
  (setq ssplf '((-4 . "<OR") (0 . "LINE") (0 . "ARC") (0 . "LWPOLYLINE") (-4 . "OR>"))) ;ss pline filter
  (setq sspl (ssget ssplf))  ;pl entity name
  (while (< (sslength sspl) 2)  ;2�� ���ϰ� ���õǸ� ��� �ݺ��ؼ� �Է¹���.
    (princ "\nPline, Arc, line�� �ΰ��̻� �����ϼ���: ")    
    (setq sspl (ssget ssplf))
  );while  
  
  ; line�� �Է¹ޱ�  
  (if (= #WC2_layer "All")
    (setq ssfilter (list (cons 0 "LINE")))
    (setq ssfilter (list (cons 0 "LINE") (cons 8 #WC2_Layer)))
  );end
  
  (princ "\nLINE�� �������ּ���")
  (while (=  nil (setq ss (ssget ssfilter)))
    (princ "\nLINE�� �������ּ���")
   ; (setq ss (ssget ssfilter))
  );

  ;--- ���ؼ��� pline�� ������ ã��
  ;��� list�����
  (setq index 0)
  (setq plilst nil)  ;pline���� list����� '(("line" (P1 p2)) ("arc" entget) ("LWPLINE" vertlist)...)
  (repeat (sslength sspl)
    (setq sspln (ssname sspl index))		;entity name
    (setq ety (cdr (assoc 0 (entget sspln)))) ;entity type
    (cond
      ((= ety "LWPOLYLINE")  		;pline�� ���
        (setq eni (car (mk_vertlist sspln)))  ;vertex list
      );subcond
      ((= ety "LINE")			;line�� ��� ������ ����
        (setq eline (entget sspln)
          lpt1 (cdr (assoc 10 eline))
          lpt2 (cdr (assoc 11 eline))
	  eni (list lpt1 lpt2))
      ) ;subcond
      ((= ety "ARC")			;arc�� ��� ent information
        (setq eni (entget sspln))
      ) ;subcond
    );cond
    (setq plilst (append plilst (list (list ety eni))))	;'((type ����)..) ����Ʈ �����
    (setq index (1+ index))
  );
  
  ;-- line list�����
  (setq nss (sslength ss))  ;line�� ����
  (setq i 0)
  (setq llst nil)         ;llst ���ε��� ������ list '((���� ����) (���� ����) ...)
  (repeat nss
    (setq len (entget (ssname ss i)))
    (setq p1 (cdr (assoc 10 len))
	   p2 (cdr (assoc 11 len)))
    (setq llst (append llst (list (list p1 p2))));line list
    (setq i (1+ i))
  );repeat
  
  (foreach line llst   ;line������ŭ ����
    (setq p1 (car line)		;�̹� line�� ����, ����	
	  p2 (cadr line))
    (setq plst nil)    ;������ list
    (foreach pl plilst  ;pline list���� ���� ����
      (setq ety (car pl)
	    eti (cadr pl));entity���� �������� �ٸ���.
      (cond
	((= ety "LINE")				;LINE�� ���
	  (setq plp1 (car eti)
		 plp2 (cadr eti))
          (setq crspt (inters p1 p2 plp1 plp2))	 
	);subcodn
	((= ety "LWPOLYLINE")			;LWPOLYLINE�� ���
          (setq crspt (car (cp_line_pline p1 p2 eti)))	 
	);subcodn
	((= ety "ARC")				;arc�� ���
          (setq crspt (cross eti p1 p2))	 
	);subcodn
      );cond
      (setq plst (append plst (list crspt)))   ;������ list�� �߰�
    );foreach sspln
    ;������ ������ �� list�����
    (setq dstlst nil)				;�Ÿ�+������ list'((�Ÿ� ������)...)
    (foreach cp plst
      (setq dstlst (append dstlst (list (list (distance p1 cp) cp))))
    );foreach cp plst
    ;������ �������� list sort
    (setq sdstlst (vl-sort dstlst '(lambda (s1 s2)
    				(< (car  s1) (car s2)))))

    (setq idx 1)
    (repeat (1- (length sdstlst))
      (setq idst (- (car (nth idx sdstlst)) (car (nth (1- idx) sdstlst))))
      (setq idst (+ idst #WC2_delta))		;delta����
      (setq idstxt (rto_dimtxt idst))		;���� text
      (setq idstxt (strcat #WC2_pref idstxt))	;prefix ����
      (setq pp1 (cadr (nth (1- idx) sdstlst))	;������
	     pp2 (cadr (nth idx sdstlst)))	;��
      (if (> idst #WC2_mind)  			;cut-off ���̺��� ���츸 ǥ��
        (djdg_wtxtonline pp1 pp2 idstxt  (* (getvar "dimscale") #WC2_th) (* gapf #WC2_th (getvar "dimscale")))
      );if  	
      (setq idx (1+ idx))			;���� �������̷�...
    );repeat  
    
  );foreach llst   				;���� line����... 

); defun WCPL2

(defun WCPL2-DIALOG( /  
                  dcl_id lay )  

  (defun wc2set_val(key / val )
    (setq val (atof (get_tile key)))
     (cond
       ((= key "wc2th") (setq #WC2_TH val))
       ((= key "wc2pref") (setq #WC2_Pref (get_tile key)))
       ((= key "wc2delta") (setq #WC2_delta val))
       ((= key "wc2mind") (setq #WC2_Mind val))
       ((= key "wc2ta") (setq #WC2_TextA val))
       ((= key "wc2laylst")
        (progn
	  (setq #WC2_layer (nth (fix val)  #laylst)) 
        );progn
       );sub cond 
     );cond
  );defun

  (defun wc2do-dialog()    ;��� �� ���������� �ֱ�
    (wc2set_val "wc2th")
    (wc2set_val "wc2pref")
    (wc2set_val "wc2delta")
    (wc2set_val "wc2mind")
    (wc2set_val "wc2ta")
    (wc2set_val "wc2laylst")
    (done_dialog) ;dialog����
    (unload_dialog dcl_id)
  );defun

  (defun wc2do-cancel( )
    (unload_dialog dcl_id)
    (exit)
  );defun

  ;--------------------------
  ;--- WCPL2-Dialog main ----
  ;--------------------------
  
  (setq dcl_id (load_dialog "DJDG"))                 ;ddscl.dcl load  
  (if (not (new_dialog "WCPL2" dcl_id)) (exit))         ;ddscl.dcl ���� WCPL
  
  ; �ʱ�ȭ
  ;; list���̾�α׿� layer list �ѷ��ֱ�
  (setq #laylst nil)
  (setq #laylst (append #laylst (list "All")))
  (setq lay (tblnext "layer" T))
  (setq #laylst (append #laylst (list (cdr (assoc 2 lay)))))
  (while (setq lay (tblnext "layer"))
    (setq #laylst (append #laylst (list (cdr (assoc 2 lay)))))
  );while
  
  
  (start_list "wc2laylst")
  (mapcar 'add_list #laylst)
  (end_list)

  (if (= nil #WC2_TH) (set_tile "wc2th" (rtos (getvar "dimtxt") 2 1))
    		     (set_tile "wc2th" (rtos #WC2_TH 2 1))) ;text���� �ʱ�ȭ
  (if (= nil #WC2_Pref) (set_tile "wc2pref" "")
    		     (set_tile "wc2pref" #WC2_Pref)) ;Prefix �ʱ�ȭ
  
  (if (= #WC2_layer nil) (set_tile "wc2laylst" "0")
      (set_tile "wc2laylst" (itoa (vl-position #WC2_layer #laylst)))); layer list�ʱ�ȭ
    
  (if (= nil #WC2_delta) (set_tile "delta" "0")
    		     (set_tile "wc2delta" (rtos #WC2_delta 2 3))) ;delta �ʱ�ȭ  
  (if (= nil #WC2_Mind) (set_tile "wc2mind" "0")
    		     (set_tile "wc2mind" (rtos #WC2_Mind 2 3))) ;Prefix �ʱ�ȭ  
  (if (= nil #WC2_TextA) (set_tile "wc2ta" "0")
    		     (set_tile "wc2ta" (rtos #WC2_TextA 2 0))) ;Prefix �ʱ�ȭ  
  
  (action_tile "wc2th" "(wc2set_val $key)")               
  (action_tile "wc2pref" "(wc2set_val $key)")           
  (action_tile "wc2delta" "(wc2set_val $key)")           
  (action_tile "wc2mind" "(wc2set_val $key)")
  (action_tile "wc2laylst" "(wc2set_val $key)") ;layer setting
  (action_tile "wc2ta" "(wc2set_val $key)")                     
  (action_tile "accept" "(wc2do-dialog)")                
  (action_tile "cancel" "(wc2do-cancel)")                
  
  (mode_tile "wc2th" 2)  
  (start_dialog)  
  (unload_dialog dcl_id)  
  (princ)
  
) ;of defun WCPL-dialog

