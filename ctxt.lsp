; Program : CTXT : ���� text�� ���� text�� �����Ͽ� ������ �����ϱ�
; Program : meq  : �������� text�� ������ ���缭 �����ϰ� �����. (Multi Equilize)
; function : meq-dialog : meq����� dialog �Է�

;****************************************
;*    ctxt
;*              Copye TeXT
;*              By Suk-Jong Yi
;*              99/2/9
;****************************************

(defun C:ctxt(
              / num srctxt ss1 entl ass1 co entl1 cnum)     ;������������

  (defun SETERR(s)                          ;���忡����ƾ ����
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)         ;���忡����ƾ ����

  (setq srctxt (entget (car (entsel "\nPick Source Text: ")))) ;����text����
  (if (= (cdr (assoc 0 srctxt)) "TEXT")
    (progn
      (setq srctxt (cdr (assoc 1 srctxt)))                     ;����text����
      (princ "\nSelct destination text: ")
      (setq ss1 (ssget))                                    ;���text����
      (setq num (sslength ss1))                             ;text����

      (princ num)
      (princ " Text Found")                             ;�߰ߵ� text���� ǥ��

      (setq index 0)                                    ;�ʱ�ȭ
      (setq cnum 0)

      (repeat num                                       ;text ������ŭ �ݺ�
        (setq entl (entget (ssname ss1 index)))         ;���ƼƼ����Ȯ��
        (if (= (cdr (assoc 0 entl)) "TEXT")             ;text�ΰ�쿡��
          (progn
            (setq ass1 (assoc 1 entl))                ;���text assoc����Ÿ Ȯ��
            (setq co (cons (car ass1) srctxt))        ;����text assoc����Ÿ �ۼ�
            (setq entl1 (subst co ass1 entl))         ;���ο� entity�����ۼ�
            (entmod entl1)                            ;���ο� text�� ������Ʈ
            (setq cnum (1+ cnum))                     ;������ text����
          );progn

          (princ "\nNon-Text entity was filtered")    ;text�ƴ� ���
        );if
        (setq index (1+ index))                   ;���� text��
      ) ;of repeat
    );progn

    (princ "\Text not found")

  );if

   (terpri)
   (princ cnum)
   (princ " Text Modified")

  (setq *error* oer seterr nil)

   (princ)

) ;of defun



;****************************************
;* Program :    meq
;*              Multi Equalize Text
;               �������� text�� ������ ���缭 �����ϰ� �����. 
;*              By Suk-Jong Yi
;*              06/08/01
;****************************************
; �ҽ� text �׷� ����
; �ҽ� text �׷��� ������ ���
; Ÿ�� text �׷� ����
; Ÿ�� text �׷��� ������ ���
; �� �׷� ���� üũ, ���� �ٸ��� ���α׷� ������
; �ҽ��׷�, Ÿ�ϱ׷� ���������������� �Ÿ� ���ϱ�
; �ҽ�, Ÿ�� �׷� �Ÿ��������� ����
; ������� ���� ���� �Ǵ� ����� ����,�Ϸ�
;  ������ ��� : �ҽ��׷쿡�� Ÿ�ٱ׷����� ��������
;  ������� ���
;      - Ÿ�ϰŸ����� �̿��Ͽ� point list������ ali_Pdim�� �θ� �� ���
;      - ������, ����, sidepoint �Է¹���. 
;      - ali_Pdim�Լ��� �ҷ��� �����
;         :textout, prefix, postfix, factor, delta, gol, realdist �ɼ� �Լ������� ó����
;----------------------------------------
(defun c:meq( / sss spt  i slst en eni ipnt tb tb1 tb2 mpnt
	        dst sslst sst tpt tlst stlst index srctxt entl ass1 modtxt co entl1
		ipnt ang p1 p2 dirang sidepnt plst dstsum txt txt0 dsti dstsum factor 
	     	delta prefix postfix oridist gapfactor)


  (meq-dialog)
  
;  (setq #MEQ_create T		;�����/��ġ����
;	#MEQ_modify nil		;��ġ��
;	#MEQ_textout T	;text���/dimension���
;	#MEQ_dimension T		;����ġ����
;	#MEQ_prefix "("		;prefix
;	#MEQ_postfix ")"	;postfix
;	#MEQ_factor 1.0		;factor
;	#MEQ_delta -70		;delta
;	#MEQ_gol nil		;����� ���뿩�� �Ȱ��� ���� ������ @�� ���� ���ΰ�?
;	#MEQ_oridist T		;�������̴��ġ������ ���� ���ΰ�?
;	#MEQ_chval T		;�������� ������ ���� ������ ����.
;  );setq
  
  ;----- �ҽ� text �׷� ����
  (princ "\n���� Text �Ǵ� Dimension ���� �����ϼ���: ")
  (setq sss (ssget '((-4 . "<OR") (0 . "TEXT") (0 . "DIMENSION") (-4 . "OR>"))))
  (setq spt (getpoint "\n���� �������� ��������: "))

  
  ;----- �ҽ��׷� ���������������� �Ÿ� ���ϱ�
  (setq i 0
	slst nil)  ; �Ÿ� ename ���� '((�Ÿ� ename) (�Ÿ� ename))
  
  (repeat (sslength sss)
    (setq en (ssname sss i))	;entity name
    (setq eni (entget en))
    (setq ety (cdr (assoc 0 eni))) 		;entity type
    (if (= ety "TEXT")
      (progn						;text�϶�.
	(setq mpnt (textboxm en))		;�߽���ǥ���ϱ�
        (setq dst (distance mpnt spt))		;���������������� �Ÿ�
      );progn
      (progn
	(setq mpnt (cdr (assoc 11 eni)))	;dimension text�� �߽���
	(setq dst (distance mpnt spt))
      );progn	
    );if  
    (setq slst (append slst (list (list dst en))))
    (setq i (1+ i))
  );repeat  

  
  ;----- �ҽ� �׷� �Ÿ��������� ����
  (setq sslst (vl-sort slst			;sort�� ���� list
		       '(lambda (s1 s2)
    				(< (car  s1) (car s2)))))  ;ù��° ��Ҹ� �������� sort

  ; ������� ���� ���� ����,�Ϸ�
  ; ����ɼ��� ON�� ���
  (if #MEQ_modify	
    (progn
  	;----- ���纻 text �׷� ����
  	(princ "\n���纻 text ���� �����ϼ���: ")
  	(setq sst (ssget '((-4 . "<OR") (0 . "TEXT") (0 . "DIMENSION") (-4 . "OR>"))))
  	(setq tpt (getpoint "\n���纻 �������� ��������: "))

  	;----- �� �׷� ���� üũ, ���� �ٸ��� ���α׷� ������
  	(if (/= (sslength sss) (sslength sst)) (progn (alert "�ؽ�Ʈ ������ �ٸ��ϴ�!") (exit)))

      ;----- Ÿ�ϱ׷� ���������������� �Ÿ� ���ϱ�

      (setq i 0
	tlst nil)  ; �Ÿ� ename ���� '((�Ÿ� ename) (�Ÿ� ename))

      (repeat (sslength sst)
        (setq en (ssname sst i))	;entity name
        (setq eni (entget en))
        (setq ety (cdr (assoc 0 eni))) 		;entity type
        (if (= ety "TEXT")
          (progn						;text�϶�.
	    (setq mpnt (textboxm en))		;�߽���ǥ���ϱ�
            (setq dst (distance mpnt spt))		;���������������� �Ÿ�
          );progn
          (progn
	    (setq mpnt (cdr (assoc 11 eni)))	;dimension text�� �߽���
	    (setq dst (distance mpnt spt))
          );progn	
        );if
	(setq tlst (append tlst (list (list dst en))))
        (setq i (1+ i))
      );repeat

      ;----- Ÿ�� �׷� �Ÿ��������� ����
      (setq stlst (vl-sort tlst			;sort�� Ÿ�� list
		       '(lambda (s1 s2)
    				(< (car  s1) (car s2)))))  ;ù��° ��Ҹ� �������� sort

      ;----- �������
      (setq index 0)
      (repeat (sslength sst)
	(setq enti (entget (cadr (nth index sslst))))		;������ƼƼ ����
	(setq enty (cdr enti))					;entity type
        (setq srctxt (cdr (assoc 1 enti))) ;���� text���� ã��
	(if (and (= enty "DIMENSION") (= srctxt ""))		;default ġ���϶�
          (setq srctxt (rto_dimtxt (cdr (assoc 42 enti))))
	);if
        (setq entl (entget (cadr (nth  index stlst))))         ;���ƼƼ����Ȯ��
        (setq ass1 (assoc 1 entl))                	;���text assoc����Ÿ Ȯ��
	(if #MEQ_chval
	  (setq modtxt (meq_txtopt srctxt #MEQ_prefix #MEQ_postfix #MEQ_factor #MEQ_delta));option����
	  (setq modtxt srctxt)
	);if  
        (setq co (cons 1 modtxt))        		;������ ����text assoc����Ÿ �ۼ�
        (setq entl1 (subst co ass1 entl))         ;���ο� entity�����ۼ�
        (entmod entl1)                            ;���ο� text�� ������Ʈ
        (setq index (1+ index))
      );repeat
    );progn
  );if #MEQ_modify
  
  ;ġ������ ����Ⱑ �������� ���
  ; ������, ����, side�� �Է¹ޱ� --> ����list����� -->  point����Ʈ ����� 
  ; --> dimensin/text ����(f_datext����, option�� f_datext���� �����)
  (if (or (and #MEQ_create #MEQ_textout) (and #MEQ_create #MEQ_dimension))
    (progn
      (setq ipnt (getpoint "\nPick Insert Point: "))  ;�������Է�
      (initget "2P")
      (setq ang (getangle ipnt "\nSpecify Direction angle or [2Point] : "))
      (cond
        ((= ang "2P")				;2p�� �������� �� ���� �Է¹޾� ���� ����
          (setq p1 (getpoint "\nSpecify first point: ")
	        p2 (getpoint p1 "\nSpecify second point: "))
          (setq dirang (angle p1 p2)) 		;������� ���� ���
        );subcond
        ; line�� �̿��� refrence�� line�� ������ 0,180�� �ΰ����� ������ ������ UCS��ȯ��
        ; �Է��� �޾ƾ���. ���� ����ڰ� cancel�� ������ �� WCS�� �����ϴ� erroró����
        ; �ؾ��ϹǷ� �̹��۾����� ������.
;      ((= ang "Object")
;        (setq rent (entget (car (entsel "\nSelect Reference Object(Only Line): "))))
;        (setq p10 (cdr (assoc 10 rent))   ;������;
;	      p11 (cdr (assoc 11 rent)))  ;����
;        (setq enang (angle (cdr (assoc 10 ;line�� ����
;      );subcond
        ((/= ang "2P")
          (setq dirang ang)
        );subcond
      );cond
    
      (setq sidepnt (getpoint "\nPick side point: "))  ;side point�Է¹���.
    
      ;------ ����list����� �� pointlist�����
      (setq plst nil
  	  plst (append plst (list ipnt))  ;ù���� 0,0,0 ����
  	  dstsum 0)				;�����Ÿ��� 0����
      (foreach dst sslst   ;dst '(�Ÿ� ename)
	(setq enti (entget (cadr dst)))			;entity ����
	(setq enty (cdr (assoc 0 enti)))		;entity type
        (setq txt (cdr (assoc 1 enti)))  		;ġ�� ����
	(if (and (= enty "DIMENSION") (= txt ""))       ;ġ���� demension�̸� default("")�ϰ��
          (setq txt (rto_dimtxt (cdr (assoc 42 enti))))
	);if  
        (if (and (= "(" (substr txt 1 1)) (= ")" (substr txt (strlen txt) 1))) 
          (setq txt0 (substr txt 2 (- (strlen txt) 2)))  ;( )�� �ִ� ��� ( ) ����
	  (setq txt0 txt)				;( )�� ���� ��� ���� text�״��...
        );if
        (setq dsti (djdg_dimtxtoreal txt0))		;text�� �ش��ϴ� �Ÿ�.
        (setq dstsum (+ dstsum dsti))			;�����Ÿ� ���ϱ�
        (setq plst (append plst			;�����Ÿ��� plis�� �߰�.
			   (list (polar ipnt dirang dstsum))))
      );foreach
      ;(ali_Pdim plst defpnt dirpnt sidepnt textout factor delta prefix postfix oridist gol
      (if #MEQ_chval (setq factor #MEQ_factor
			    delta #MEQ_delta
			    prefix #MEQ_prefix
			    postfix #MEQ_postfix
			    oridist #MEQ_oridist)
		     (setq factor 1.0
			    delta 0.0
			    prefix ""
			    postfix ""
			    oridist T)
	);if
      (ali_Pdim plst ipnt (polar ipnt dirang 100) sidepnt
		#MEQ_textout factor delta prefix postfix oridist #MEQ_gol)
    );progn  
  );if #MEQ_create

  ;---- text list�� ���鶧...
  (if (and #MEQ_create #MEQ_txtlist)
    (progn
      (setq gapfactor  1.25)					;�ٰ��� factor
      (setq th (getvar "dimtxt"))				;���ڳ���
      (setq ipnt (getpoint "\nPick insert point: "))		;������ �Ϻ�����
      (setq index 0)
      (repeat (length sslst)
	(setq enti (entget (cadr (nth index sslst))))		;������ƼƼ ����
	(setq enty (cdr enti))					;entity type
        (setq srctxt (cdr (assoc 1 enti))) ;���� text���� ã��
	(if (and (= enty "DIMENSION") (= srctxt ""))		;default ġ���϶�
          (setq srctxt (rto_dimtxt (cdr (assoc 42 enti))))
	);if
;        (setq entl (entget (cadr (nth  index stlst))))         ;���ƼƼ����Ȯ��
;        (setq ass1 (assoc 1 entl))                	;���text assoc����Ÿ Ȯ��
	(if #MEQ_chval
	  (setq modtxt (meq_txtopt srctxt #MEQ_prefix #MEQ_postfix #MEQ_factor #MEQ_delta));option����
	  (setq modtxt srctxt)
	);if
;        (setq co (cons 1 modtxt))        		;������ ����text assoc����Ÿ �ۼ�
;        (setq entl1 (subst co ass1 entl))         ;���ο� entity�����ۼ�
;        (entmod entl1)                            ;���ο� text�� ������Ʈ
	(command "text" (list (car ipnt) (- (cadr ipnt) (* index th gapfactor)))
		 (* (getvar "dimscale") th) "0"
		 modtxt)
        (setq index (1+ index))
      );repeat
    );progn
  );if
;  (princ "OK")
);defun meq


;---------------------
; function: meq_txtopt
;           �����ؽ�Ʈ�� meq�� �ɼ��� �����Ͽ� �����ش�.
;---------------------
(defun meq_txtopt( txt prefix postfix factor delta / )
;  (setq 
;	prefix "("		;prefix
;	postfix ")"	;postfix
;	factor 1.0		;factor
;	delta -70		;delta
;  );
  (if (and (= "(" (substr txt 1 1)) (= ")" (substr txt (strlen txt) 1)))
    (setq txt0 (substr txt 2 (- (strlen txt) 2)))  ;( )�� �ִ� ��� ( ) ����
    (setq txt0 txt)					;���� ��� �������...
  );if
  (setq dst (djdg_dimtxtoreal txt0))	;�������� ����
  (setq fdst (* factor dst))		;factor����
  (setq dfdst (+ delta fdst))		;delta����
  (setq txt1 (rto_dimtxt dfdst))	;�������̸�  text��
  (setq ftxt (strcat prefix txt1 postfix))
)  ;defun

;--------------------------
; function : meq-dialog
;           06/08/05
;--------------------------
(defun meq-dialog( / dcl_id lay )  
  
  (setq dcl_id (load_dialog "DJDG"))                 ;ddscl.dcl load  
  (if (not (new_dialog "MEQ" dcl_id)) (exit))         ;ddscl.dcl ���� MEQ
  
  ; �ʱ�ȭ
  ;; 
  (if (and (= #MEQ_create nil) (= #MEQ_modify nil))
    (progn (setq #MEQ_modify T) (set_tile "create" "0") (set_tile "modify" "1"))
    (if (= #MEQ_create T) (progn (set_tile "create" "1") (mode_tile "createbox" 0))
 		          (progn (set_tile "modify" "1") (mode_tile "createbox" 1)))
  );if
  (if (and (= #MEQ_textout nil) (= #MEQ_dimension nil) (= #MEQ_txtlist nil))
    (progn (setq #MEQ_textout T) (set_tile "text" "1") (set_tile "dimension" "0"))
    (if (= #MEQ_textout T)
      (set_tile "text" "1")
      (if (= #MEQ_dimension T)
	(set_tile "dimension" "1")
	(set_tile "txtlist" "1")
      );if
    );if  
  );if  
  (if #MEQ_chval (progn (set_tile "chval" "1") (mode_tile "chvalbox" 0))
    		 (progn (set_tile "chval" "0") (mode_tile "chvalbox" 1)))
  (if (= #MEQ_prefix nil) (set_tile "pref" "") (set_tile "pref" #MEQ_prefix))
  (if (= #MEQ_postfix nil) (set_tile "postf" "") (set_tile "postf" #MEQ_postfix))
  (if (= #MEQ_factor nil) (set_tile "factor" "1.0") (set_tile "factor" (rtos #MEQ_factor 2 3)))
  (if (= #MEQ_delta nil) (set_tile "delta" "0.0") (set_tile "delta" (rtos #MEQ_delta 2 3)))
  (if (= #MEQ_oridist nil)  (set_tile "oridist" "0") (set_tile "oridist" "1"))
  (if (= #MEQ_gol nil) (set_tile "gol" "0") (set_tile "gol" "1"))
  
  
  (action_tile "create" "(set_val $key)")               
  (action_tile "modify" "(set_val $key)")           
  (action_tile "text" "(set_val $key)")           
  (action_tile "dimension" "(set_val $key)")
  (action_tile "txtlist" "(set_val $key)")
  (action_tile "chval" "(set_val $key)")
  (action_tile "prefix" "(set_val $key)")
  (action_tile "postfix" "(set_val $key)")
  (action_tile "factor" "(set_val $key)")
  (action_tile "delta" "(set_val $key)")
  (action_tile "gol" "(set_val $key)")
  (action_tile "oridist" "(set_val $key)")
  
  (action_tile "accept" "(do-dialog)")                
  (action_tile "cancel" "(do-cancel)")                
  
;  (mode_tile "th" 2)  
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
     ((= key "create") (if (= 1 val) (setq #MEQ_create T #MEQ_modify nil))
      			(if #MEQ_create (mode_tile "createbox" 0) (mode_tile "createbox" 1)));subcond
     ((= key "modify") (if (= 1 val) (setq #MEQ_modify T #MEQ_create nil))
      			(mode_tile "createbox" 1))
     ((= key "text") (if (= 1 val) (setq #MEQ_textout T #MEQ_dimension nil #MEQ_txtlist nil)))
     ((= key "dimension") (if (= 1 val) (setq #MEQ_dimension T #MEQ_textout nil #MEQ_txtlist nil)))
     ((= key "txtlist") (if (= 1 val) (setq #MEQ_txtlist T #MEQ_textout nil #MEQ_dimension nil)))
     ((= key "chval") (setq #MEQ_chval (= 1 val))
      		      (if #MEQ_chval (mode_tile "chvalbox" 0) (mode_tile "chvalbox" 1)))	
     ((= key "pref") (setq #MEQ_prefix (get_tile key)))
     ((= key "postf") (setq #MEQ_postfix (get_tile key)))
     ((= key "factor") (setq #MEQ_factor val))
     ((= key "delta") (setq #MEQ_delta val))
     ((= key "gol") (setq #MEQ_gol (= 1 val)))
     ((= key "oridist") (setq #MEQ_oridist (= 1 val)))
   );cond
);defun

(defun do-dialog()    ;��� �� ���������� �ֱ�
  (set_val "create")
  (set_val "modify")
  (set_val "text")
  (set_val "dimension")
  (set_val "txtlist")
  (set_val "chval")
  (set_val "pref")
  (set_val "postf")
  (set_val "factor")
  (set_val "delta")
  (set_val "gol")
  (set_val "oridist")
  (done_dialog) ;dialog����
  (unload_dialog dcl_id)
);defun

(defun do-cancel()
  (unload_dialog dcl_id)
  (exit)
);  