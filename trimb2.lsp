;*******************************************
; Program : TRIMB2
;           TRIM B2
;           By Suk-Jong Yi
;           1995/7/13
;*******************************************
; �� �����̳� ȣ ������ ��ü�� TRIM

(defun C:TRIMB2(/
)

;(push-env)
  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

(setq ent1 (entget (car (entsel "\nSelect first entity: "))))  ;���ؼ� ����
(setq ent2 (entget (car (entsel "\nSelect second entity: "))))
(if (= offdst nil)
  (setq offdst_old 0.0)
  (setq offdst_old offdst)
) ;of IF
(princ "\nOffset distance<")
(princ offdst_old)
(setq offdst (getreal "\>: "))
(if (= offdst nil) (setq offdst offdst_old))

(while (setq p1 (getpoint "\nPick first point: "))         ;©���� ��ƼƼ ����
  (setq p2 (getpoint p1 "\nPick second point: "))
  (setq ssent (ssget "F" (list p1 p2)))                 ;ssget "F"�ɼ�
  (setq ssnum (sslength ssent))                         ;ssget�� ��ƼƼ ����
  (setq bent1 (entget (ssname ssent 0)))
  (setq bent2 (entget (ssname ssent 1)))

  (setq ent1s (cdr (assoc 10 ent1))                   ;���ؼ�1�� ���۰� ����
        ent1e (cdr (assoc 11 ent1))
        ent2s (cdr (assoc 10 ent2))                   ;���ؼ�2�� ���۰� ����
        ent2e (cdr (assoc 11 ent2)))

  (setq bent1s (cdr (assoc 10 bent1))               ;ù° line break
        bent1e (cdr (assoc 11 bent1)))
  (setq crsp1s (inters ent1s ent1e bent1s bent1e))
  (setq crsp1e (inters ent2s ent2e bent1s bent1e))
  (setq ang (angle crsp1s crsp1e))
  (setq brk1s (polar crsp1s (+ pi ang) offdst))
  (setq brk1e (polar crsp1e ang offdst))

  (setq bent2s (cdr (assoc 10 bent2))               ;��° line break
        bent2e (cdr (assoc 11 bent2)))
  (setq crsp2s (inters ent1s ent1e bent2s bent2e))
  (setq crsp2e (inters ent2s ent2e bent2s bent2e))
  (setq ang (angle crsp2s crsp2e))
  (setq brk2s (polar crsp2s (+ pi ang) offdst))
  (setq brk2e (polar crsp2e ang offdst))

  (command "BREAK" (list (ssname ssent 0) crsp1s) "F" brk1s brk1e)
  (command "BREAK" (list (ssname ssent 1) crsp2s) "F" brk2s brk2e)
  (if (/= offdst 0.0)
    (progn
      (command "LINE" brk1s brk2s "")
      (command "LINE" brk1e brk2e "")
    ) ;of progn
  ) ;of IF
) ;of while

;(pop-env)

  (setq *error* oer seterr nil)
(princ)
) ;of defun



;--------------------------
; Program : mtrimb
;	    Multi Trimb2
;		���������� ��ġ���� ���򺸰��� �߶��ֱ�
; 		06/08/11
;--------------------------
; ����������, ���򺸰��� ����
; ���򺸰��� �׷캰�� ������ ã��
; ������� �׸���. (trim ����� ������ ���� �ƴ�, trim�� �Ǹ� ���� entity�� ���ϱ� ����)
; 
(defun c:mtrimb( / ssvf ssv sshf ssh index elist selist cplist dsten hen eni a10 a11 i
		clist veni v10 v11 fpnt sclist gap h1 cahplist hplist ang
		ang1 chplist p pt hs p1 p2 id id1 plist1 plist2 nv iv ihs dst dstxt )
  
  (setq #MTR_th	    2.5
     	#MTR_hlayer "0"
	#MTR_vlayer "0"
	#MTR_pref "H-stiff 13x340x"
	#MTR_wlen T
  	#MTR_gap 35
	)

  (MTR-DIALOG)				;dialog�Է�
  
  ;----- ���� ������ setting
  (setq th (getvar "dimtxt")					;text���� dimscale������
        thds (* (getvar "dimscale") th)				;text����(dimscale����)
	tgap (* 1.5 thds))					;���ؼ����� text���� gap 

  ;----���������� �Է¹ޱ�
  (princ "\n���������縦 �����ϼ���: ")
  (if (= #MTR_vlayer "ALL")				;ALL�̸� layer������� ��� ����
    (setq ssvf '((0 . "LINE")))	;line��ƼƼ�� filter
    (setq ssvf (list (cons 0 "LINE") (cons 8 #MTR_vlayer)))
  );if  
  (setq ssv (ssget ssvf))	;���������� ����
  
  ;---- ���򺸰��� �Է¹ޱ�
  (princ "\n���򺸰��縦 �����ϼ���: ")
  (if (= #MTR_hlayer "ALL")  				;All�̸� layer������� ��� ����
    (setq sshf '((0 . "LINE")))	;line��ƼƼ�� filter
    (setq sshf (list (cons 0 "LINE") (cons 8 #MTR_hlayer)))
  );if  
  (setq ssh (ssget sshf))	;���򺸰��� ����
  
  (setq index 0)
  (setq elist nil)		;���򺸰��� entity list
  (repeat (sslength ssh)
    (setq elist (append elist (list (ssname ssh index))))
    (setq index (1+ index))
  );repeat


  ;���򺸰��� �� entity sort
  (setq selist (entity-sort elist 10 nil)); entity sort

  (setq cplist nil);������ list)
  (foreach dsten selist
    (setq hen (cadr dsten))	;���򺸰��� entity name
    (setq eni (entget hen))
    (setq a10 (cdr (assoc 10 eni))
	  a11 (cdr (assoc 11 eni)))
    (setq i 0)
    (setq clist nil)		;���������� ������ list
    (setq clist (append clist (list a10))	;�������� ���� �߰�
	  clist (append clist (list a11)))
    (repeat (sslength ssv)
      (setq veni (entget (ssname  ssv i)))
      (setq v10 (cdr (assoc 10 veni))
	    v11 (cdr (assoc 11 veni)))
      (setq clist (append clist (list (inters a10 a11 v10 v11)))) ;������list
      (setq i (1+ i))			;���� ���������缱����.
    );repeat
    (setq fpnt (farest clist));
    (setq sclist (point-sort clist 3 nil))		;sort�� point list
	  
    (setq cplist (append cplist (list sclist)))	;���򺸰��纰 list
    (setq i (1+ i))
  );foreach

  ; �߶��� plist�����

  
  (setq h1 0)
  (setq cahplist nil)		;�߷��� ���򺸰��纰 list, �߷��� ���򺸰��� point list�� ����
  (foreach hplist cplist
    (setq ang (angle (nth 0 hplist) (nth 1 hplist)))	;������� ����
    (setq ang1 (+ ang pi))				;�ݴ���� ����
    (setq chplist nil)		;�߷��� ���򺸰��� point list
    (setq chplist (append (list (nth 0 hplist))))  ;ù point �߰�
    (setq index 1)
    (repeat (- (length hplist) 2)
      (setq p (nth index hplist))
      (if (/= (rem index 2) 0) 		;1,3,5,7,9�϶�.
        (setq pt (polar p ang1 #MTR_gap))	;�ݴ�������� ����
        (setq pt (polar p ang #MTR_gap))	;2,4,6,8,10 �϶� ���� �������λ���
      );if
      (setq chplist (append chplist (list pt)))
      (setq index (1+ index))
    );repeat
    (setq chplist (append chplist (list (nth (1- (length hplist)) hplist)))) 	;������ �� �߰�
    (setq cahplist (append cahplist (list chplist)))				;��ü list�� �߰�.
  );foreach

  ;----- ���� line �׸���.
  (setq ihs 0)
  (foreach hs cahplist	;�� ���򺸰��� ������ �׸���
    (setq index 0)
    (repeat (fix (/ (length hs) 2))	;��ü point ���� / 2 �� �ݺ�����
      (setq p1 (nth (* 2 index) hs)
	    p2 (nth (+ (* 2 index) 1) hs))

      (push-os)
        (command "line" p1 p2 "")
      (pop-os)
      (if (and #MTR_wlen (= (rem ihs 2) 0))	;���̾��� �ɼ��� ON�̰� 0,2,3,5��° h-line�ΰ��
	(progn
          (setq dst (distance p1 p2)) ;������ ����.
          (setq dstxt (rto_dimtxt dst))
          (setq dstxt (strcat #MTR_pref dstxt))	;prefix����
          (djdg_wtxtonline p1 p2 dstxt thds tgap)
	);progn
      );if	
      (setq index (1+ index))      
    );repeat
    (setq ihs (1+ ihs))				;���� h-line����
  );foreach  

  ;------ ����line �׸���...
  (setq index 0)
  (repeat (fix (/ (length cahplist) 2))  ;���򺸰��� ������ /2 ��ŭ �ݺ�
    (setq id (* 2 index))		;line index 0,2,4,6
    (setq id1 (1+ id))			;line index 1,,3,5,7
    (setq plist1 (nth id cahplist)
	  plist2 (nth id1 cahplist))
    (setq nv (- (length plist1) 2))
    (setq iv 1)
    (repeat nv				;�߶��� ����ŭ �ݺ�
      (setq p1 (nth iv plist1)
	    p2 (nth iv plist2))
      (push-os)
        (command "LINE" p1 p2 "")
      (pop-os)
      (setq iv (1+ iv))
    );repeat  
    (setq index (1+ index))		;���� line����
  );repeat
  
  (setq i 0)
  (repeat (sslength ssh)
    (entdel (ssname ssh i))
    (setq i (1+ i))
  );repeat  
);defun



(defun MTR-DIALOG( /  
                  dcl_id lay )  

  (defun mtrset_val(key / val )
    (setq val (atof (get_tile key)))
     (cond
       ((= key "mtrth") (setq #MTR_th val))
       ((= key "mtrpref") (setq #MTR_Pref (get_tile key)))
       ((= key "mtrgap") (setq #MTR_gap val))
       ((= key "mtrwlen") (setq #MTR_wlen val))
       ((= key "mtrvlaylst") (setq #MTR_vlayer (nth (fix val)  #laylst)));sub cond
       ((= key "mtrhlaylst") (setq #MTR_hlayer (nth (fix val)  #laylst)));sub cond        
     );cond
  );defun

  (defun mtrdo-dialog()    ;��� �� ���������� �ֱ�
    (mtrset_val "mtrth")
    (mtrset_val "mtrpref")
    (mtrset_val "mtrgap")
    (mtrset_val "mtrwlen")
    (mtrset_val "mtrvlaylst")
    (mtrset_val "mtrhlaylst")
    (done_dialog) ;dialog����
    (unload_dialog dcl_id)
  );defun

  (defun mtrdo-cancel( )
    (unload_dialog dcl_id)
    (exit)
  );defun

  ;--------------------------
  ;--- MTR-Dialog main ----
  ;--------------------------
  
  (setq dcl_id (load_dialog "DJDG"))                 ;ddscl.dcl load  
  (if (not (new_dialog "MTRIMB" dcl_id)) (exit))         ;ddscl.dcl ���� WCPL
  
  ; �ʱ�ȭ
  ;; list���̾�α׿� layer list �ѷ��ֱ�
  (setq #laylst nil)
  (setq #laylst (append #laylst (list "ALL")))
  (setq lay (tblnext "layer" T))
  (setq #laylst (append #laylst (list (cdr (assoc 2 lay)))))
  (while (setq lay (tblnext "layer"))
    (setq #laylst (append #laylst (list (cdr (assoc 2 lay)))))
  );while
  
  ; ���������� layer���� list�� �߰�  
  (start_list "mtrvlaylst")
  (mapcar 'add_list #laylst)
  (end_list)
  ; ���򺸰��� layer���� list�� �߰�  
  (start_list "mtrhlaylst")
  (mapcar 'add_list #laylst)
  (end_list)

  (if (= nil #MTR_th) (set_tile "mtrth" (rtos (getvar "dimtxt") 2 1))
    		     (set_tile "mtrth" (rtos #MTR_TH 2 1))) ;text���� �ʱ�ȭ
  (if (= nil #MTR_Pref) (set_tile "mtrpref" "")
    		     (set_tile "mtrpref" #MTR_Pref)) ;Prefix �ʱ�ȭ
  
  (if (= #MTR_vlayer nil) (set_tile "mtrvlaylst" "0")
      (set_tile "mtrvlaylst" (itoa (vl-position #MTR_vlayer #laylst)))); layer list�ʱ�ȭ
  (if (= #MTR_hlayer nil) (set_tile "mtrhlaylst" "0")
      (set_tile "mtrhlaylst" (itoa (vl-position #MTR_hlayer #laylst)))); layer list�ʱ�ȭ
    
  (if (= nil #MTR_gap) (set_tile "mtrgap" "35")
    		     (set_tile "mtrgap" (rtos #MTR_gap 2 3))) ;gap �ʱ�ȭ  
  (if (= nil #MTR_wlen) (set_tile "mtrwlen" "0")
    		     (set_tile "mtrwlen" (if #MTR_wlen "1" "0"))) ;���̾��� �ʱ�ȭ  
  
  (action_tile "mtrth" "(mtrset_val $key)")               
  (action_tile "mtrpref" "(mtrset_val $key)")           
  (action_tile "mtrgap" "(mtrset_val $key)")           

  (action_tile "wc2laylst" "(mtrset_val $key)") ;layer setting
  (action_tile "wc2laylst" "(mtrset_val $key)") ;layer setting

  (action_tile "mtrwlen" "(mtrset_val $key)")
  
  (action_tile "accept" "(mtrdo-dialog)")                
  (action_tile "cancel" "(mtrdo-cancel)")                
  
  (mode_tile "mtrgap" 2)  
  (start_dialog)  
  (unload_dialog dcl_id)  
  (princ)
  
) ;of defun TRIMB2-dialog



;--------------------------
; function : entity�� Ư�� dxf�ڵ�(��ǥ), Ư�� ���������κ����� �Ÿ��� �������� sort
; 		06/08/11
;--------------------------
; enlist : entity name list
; ass : assoc���� �ڵ�..(ex: �����̸� 10)
; ipt : ������	
;--------------------------
(defun entity-sort(enlist ass ipt
		   / ne plist en farpnt ipt pnt dstenlist dst sdstenlist)
  (setq ne (length enlist))
  (setq plist nil)
  (foreach en enlist
    (setq plist (append plist (list (cdr (assoc ass (entget en))))))
  );foreach
  (setq farpnt (farest plist))		;���� �� �� ã��

  (if (= ipt nil)			;���� �������� �־����� ������.
    (setq pnt (car farpnt))		;����� �� �ΰ��� �տ����� ����������...
    (setq pnt ipt)			
  );  
  (setq dstenlist nil)
  (foreach en enlist
    (setq dst (distance pnt (cdr (assoc ass (entget en)))))
    (setq dstenlist (append dstenlist (list (list dst en))))
  );foreach

  (setq sdstenlist (vl-sort dstenlist '(lambda (s1 s2)	;�������������� �Ÿ��� sort
    				(< (car  s1) (car s2)))))
  
);

;--------------------------
; function : point list�� Ư�� ����(x,y,z,�Ÿ�)�� �������� sort
; 		06/08/11
;--------------------------
; ptlist : point list
; idx : assoc���� �ڵ�..(ex: �����̸� 0:x, 1:y, 2:z 3:�Ÿ�)
; ipt : ������(idx�� 3�϶��� ���ȴ�) nil�̸� ����������� �������κ��� �Ÿ��� sort
;--------------------------
(defun point-sort(plist idx ipt
		   / return farpnt ipt pnt dstlist p dst dstplist sdstplist dp )
;  (setq ne (length plist))

  (cond
    ((< idx 3)
     (setq return (vl-sort plist '(lambda (s1 s2)
				    (< (nth idx  s1) (nth idx s2)))))
				    
    );subcond
    ((= idx 3)
      (setq farpnt (farest plist))		;���� �� �� ã��
      (if (= ipt nil)			;���� �������� �־����� ������.
        (setq pnt (car farpnt))		;����� �� �ΰ��� �տ����� ����������...
        (setq pnt ipt)			
      );  
      (setq dstplist nil)
      (foreach p plist
        (setq dst (distance pnt p))
        (setq dstplist (append dstplist (list (list dst p))))
      );foreach

      (setq sdstplist (vl-sort dstplist '(lambda (s1 s2)	;�������������� �Ÿ��� sort
    				(< (car  s1) (car s2)))))
      (setq return nil)
      (foreach dp sdstplist
	(setq return (append return (list (cadr dp))))
      );foreach	
    );subcond
    
  );cond 
);