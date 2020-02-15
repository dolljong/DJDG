;----------------------
; Program : sscl
;           Sapdo Scale
;           Yi Suk Jong
;           04/10/26
;   ����½� ũ�⸦ �Է��ϰ� ���̸�����ָ� �ؽ�Ʈ ũ�⸦ �󸶷� �ؾߵ��� �˷��ش�.
; text�� ũ�⸦ �����ϸ� ��½� ���ϴ� �ؽ�Ʈ ũ�Ⱑ �ǵ��� text����(text�� ���)��
; dimscale(dimension�� ���)�� �����ش�.
; 05/08/07 xdata�� ���� dim���� ���밡���ϵ��� ������.
;----------------------
;
(defun c:sscl(
	       /  point1 point2 hlen vlen indexstar wsize hsize bbox hbox hratio vratio
	          scale ntexth2 txt ss1 nent i
	         ent etype ass40 newass40 fnd dimtxt txth newdimscl newent)
  
  (setq prnth (getrealold prnth "\nFont size: "))

  (setq point1 (getpoint "\nPick first point: "))
  (setq point2 (getcorner point1 "\nPick Second point: "))

;  (setq dist (abs (- (car point2 ) (car point1))))	

  (setq hlen (abs (- (car point2 ) (car point1))))   ;�������
  (setq vlen (abs (- (cadr point2 ) (cadr point1)))) ;��������
  
  ;����� box size�� �Է¹���.
  (setq sizetxt (getstringold sizetxt "Enter size(B*H, Unit mm): "))  

    
  (setq indexstar (vl-string-search "*" sizetxt))  ; *��ġ
  (setq wsize (substr sizetxt 1 indexstar))
  (setq hsize (substr sizetxt (+ 2 indexstar) (- (strlen sizetxt) 1 indexstar)))  
  (setq bbox (atof wsize)	;��
	hbox (atof hsize))      ;����
  
  ; hscale�� vscale�� ���ؼ� ���� scale�� ����
  (setq hratio (/ bbox hlen)
	vratio (/ hbox vlen))

  (if (<= hratio vratio)
    (setq scale (/ 1 hratio))		;��½�ũ��/����ũ�� �� ���� ���� scale���� ���� scale������..
    (setq scale (/ 1 vratio))
  );of if


  ;���ο� textũ�� ����.

  (setq ntexth2 (* scale prnth))  ;���ο� textũ�� scale * 2.0

  (setq txt (strcat "Print scale= 1:" (rtos scale 2 3) ",Printed Text Height"
		    (rtos prnth 2 1) ": Drawing Height" (rtos ntexth2 2 3) ))
  (print txt)
  (princ "\n")
  
  (setq ss1 (ssget '((-4 .   "<OR")   ;text, mtext, dimension ����
		     ( 0 .  "TEXT")
		     ( 0 . "MTEXT")
		     ( 0 . "DIMENSION")
		     (-4 .   "OR>"))))  
  
  (setq nent (sslength ss1))   ;��ƼƼ ����
  (setq i 0)
  (repeat nent    ;��ƼƼ ������ƴ �ݺ�
    (setq ent (entget (ssname ss1 i)))
    (setq etype (cdr (assoc 0 ent)))  ;entity type����
    (cond
       ((or (= etype "TEXT") (= etype "MTEXT"))
	  (princ "TEXT")
           (setq ass40 (assoc 40 ent))
;           (setq newTH (* Hscale (cdr ass1)))
           (setq newass40 (cons 40 ntexth2))
           (setq nent (subst newass40 ass40 ent))
           (entmod nent)                       ;���ο� ũ��� ������Ʈ
       );sub cond
       ((= etype "DIMENSION")

	  (setq ent (entget (ssname ss1 i) '("ACAD")))   ;xdata���� entity ����
	  (if (> (length (cdr (cadr (assoc -3 ent)))) 0) (setq hasxd T) (setq hasxd nil)) ;x-data�� �ִ��� Ȯ��
	  (if hasxd
	    (progn			;-- x-data���� ������ �ִ� ���
          	(setq fnd (djdg_findxdata ent 1070 40))        ;findxdata 1070 40 : dimscale
	  	(if (= fnd nil) (setq dimscl 1) (setq dimscl (cdr (nth 2 fnd))))  ;dimscale ���� nil�̸� 1.0����
	  	(setq dimtxt (cdr (nth 2 (djdg_findxdata ent 1070 140))))         ;dimtxt�� ����
          	(setq txth (* dimscl dimtxt))  ;text���� = dimscale x dimtxt
	  	(setq newdimscl (/ (* scale prnth) dimtxt))   ; newdimscale = print_text_height x scale / dimtxt
	  	(setq newent (djdg_chxdata ent 1070 40 newdimscl))  ;���ο� dimscale���� ����� entity���� ����
	  	(entmod newent)
	  	(princ "DIM")
	    );progn
	    (progn			;-- x-data�� ���� ���
                ;dimstyle table���� dimtxt�� ã��
                (setq dimtxt (cdr (assoc 140 (tblsearch "dimstyle" (cdr (assoc 3 ent))))))
	        (setq newdimscale (/ (* scale prnth) dimtxt))
	    	(setq newent (djdg_chxdata ent 1070 40 newdimscale))  ;dimscale���� �߰��ϱ�
	    	(entmod newent)

	    );progn
	  );if  
       );sub cond

    );cond  
    (setq i (1+ i))
  );  
);defun

;Command: (assoc -3 (entget (car (entsel)) '("ACAD")))
;Select object: (-3 ("ACAD" (1000 . "DSTYLE") (1002 . "{") (1070 . 40) (1040 . 1.6)
;(1070 . 41) (1040 . 0.8) (1070 . 42) (1040 . 10.0) (1070 . 43) (1040 . 10.0)
;(1070 . 44) (1040 . 2.0) (1070 . 45) (1040 . 0.0005) (1070 . 73) (1070 . 0)
;(1070 . 74) (1070 . 0) (1070 . 77) (1070 . 1) (1070 . 140) (1040 . 2.5) 
;(1070 . 144) (1040 . 0.001) (1070 . 146) (1040 . 0.072) (1070 . 147) (1040 . 1.25)
;(1070 . 172) (1070 . 1) (1070 . 173) (1070 . 1) (1070 . 174) (1070 . 1) 
;(1070 . 176) (1070 . 1) (1070 . 177) (1070 . 1) (1070 . 178) (1070 . 7) (1070 . 179)
;(1070 . 3) (1070 . 271) (1070 . 3) (1070 . 272) (1070 . 3) (1070 . 279) 
;(1070 . 2) (1070 . 340) (1005 . "1D") (1070 . 341) (1005 . "1C") (1070 . 343) 
;(1005 . "1C") (1070 . 344) (1005 . "1C") (1002 . "}")))

; -------------------------
; fundtion : djdg_chxdata
;            change xdata
;            Yi Suk Jong
;--------------------------
; Argument
;   ent : xdata�� ������ entity����
;   idx1 : index �ٲٰ����ϴ� index
;   idx2 : index �ٲٰ����ϴ� index
;   v : ã�� item�� ���� item�� ���� ��
;   ���� ã�� item�� ������ (idx1 idx2) (1040 v) �� (1002 . "{")�ڿ� �߰��Ѵ�.
; Return
;   entity data
(defun djdg_chxdata(ent idx1 idx2 v / xd finallst l return foundflag i idf idf1 newxd oldxd ent)
   (setq xd (cdr (cadr (assoc -3 ent))))
   (setq finallst nil
         l (length xd)  ;xdata����
         return nil 
	 foundflag nil  ;ã�� ��� nil�� ����
	 i 0 )  ;ù item����
   (setq idf (car (djdg_findxdata ent idx1 idx2)))   ;ã�� item�� nth
   (if (= idf nil)    
     (progn    ;xdata�� ã�� data (1070 . 40)�� ������.
       ; x-data�� ���� ���Ȯ�� x-data�� ���� ��쿡�� �� xdata�߰� �� ����
       ; �߰������� (-3 ("ACAD" (1000 . "DSTYLE") (1002 . "{")  (1002 . "}")))
       (if (= xd nil)
	 (progn
	     (setq ent (append ent '((-3 ("ACAD" (1000 . "DSTYLE") (1002 . "{")  (1002 . "}"))))))  ;�� x-data�߰�
	     (setq xd (cdr (cadr (assoc -3 ent))))  ;x-data����
	     (setq l (length xd))   ;x-data�� ����
	 );progn    
       );if
       (setq idf1 (car (djdg_findxdata ent 1002 "{")))  ;(1002 . "{")��ġã��
       (setq i 0)
       (repeat  (1+ idf1)     ;(1002 . "{")���� �߰�
         (setq finallst (append finallst (list (nth i xd))))  ;frontlist�� �߰�
         (setq i (1+ i))  ;���� i��
       );repeat
       (setq finallst (append finallst (list (cons idx1 idx2)) (list (cons 1040 v))))
       (setq i (+ idf1 1))                 ;ã�� item ���� ���� �ͺ��� ���ϱ�
       (repeat (- (1- l) idf1)
         (setq finallst (append finallst (list (nth i xd))))
         (setq i (1+ i))  ;���� item����
       );repeat
       
     );progn
     (progn	;xdata�� ã�� data (1070 . 40) �� ���� ��.
       (setq i 0)
       (repeat (1+ idf)
         (setq finallst (append finallst (list (nth i xd))))  ;frontlist�� �߰�
         (setq i (1+ i))  ;���� i��
       );repeat

       (setq finallst (append finallst (list (cons (car (nth (1+ idf) xd)) v)))) ;ã�� item���� item �հ��� v�߰�
       (setq i (+ idf 2))                 ;ã�� item ���� ���� �ͺ��� ���ϱ�
       (repeat (- (1- l) idf 1)
         (setq finallst (append finallst (list (nth i xd))))
         (setq i (1+ i))  ;���� item����
       );repeat
     );progn
   );if
  
   (setq newxd (list -3 (append '("ACAD") finallst)))
   (setq oldxd (assoc -3 ent))
   (setq ent (subst newxd oldxd ent))
;  newxd
);defun


; -------------------------
; fundtion : djdg_findxdata
;            change xdata
;            Yi Suk Jong
;--------------------------
; Argument
;   ent : xdata�� ���Ե� entity����
;   inx1,idx2 : index �ٲٰ����ϴ� index1, 2 (indx1 . indx2)
; Retrun
;   (i (indx1 . indx2) (   .   )) ; ���°item�ΰ��� �ش� item�� �� �� item �Ѱ���
; ��뿹:
;   (djdg_findxdata ent 1070 40)  ; xdata���� (1070 . 40)�� �ִ� ��ġ�� ã���ش�.
(defun djdg_findxdata(ent idx1 idx2 / xd l return foundflag i nthv )
   (setq xd (cdr (cadr (assoc -3 ent))))
   (setq l (length xd)  ;xdata����
         return nil 
	 foundflag nil  ;ã�� ��� nil�� ����
	 i 0 )  ;ù item����
   (while (and (<= i (1- l)) (= foundflag nil))   ;i�� ��ü�������� �۰�, ���� ã�� ���������� �ݺ�
     (setq nthv (nth i xd))  ;i��° item ��.
     (if (and (= (car nthv) idx1) (= (cdr nthv) idx2)) ; ù��° item�� idx1�� ���� �ι�° item�� idx2�� ������
       (progn
	 (setq return (list i (nth i xd) (nth (1+ i) xd))) ;������ ��
         (setq foundflag T) ;ã���ɷ� ǥ��
       );progn  
     );if
     (setq i (1+ i))  ;���� item����
   );while
  return
);defun


;;;(defun te2()
;;;  (djdg_findxdata (entget (car (entsel)) '("acad")) 1070 42)
;;;);defun
;;;
;;;(defun te3()
;;;  (djdg_chxdata (entget (car (entsel)) '("acad")) 1070 40 10.0)
;;;);defun  