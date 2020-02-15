;-------------------------------------
; function : cctc_dia
;            dialog box�� ���ؼ� ctc�� �����ϵ��� ��.
;--------------------
; function : djdg_divint
;            Divide integer
;            ������ �־��� ������ ����� ��ҵ�� �������
;-------------------------------
; function : djdg_divdim
;            divide dimension
;            �־��� �Ÿ�/����/����/������ ���� �� �̿��Ͽ� �������� ����� ġ���ؽ�Ʈ�� ������.
;            ex) �Ÿ�=1250 ����=125 ����=8 ����������=2
;               -->  2@125+8@125, 8@125+2@125, 4@125+2@125+4@125, 125+8@125+125
;                  (������������) (�������ڷ�) (�����������)   (������ �ѷγ�����)
;-----------------
; function : djdg_retxt1
;            remain text1
;            ������ list�� ������ 1�� �Ǵ� 2���� �����ִ� �Լ�
;            ������ list ((2 250) (2 251))    
;            1���� ����� ��� : ("2@250+2@251")
;            2���� ����� ��� : ("2@250" "2@251")
;               ������ ������ ������ �ѷ� �ɰ���. �ɰ��� ����� ������ list�� �Ϸķ� �����ϰ�
;               �ΰ��Ǵ� �Ѱ��� �����. �̶� ������ ������ ������ @�� �����ش�.
;               1�ķ� ������ �� �߶󳻴� ���� djdg_retxt�� �ٸ� ���̴�.
;----------------------------
; function : djdg_makegoltext
;            ����� text�� ������ش�.
;             ex) '((2 200) (2 500))  --> 2@200+2@500
;----------------------------------------
; function : djdg_checkgol
;            �־��� ���ݵ�� �����(@)�� ������ش�.
;            ex) '(100 100 200 200)  --> ((2 100) (2 200))
;----------------------------
; function : djdg_splitlist
;            list�� �־��� ��ġ���� �߶� �ΰ��� list�� �������.
;            ex) '( 1 2 3 4) 2 -->  (1 2) (3 4)  : �ΰ��� list�� ��������.

;--------------------------------
; Program : CCTC
;           Change CTC
;           Yi Suk Jong
;           05/02/21
; Dim�� ctc�� �ٲ���
; ex) 10@125=1250  --> ctc 150���� ����  --> 8@150+50=1250
; -------------------------------

(defun c:cctc()
  (setq desel (entsel "\nSelect Dimension: "))
  (setq de (entget (car desel)))

  (setq as1 (cdr (assoc 1 de))) ;text
  (setq as10 (cdr (assoc 10 de))) ; dimpoint

  (setq as13 (cdr (assoc 13 de))) ; def point 13
  
  (setq as14 (cdr (assoc 14 de))) ; def point 14
  
  (grdraw as13 (mapcar '+ as13 '(10 10 0)) 1 1)  ;grdraw test
  
  (setq sptxt (djdg_splitdimtxt as1))

  (setq spnum (djdg_splitdimtxtnum as1))  ; dimtxt�� �߶� ���ڷ� ����
  (if (= (nth 0 spnum) nil)		; ù��° ��(mok)�� nil�ΰ�� --> ���̸� ���� (nil nil 1.250)
    (setq totl (nth 2 spnum))           ; ����°���� total length��
    (setq totl (* (nth 0 spnum) (nth 1 spnum)))  ; �׷��� ���� ��� mok@divl���� total length��
  );

  (if (= (nth 1 spnum) nil)  ; �ι��� ���� nil�� ��� �� ����̰� ���� ġ���� �ִ� ���
     (setq divl totl)   ;divl�� ġ���� ����.
     (setq divl (nth 1 spnum)) ;����̰� �ִ� ���� divl�� �ι�° ������ ����.
  );if

  (setq divlist (djdg_divlen totl divl))  ;��� ������.

  (setq mok (nth 0 divlist)
	re (nth 1 divlist))

  ;�ʱ�ġ�� �����ϰ� dialogn�ڽ��� ���.
  (setq #dst totl)    ;�ѱ���.
  (setq #num mok)  ;��.
  (setq #divl divl)    ;���ݰ�.
  (setq #re (rem #dst #divl))  ;������

  (cctc_dia)		;���̾�α׹ڽ��� #dimstr(���� dim����) �ޱ�.
  
  ;----- dialog box�� ������ #dimstr��� ġ���� �׸���.
  (setq spdstr (divide_str #dimstr "+"))	;+�� split�� #dimstr
  (setq idx 0)
  (repeat (length spdstr)
    (setq dstr (nth idx spdstr))	;ó���� dimstring
    (setq spgol (divide_str dstr "@"))
    (if (> (length spgol) 1)     	;@�� �ִ� ���.
      (setq mok (atoi (car spgol))
	    divl (djdg_dimtxtoreal (cadr spgol)))
      (setq mok 1			;@�� ���� ���.
	    divl (djdg_dimtxtoreal (car spgol)))
    );if
    
    (if (= idx 0)
      (setq stpnt as13)
      (setq stpnt ipnt)
    );if
    (setq angdl (djdg_angofdimline (car desel)))  ;angle of dim line
    (cond			;Hor, Ver, Align �� ��� ó�� note:05/12/30 Align�� ��� ó�� ������.
      ((= angdl 0)  		;horizontal�� ���
        (if (minusp (- (car as14) (car as13))) (setq mok (* mok -1)))
        (setq ipnt (f_dh stpnt divl mok as10 nil))	;ù dim�ΰ�� ���� ġ������ ���������� ����
      );sub cond
      ((= angdl (/ pi 2.0))	;vertcial�� ���
        (if (minusp (- (cadr as14) (cadr as13))) (setq mok (* mok -1)))
	(setq ipnt (f_dv stpnt divl mok as10 nil))  
      );sub cond
      (T			;Aligned�� ���
        (setq ipnt (f_da stpnt angdl divl mok as10 nil))
      );sub cond else 
    );cond  
    
    (setq idx (1+ idx))  ;���� ġ�� string����..
  );repeat
  (if (= #delold 1) 
    (command "erase" (car desel) "")		;����ġ���� �����.
    (djdg_cdimdan (car desel) 1)		;����ġ���� 1�� �ø���.
  );if  

);defun


;-------------------------------------
; function : cctc_dia
;            dialog box�� ���ؼ� ctc�� �����ϵ��� ��.
;            Yi Suk Jong
;            05/02/25
;-------------------------------------
(defun cctc_dia()

  (defun done-dialog()
    (if (= (get_tile "delold") "1")
      (setq #delold 1)
      (setq #delold 0)
    );if
    (done_dialog)
  );defun
  
    (setq dcl_id (load_dialog "DJDG.DCL"))                  ;dialogȣ��
    (if (not (new_dialog "CCTC" dcl_id)) (exit))

;-------------------
; �ʱⰪ����
;-------------------
  
  (set_tile "length" (strcat "Distance: " (rtos #dst 2 0)))
  (if (/= #num nil) (set_tile "num" (rtos #num 2 0)))
  (if (/= #divl nil) (set_tile "divl" (rtos #divl 2 0)))
  (if (/= #rnum nil) (set_tile "rnum" (rtos #rnum 2 0)) (set_tile "rnum" "1"))  ;���������� �ʱⰪ=1
  (if (/= #re nil) (set_tile "re" (rtos #re 2 0)) (set_tile "re" "0")) ;������ �ʱⰪ=0
  (if (or (= #delold nil) (= #delold "1")) (set_tile "delold" "1") (set_tile "plus1dan" "1")) ;������ �ʱⰪ=0
  
  (set_tile "ntd" (rtos (* #num #divl) 2 0))

  (checkex)
;  (if (/= #lstiff nil) (set_tile "brib" (rtos #lstiff 2 0)))  
;  (if (/= #tstiff nil) (set_tile "thick" (rtos #tstiff 2 0)))
    
;---------------------------
; dialog box �ʱ�ȭ
;---------------------------
;   (action_tile "num" "(setq at \"num\")(checkex)")
   (action_tile "num" "(checkex)")  
   (action_tile "divl" "(checkex)")
   (action_tile "rnum" "(checkex)")
   (action_tile "lst" "(selectdimcase)")
   (action_tile "final" "(flen)")
  
   (action_tile "brib" "(checkex)")
   (action_tile "thick" "(checkex)")
   (action_tile "check" "(checkex)") 
    
   (action_tile "accept" "(done-dialog)")

   (action_tile "cancel"  "(exit)")

    ;   (mode_tile "fl_weld" 2)

    (start_dialog)

    (unload_dialog dcl_id)
) ;of sub defun

;(defun do-accept();
;
;);defun  

(defun selectdimcase()
  (setq #lstidx (atoi (get_tile "lst")))
  (setq #dimstr (nth #lstidx #caselst))
  (setq #flen (djdg_getdimlen #dimstr))  ;�ѱ��� ���ϱ�.
  (set_tile "final" #dimstr)    ;�������� text�� ���̾�α� �ڽ��� ǥ��
  (set_tile "flen" (strcat "��������: " (rtos #flen 2 0)))  ;���̸� ���̾�α׹ڽ��� ǥ��
  
);defun

(defun flen()
  (setq #dimstr (get_tile "final"))		;�ѱ��� string  -> ���������� �Ѱ���.
  (setq #flen (djdg_getdimlen #dimstr))  ;�ѱ��� ���ϱ�.
  (set_tile "flen" (strcat "��������: " (rtos #flen 2 0)))  ;���̸� ���̾�α׹ڽ��� ǥ��
);defun

(defun checkex()
  (setq #num (atof (get_tile "num")))
  (setq #divl (atof (get_tile "divl")))
  (setq #rnum (atof (get_tile "rnum")))
  
  (setq #ntd (* #num #divl))
  
  (if (> #ntd #dst) (setq #num (fix (/ #dst #divl))))  ;���� �Էµ� ���� ���ġ�� �� ���̺��� ū��� ���� divl�������� �����

  (setq #ntd (* #num #divl))
  (setq #re (- #dst (* #divl #num)))

  (setq ren (djdg_divint #re #rnum))   ;���������� ���.

  (setq #caselst (djdg_divdim #dst #divl #num #rnum)) ;������ ���� ����� ��ġ���� ����� ���ؼ� case����.

  ;; ������� ����...  05/02/26  am 7:38

  ;; list���̾�α׿� case���Ұ�� �ѷ��ֱ�
  (start_list "lst")
  (mapcar 'add_list #caselst)
  (end_list)

  (set_tile "num" (rtos #num 2 0))
  (set_tile "ntd" (rtos #ntd 2 0))
  (set_tile "re" (rtos #re 2 0))
)  ; defun



;--------------------
; function : djdg_divint
;            Divide integer
;            Yi Suk Jong
;            04/10/23
;--------------------------
; v : value��
; div : ������ ��
; >> return
; ex) 100 5 --> ((5 20))
; ex) 86 3  --> ((2 29) (1 28))
(defun djdg_divint(v div / v div mk rm vlst)
  (setq mk (fix (/ v div)))  ;�� 
  (setq rm (fix (rem v div)))      ;������
  (setq vlst nil)
  (if (> rm 0)
    (setq vlst (append vlst (list (list rm (1+ mk)))))
  );if
  (if (> (- div rm) 0)
    (setq vlst (append vlst (list (list (fix (- div rm)) mk))))
  );if
);defun



;-------------------------------
; function : djdg_divdim
;            divide dimension
;            �־��� �Ÿ�/����/����/������ ���� �� �̿��Ͽ� �������� ����� ġ���ؽ�Ʈ�� ������.
;            ex) �Ÿ�=1250 ����=125 ����=8 ����������=2
;               -->  2@125+8@125, 8@125+2@125, 4@125+2@125+4@125, 125+8@125+125
;                  (������������) (�������ڷ�) (�����������)   (������ �ѷγ�����)
;            Yi Suk Jong
;            04/09/02
;-------------------------------
;> argument :
;    dst   : total distance
;    pitch : pitch
;    div : ������ ���� �� ���� ���� �򺸴� ���� ��� �� ������ �۾���.
;    rediv : �������� ������ ���� ��) 1250 = 8@125 + 250 --> �������� 250�� �� �������� ��� �������ΰ�?
;> return (djdg_divdim 1955 200 9 1)
; case-1 : 2@200
; case-1 : 155+9@200  �������� ������
; case-2 : 9@200+155  �������� �ڷ�
; case-3 : 5@200+155+4@200 �������� �����
; case-4 : 77+9@200+78 �������� �ѷ� ������

(defun djdg_divdim( dst pitch div rediv / re mok dimlst rmlst rtxt1 rtxt2 mktxt mokr1 mokr2)
  
  (setq re (rem dst pitch))    ;remain
  (setq mok (fix (/ dst pitch)))  ;mok

  (if (< div mok)  ;�������־��� ������ ������ ���� �򺸴� ���� ��� mok�� div�� ����
    (setq mok div
	  re (- dst (* mok pitch)))
  );if
  
  (setq dimlst nil)   ;initailize dim list
  
  (cond
    ((= re 0)    ;remain = zero  �������� ���� ��� 3@100=3000 
      (setq dimlst (append dimlst (list (strcat (rtos mok 2 0) "@" (rtos pitch 2 0)))))
    ); re = 0
    ((/= re 0)   ; remain /= zero  �������� �ִ� ���
     			; 155+8@200=1955
      (setq rmlst (djdg_divint re rediv))  ;�������� rediv�� ������ --> 155/2 --> ((1 78) (1 77))
      ; ������ �������� 1�� Ȥ�� 2���� �ؽ�Ʈ�� �������.
      ; �Ѱ��� �ؽ�Ʈ�� �����.
      (setq rtxt1 (djdg_retxt1 rmlst 1))  ; �Ѱ��� ����� "78+77"
      (if (>= rediv 2)
        (setq rtxt2 (djdg_retxt1 rmlst 2))  ; �ΰ��� ����� "78" "77"
      );if	
     
      (setq mktxt (strcat (rtos mok 2 0) "@" (rtos pitch 2 0)))  ;��x���� 8@200

     ;      (print mktxt)
     ;      (print rmlst)
     ;������ �ؽ�Ʈ�� �Ѱ��� ó���ϴ� ��� �� �������� �տ� ���ų� �ڿ� ���ų� �߰��� ���� ���
      (setq dimlst (append dimlst (list (strcat  rtxt1 "+" mktxt))))  ; 155+8@200=1955  ;�������� �տ� ���� ���
               		
      (setq dimlst (append dimlst (list (strcat mktxt "+" rtxt1))))   ; 8@200+155=1955  ;�������� �ڿ� ���� ���
     
      (setq mokr1 (fix (/ mok 2)) ; �������� �߰��� ���� ��� --> �� ������ �ѷ� ����   ;���� �� ����
	    mokr2 (- mok mokr1))                                  ;���� �򰹼�.
     								;4@200+155+4@200=1755 or 4@200+155+5@200=1955
      (setq dimlst (append dimlst (list (strcat (rtos mokr1 2 0) "@" (rtos pitch 2 0) "+"      ;�� ��
					        rtxt1 "+"				;������	
						(rtos mokr2 2 0) "@" (rtos pitch 2 0)))))  ;�޸�

      ;������ �ؽ�Ʈ�� �ΰ��� ó���ϴ� ���
      (if (>= rediv 2)  ;�������� 2���̻����� ������ ���� ����
;       (setq re2 (/ re 2))
       (setq dimlst (append dimlst (list (strcat (nth 0 rtxt2) "+"        ;�� ������
						 (rtos mok 2 0) "@" (rtos pitch 2 0) "+"   ;�߰� ��
      						 (nth 1 rtxt2)))))       ;�� ������.
      );if	
    ); re /= 0 
  );cond
  dimlst
);defun



;-----------------
; function : djdg_retxt1
;            remain text1
;            ������ list�� ������ 1�� �Ǵ� 2���� �����ִ� �Լ�
;            ������ list ((2 250) (2 251))    
;            1���� ����� ��� : ("2@250+2@251")
;            2���� ����� ��� : ("2@250" "2@251")
;               ������ ������ ������ �ѷ� �ɰ���. �ɰ��� ����� ������ list�� �Ϸķ� �����ϰ�
;               �ΰ��Ǵ� �Ѱ��� �����. �̶� ������ ������ ������ @�� �����ش�.
;               1�ķ� ������ �� �߶󳻴� ���� djdg_retxt�� �ٸ� ���̴�.
;            Yi Suk Jong
;--------------------------------------
; Argument :
;    rmlst : ((2 300) (2 500))
;    div   : 1 2
; Return
;   1�� ��: ("2@300+2@500")
;   2�� ��: ("2@200" "2@500")
(defun djdg_retxt1(rmlst div / dlist n1 n v n1 v1 num regol return ndlist nn1 nn2 splitlst regol1 regol2 )
  (setq dlist nil)     ; distance list����
  (setq n1 0)
  (setq n (car (nth 0 rmlst))  ;ù���� ������ list
        v (cadr (nth 0 rmlst)))
  (if (= (length rmlst) 2)   ;'((2 200) (2 500))  2���� ���
      (setq n1 (car (nth 1 rmlst))
  	    v1 (cadr (nth 1 rmlst)))
  );if
  
  ; ('(2 200) (2 500))  --> (200 200 500 500) : dlist
  (repeat n  (setq dlist (append dlist (list v))))   ; ù��° ������ list
  (if (= (length rmlst) 2)                           ;�ι�° ������ list�� ������ dlist�� �߰�
    (repeat n1  (setq dlist (append dlist (list v1)))) 
  );if
  
  (setq num (+ n n1))   ;��ü ����

  ;;;; 
  ;;;; �Ѱ� �Ǵ� �ΰ��� �߶��ִ� �κ�.
  ;;;; �Ѱ��� ����� ��� checkgol�� ���� �� return��
  (cond
    ((= div 1)
        (setq regol (djdg_checkgol dlist))   ;checkgol�� ���.
	(setq return (djdg_makegoltext regol))  ;gollist�� �̿��Ͽ� textȭ ��.
    );sub cond

    
    ;; �ΰ��� �����ִ� �κ� 
    ((= div 2)
      (setq ndist (length dlist))     ; dlist�� ���� ������ ���� .
      (setq nn1 (fix (/ ndist 2.0)))  ;ù��° ���� ������ ��ü ������ ������ ���� ������.
      (setq nn2 (- ndist nn1))        ;�ι�° ���� ������ ��ü �������� ù��° ���� ���� ����.
      (setq splitlst (djdg_splitlist dlist nn1))   ;�ΰ��� ����  ������ ��ġ�� ù��° ���� ����.
      (setq regol1 (djdg_checkgol (car splitlst))) ;ù��° ������ ����̷� ����.
      (setq regol2 (djdg_checkgol (cadr splitlst))) ;�ι�° ������ ����̷� ����.
      (setq return (list (djdg_makegoltext regol1)
			 (djdg_makegoltext regol2))) ;ù��°�� �ι�° ������ textȭ ��.
  );sub cond 
 );cond   
  return
);defun


;----------------------------
; function : djdg_makegoltext
;            ����� text�� ������ش�.
;             ex) '((2 200) (2 500))  --> 2@200+2@500
;            Yi Suk Jong
;            05/02/24
;----------------------------
(defun djdg_makegoltext(gol / return n i ni v vtxt )
  (setq return "")
  (setq n (length gol))  ;��ü ����� ����
  (setq i 0)  ;ù ��Һ���
  (repeat n   ;��Ұ�����ŭ �ݺ�.
    (setq ni (car (nth i gol)))  ;�ݺ�����.
    (setq v (cadr (nth i gol)))  ;��.
    (setq vtxt (rto_dimtxt v))   ;���� �ؽ�Ʈ�� �ٲ�. rto_dimtxt�Լ�����(djdgfun.lsp)
    (setq return (strcat return (if (>= i 1) "+" "")  ;2��°��� ���Ŀ� "+"�� �߰�
			 (if (>= ni 2) (strcat (rtos ni 2 0) "@") "")    ;������ �ΰ��̻��̸� 2@  ������� ǥ�� 1���� ���� �ƹ� ǥ�� ����.
			 vtxt))   ;���ؽ�Ʈ  .
    (setq i (1+ i))   ;���� ��ҷ�...
  );repeat
  return
);defun



;----------------------------------------
; function : djdg_checkgol
;            �־��� ���ݵ�� �����(@)�� ������ش�.
;            ex) '(100 100 200 200)  --> ((2 100) (2 200))
;            05/02/24
;            Yi Suk Jong
;----------------------------------------
; argument : dlist = '(100 100 200 200)  ���� list
(defun djdg_checkgol(dlist / dlist golist n i snum cv)
    (setq n (length dlist))   ;���ݰ��� 
    (setq golist nil)   ;����� list --> return value
    (setq i 1       ;�ι�° ������ üũ�ϱ� ������
	  snum 1)   ;���� �� ����  ó���� ù��° ���� �����Ƿ� 1
    (repeat n
      (setq cv (nth i dlist));���� ��. i�� dlist�� ������ �ʰ��� ��쿡 nil�� �����ֹǷ� �Ʒ� if�� ��ȿ
      (if (/= cv  (nth (1- i) dlist)) ;���� ���� ���� ���� �����ʰų� ������ ���̸� golist�� ����.
	(progn
	  (setq golist (append golist (list (list snum (nth (1- i) dlist)))));���ο� gol�� �߰���. ���´� '(snum ������).
	  (setq snum 1)  ;���� �� ������ 1�� reset
	);progn
	(setq snum (1+ snum))  ;���� ���� �հ��� ������� snum�� 1����
      );if	
	
      (setq i (1+ i)) ;���� ������..	    
    );repeat
  golist
);  defun



;----------------------------
; function : djdg_splitlist
;            list�� �־��� ��ġ���� �߶� �ΰ��� list�� �������.
;            ex) '( 1 2 3 4) 2 -->  (1 2) (3 4)  : �ΰ��� list�� ��������.
;            Yi Suk Jong
;            05/02/25
;----------------------------
(defun djdg_splitlist(lst idx / len n1 n2 i lst1 lst2)
 (setq len (length lst))   ;��ü ��� ����.
 (setq n2 (- len idx))     ;�ι�° ���� ����.
 (setq n1 (- len n2))      ;ù���� ���� ����.
 (setq i 0)			;ù��° ��Һ���
 (repeat n1			;ù���� ���� ����
   (setq lst1 (append lst1 (list (nth i lst))))
   (setq i (1+ i))
 );repeat
 (setq i idx)			;idx��°��Һ���.
 (repeat n2			;�ι�°���� ����
   (setq lst2 (append lst2 (list (nth i lst))))
   (setq i (1+ i))
 );repeat
 (list lst1 lst2)
);defun


;-----------------------------
; function : djdg_strdim
;            string dim
;            string�� �̿��Ͽ� dimension�� �������.
;            ex) 500+5@200+250
;            Yi Suk Jong
;            05/02/28
;-----------------------------
;>>> argument
; spnt :������.
; ang : ����.
; ud : up/down
; dstr : sring ; Dimension string
;>>> return
; ������ ��.
(defun djdg_strdim(spnt ang ud dstr / spnt ang ud dstr a i l ipnt)
  (setq dlst (divide_str dstr "+"))
;  (setq n (length str)) ;; ġ������.
  (setq ipnt spnt)
  
  (foreach a dlst
	(setq dlst1 (divide_str a "@"))
    (if (= (length dlst1) 2)
      (setq i (atoi (nth 0  dlst1))
	    l (atof (nth 1 dlst1)))
      (setq i 1
	    l (atof (car dlst1)))
    );if  
    (setq ipnt (f_da ipnt ang l i ud nil))
  );foreach  
  
);