;-------------------------------
; function : djdg_divdim
;            divide dimension
;            Yi Suk Jong
;            04/09/02
;-------------------------------
;> argument :
;    dst   : total distance
;    pitch : pitch
;> return
; case-1 : 2@200
; case-2 : 155+9@200
; case-3 : 9@200+155
; case-4 : 5@200+155+4@200
; case-5 : 4@200+155+5@200
; 2-div case-6 : 77+9@200+78
;       case-7 : 78+9@200+77
; 3-div case-8 : 52+52+53+9@200
; 4-div case-9 : 38+38+38+39+9@200

(defun djdg_divdim( dst pitch div rediv / )
  
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
     

;     (if (= (length rmlst) 2)   ; �ѳ����� �������ϱ�;
;	(setq nrem1 (car (nth 0 rmlst))
;	      nrem2 (car (nth 1 rmlst))
;	      nrem  (+ nrem1 nrem2))
;	(setq nrem1 (car (nth 0 rmlst))
;	      nrem nrem1)
;     );if
      
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
    (setq vlst (append vlst (list (list (- div rm) mk))))
  );if
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


(defun djdg_retxt(rmlst div)
  (setq n1 0)
  (setq n (car (nth 0 rmlst))
        v (cadr (nth 0 rmlst)))
  (if (= (length rmlst) 2)
    (setq n1 (car (nth 1 rmlst))
	  v1 (cadr (nth 1 rmlst)))
  );if
  
  (setq num (+ n n1))

  
  (cond
    ((or (= div 1) (= num 1))
      (if (= (length rmlst) 1)
	(progn  ; ((1 100))  ;�Ѱ��� ���
	    (if (= n 1)      ;n=1�ΰ��
            (setq return (strcat (rtos v 2 0)))
	    (setq return (strcat (rtos n 2 0) "@" (rtos v 2 0)))
	  );if  
	);progn
	(progn  ; ( (1 100) (1 101)) �ΰ��ΰ��
	  (setq return (strcat (if (= n 1) (rtos v 2 0) (strcat (rtos n 2 0) "@" (rtos v 2 0))) "+"
			       (if (= n1 1) (rtos v1 2 0) (strcat (rtos n1 2 0) "@" (rtos v1 2 0)))))
	  
	);progn  
    );if
  );sub cond
  ((and (= div 2) (/= num 1))
    (setq vlst nil)
    (repeat n (setq vlst (append vlst (list (rtos v 2 0)))))
    (repeat n1 (setq vlst (append vlst (list (rtos v1 2 0)))))
    (cond
      ((= (rem num 2) 0)   ;¦���϶�
        (setq div1 (fix (/ num 2)))
        (setq i 0
	      return1 "")
        (repeat div1
	  (setq return1 (strcat return1 (nth i vlst) (if (< i (1- div1)) "+" "")))
	  (setq i (1+ i))
	);repeat
        (setq return return1)
      );subcond	  
    );cond	  
  );sub cond 
 );cond   
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