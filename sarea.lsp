; Program : SAREA;      Sum of AREA   �������� �������� �̷���� ������ �������ϱ�.
; program : cala;           calulate Area ����ٰ� �����ִ� �������ϱ�


;*************************************            
;     Program : SAREA
;               Sum of AREA
;               By Suk-Jong Yi            
;               03/07/18
;*************************************
; 05/08/13 : ������ �����ϰ� ����� �� �ֵ��� ����
; ���� ��� �������� �������ְ� �� �������� ���� ���� �˷���
;*************************************            
(defun c:sarea(/
	        th unit sumar plines e  ar  narea var scaledarea scaledareastr 
              )
  (princ "\n[�����ٰ�] SAREA : �������ϱ�")
  (setq th (* (getvar "dimtxt") (getvar "dimscale")))  ;textũ��� dimensionũ�� * dimscale

  (if (not unit) (setq unit 1.0))
  (setq unit (getrealold unit "\nǥ���� ���� �������� �Է��ϼ���: "))

  
  (print "Select polyline defining area to be calculated: ")
 
;(setq e (entsel))
  (setq sumar 0)
  (setq plines nil)
  (setq e (bpoly (getpoint "\nPick Point: ")))
  (while e
    (setq plines (append plines (list e)))
    (foreach var plines
      (redraw var 3)
    ); foreach  
    (command ".area" "e" e)
    (setq ar (getvar "area"))
    (setq sumar (+ sumar ar))
    (setq e (bpoly (getpoint "\Pick Point: ")))  
  ); while
;  (foreach var plines
;    (redraw var 4)
;  ); foreach
  (foreach var plines
    (entdel var)
  ); foreach    
  (setq narea (length plines))
  (princ "\n Number of Area: ")
  (princ narea)
  (princ "\n Total Area: " )
  (setq scaledarea (* sumar unit unit))    ;scale�� ����� ����
  (setq scaledareastr (rtos scaledarea 2 3))  ;text�� ��ȯ
  (princ scaledareastr)

  (setq ip (getpoint "\n������ ǥ���� ��ġ�� ��������: "))  
  (push-os)
  (command "text" "j" "m" ip th "0" (strcat "A=" scaledareastr))
  (pop-os)  
);defun


;----------------------------
; program : cala
;           calulate Area
;		Yi Suk Jong
;		06/08/12
;----------------------------
;������ �����ְ� �� ������ ��������ٰŸ� �����ش�.
; ������ �簢���̳� �ﰢ������ �̷���� �־�� �Ѵ�.
;----------------------------
(defun c:cala( / th ds linegapf thds linegapds icount pp e results plst result1 icountxt ip
		 i sum ipnt sum bdeq bdv maxeq maxv eqx vx ipnt1 y lpt1 lpt2 y1 sumpt sumtxt
	      )
  
  (setq th (getvar "dimtxt")
	ds (getvar "dimscale"))
  (setq linegapf 1.5)	;line gap factor
  
  (setq thds (* th ds)			;scale����� 
  	linegapds (* thds linegapf))


  (setq icount 1)
  (setq pp (getpoint "\nPick Point(����=Enter): "))
  (setq e (bpoly pp))        
  (if (= e nil)
    (progn
      (alert "������ ������ �ﰢ���̳� �簢���� �ƴմϴ�.")
	(exit)
    );progn
  );if  
  (setq results nil)
  (while e
    (setq plst (car (mk_vertlist e)))
    (if (= (cdr (assoc 70 (entget e))) 1) (setq plst (append plst (list (nth 0 plst))))) ;���� ���������� ó���� ������������ �߰�
    (setq result (plarea plst 0.001))
    (if (/= result "No rectangle")
      (progn
        (setq result (strcat (itoa icount) " : " result))
        (setq result1 (divide_str result "="))	;����ٰſ� ����� ����
        (setq results (append results (list result1)))
       );progn
      (progn
	(alert "������ ������ �ﰢ���̳� �簢���� �ƴմϴ�.")
	(exit)
      );
    );if  
      
   
    (redraw e 3)
    (setq icountxt (itoa icount))
    (command "text" pp thds "0" icountxt )
    (setq pp (getpoint "\nPick Point(����=Enter): "))
    (entdel e)					;�ٷ��� pline����� ������ ������ ������ ������ node����    
    (if (/= pp nil)
      (progn
        (setq e (bpoly pp))		;�Է¹��� ������ polyline�����
        (if (= e nil)			;poly line�� ��������� ���� ���
          (progn
            (alert "������ ������ �ﰢ���̳� �簢���� �ƴմϴ�.")
	    (exit)
          );progn
        );if  
        (setq icount (1+ icount))
      );progn
      (setq e nil)			;pp�� nil�ΰ�� while��  �����ϱ� ���� e=nil����
    );if	
  ); while
  
  (setq ip (getpoint "\n����� �����ġ�� ��������: "))

  (setq i 0)

;  (foreach plst plines
;    (setq result (plarea plst 0.001))
;    (setq result (strcat (itoa (1+ i)) " : " result))
;    (setq result1 (divide_str result "="))	;����ٰſ� ����� ����
;    (setq results (append results (list result1)))
;    (setq i (1+ i))
;  );foreach
  
  (setq sum 0)
  (setq i 0)
  (foreach result results
    (setq ipnt (list (car ip) (- (cadr ip) (* linegapds i)) 0))
    (push-os)
    (command "text" ipnt thds "0" (car result))
    (pop-os)
    ;�հ� ���
    (setq sum (+ sum (atof (cadr result))))		;�ջ�
      
    (setq bdeq (car (nth 1 (textbox (list (cons 1 (car result))))))		;����ٰ� textũ�� �Ǵ�
          bdv  (car (nth 1 (textbox (list (cons 1 (cadr result)))))))		;������ textũ�� �Ǵ�
      
    (if (= i 0)
      (setq maxeq bdeq maxv bdv)  		;ùtext�� �ִ밪���� ���
      (progn
        (if (> bdeq maxeq) (setq maxeq bdeq)) ;�ִ밪�̸� ���
	(if (> bdv maxv) (setq maxv bdv)) ;�ִ밪�̸� ���
      );progn	
    );if  
    (setq i (1+ i))
  );foreach

  (setq eqx (+ (car ip) maxeq (* thds 2)))			; =ǥ�� x��ǥ
  (setq vx (+  eqx maxv (* thds 3)))
  (setq i 0)
  (foreach result results
    (setq ipnt (list eqx (- (cadr ip) (* linegapds i)) 0))   ; "=" ǥ�� ��ǥ
    (setq ipnt1 (list vx (- (cadr ip) (* linegapds i)) 0))  ; ��� ǥ�� ��ǥ��������
    (push-os)
      (command "text" ipnt thds "0" "=")
      (command "text" "J" "R" ipnt1 thds "0" (cadr result))
    (pop-os)
    (setq i (1+ i))
  );foreach

  ;---�հ� ���
  
  (setq y (- (cadr ip) (* linegapds (- i 0.5)))
        lpt1 (list (car ip) y)
	lpt2 (list (car ipnt1) y)
	y1 (- y (* linegapds 1.0))
	sumpt (list (car lpt2) y1))
  (setq sumtxt (rtos sum 2 3))
  (push-os)
    (command "line" lpt1 lpt2 "")
    (command "text" "J" "R" sumpt thds "0" sumtxt)
  (pop-os)	     
  
);defun

;-----------------------
; function : plarea
;	     poly line�� vertext�� �̷���� ������ �����ش�. �ﰢ��, 4������ ����
;		Yi Suk Jong
;		06/08/12
;-----------------------
;argument
; plist : point list
; lfac : ���� factor
;��� : �簢�� -> "1.000x2.000=2.000"
;	�ﰢ�� -> "1/2x1.000x2.000=1.000"
;-----------------------

(defun plarea(plist lfac
	              / np p0 p1 p2 p3 p4 ang01 ang23 ang12 ang34 da13 da24 return
	      		l1 l2 a l1s l2s as b01 b12 b23 mini angv cp h bs hs as
	      
	      )
  (setq np (length plist))		;point �� ;���ֺ��� ���� ���� ���ϱ�.
  (setq p0 (nth 0 plist)
	p1 (nth 1 plist)
	p2 (nth 2 plist)
	p3 (nth 3 plist))
  
  (if (> np 4) (setq p4 (nth 4 plist))) ;4�����ΰ��
  
  (setq ang01 (angle p0 p1)  
	ang23 (angle p2 p3)
	ang12 (angle p1 p2))
  (if (> np 4)  (setq ang34 (angle p3 p4))) ;4������ ���
  (cond
    ((= np 5)	;�簢���ΰ��
  	(setq da13 (dang ang01 ang23)		;��1,3�� ����
	     da24 (dang ang12 ang34))	;��2,4�� ����
  	(if (or (> (abs (- (abs da13) pi)) 0.001) (> (abs (- (abs da24) pi)) 0.001))
    	  (setq return "No rectangle")
    	  (progn
      	    (setq l1 (distance p0 p1)
	          l2 (distance p1 p2))
      	    (setq a (* l1 l2))
      	    (setq l1s (* l1 lfac)
	    	  l2s (* l2 lfac)
	    	  as (* a lfac lfac))
      	    (setq return (strcat (rtos l1s 2 3) "x" (rtos l2s 2 3) "=" (rtos as 2 3)))
    	  );progn
  	);if
     );subcond
    ((= np 4)	;�ﰢ���ΰ��
      (setq b01 (distance p0 p1)
	    b12 (distance p1 p2)
	    b23 (distance p2 p3))
      (setq mini (car (vl-sort-i (list b01 b12 b23) '<)))  ;���� ������ ã��
      (cond
	((= mini 0) (setq b1 p0 b2 p1 b3 p2 b b01))	;basepoint �� ������
	((= mini 1) (setq b1 p1 b2 p2 b3 p3 b b12))
	((= mini 2) (setq b1 p2 b2 p3 b3 p1 b b23))
      );cond	
      (setq angv (+ (angle b1 b2) (* 0.5 pi)))
      (setq cp (inters b3 (polar b3 angv 100) b1 b2 nil))
      (setq h (distance cp b3))
      (setq bs (* b lfac)
	    hs (* h lfac)
	    as (* 0.5 bs hs))
      (setq return (strcat "1/2x" (rtos bs 2 3) "x" (rtos hs 2 3) "=" (rtos as 2 3)))
    );subcond
  );cond  
  return
)  ;defun