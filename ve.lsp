;---------------------
; program : VE graph
; 	07/06/21
;---------------------
(defun c:ve( /
	    alts fn dlist nline dllist i dllist il head a temp items wf alt valt
	    alts j jl nalt ipnt vegap cpnt ialt altname lcc vs tpnt )

  (setq alts nil) 	;alts list����

  (setq fn (getfiled "Open VE data file" "" "ve" 1))      ;file�̸� �Է¹���
  (setq dlist (djdg_readdatafile fn))  ;data file�о ����
  (setq nline (length dlist))  ;data ���� ";" �� ����..

  (setq dllist nil)		; �������� �и��� list (("A=1.200" "B=4.500") ("C=3.4" "D=4.5"))
  
  (setq i 0)
  (repeat nline			; ��� list�� ����
    (setq dllist (append dllist (list (djdg_splitstr (nth i dlist) " "))))  ;�������� �и� -> ("A=1.200" "B=4.500")
    (setq i (1+ i))	;���� line
  );repeat

  (setq i 0)
  (repeat nline
    (setq il (nth i dllist))  ;i��° ���� list
    (setq head (car (djdg_splitstr (car il) "=")))      ;ù��° atom
    (cond
      ((= (strcase head) "ITEMS")
        (setq a (apply 'strcat il))
        (setq temp (cadr (djdg_splitstr a "=")))
        (setq items (djdg_splitstr temp ","))
      );sub cond
      ((= (strcase head) "WF")
        (setq a (apply 'strcat il))
        (setq temp (cadr (djdg_splitstr a "=")))
        (setq wf (djdg_splitstr temp ","))
        (setq wf (mapcar 'atof wf))
      );sub cond
      ((= (strcase head) "ALT")
        (setq a (apply 'strcat il))
        (setq temp (cadr (djdg_splitstr a "=")))
        (setq alt (djdg_splitstr temp ","))
        (setq valt (mapcar '(lambda (a) (if (is-num a) (atof a) a)) alt))
        (setq alts (append alts (list valt)))  ;alts list�� alt�߰�
       
      );sub cond
      (T
        (setq j 0)
        (repeat (length il)
	  (setq jl (djdg_splitstr (nth j il) "="))
	  (set (read (car jl)) (if (is-num (cadr jl)) (atof (cadr jl)) (cadr jl)))
	  (setq j (1+ j))
	);repeat
      );subcond 
    );cond
    
    (setq i (1+ i))		;���� i��
  );repeat


  (setq nalt (length alts))   ;alt ����
  
  (setq ipnt (getpoint "\nPick insert point: "))
  (command "text" "j" "c" ipnt vth 0 title)  ;title����
  (setq vegap (+ (* cr 2) (* 6 th)))		;�� �׷��� ���� ����
  (setq cpnt (polar ipnt (* -0.5 pi) vegap))
  (setq i 0)
  (repeat nalt
    (setq ialt (nth i alts))			;i���� alt����
    (setq altname (car ialt))			;alt name
    (setq lcc (car (reverse ialt)))		;������ ������
    (setq vs (reverse (cdr (reverse (cdr ialt)))))	;�յ� ���� �������� ����
    ; alt name����
    (setq tpnt (polar cpnt (* 0.5 pi) (+ cr (* 4 th)))) ;alt name text ��ǥ
    (command "text" "j" "c" tpnt vth 0 altname)          ;alt name ����
    
    ;�׷��� �׸���	  
    (draw_ve_graph cpnt cr irratio th vth lth items vs wf lcc)
    
    (setq cpnt (polar cpnt 0 vegap))	        ;�����׷��� �߽� ��ǥ
    (setq i (1+ i))	     
  );repeat   
);defun


;------------------------
; function : draw_ve_graph
;        07/06/22
;------------------------
; arguments
;  ip : ������(�߽���)
;  cr : ���� ������
;  irratio: ���� �������� ����
;  th ; item text����
;  vth : ��ġ���� text����
;  lth : level text����
; vitem : item������ '(1 2 3 4 5)
; wfitem: item�� weight factor
; lcc : ��� lcc����
;-------------------------
(defun draw_ve_graph(ip cr irratio th vth lth titem vitem wfitem lcc /
	       gap tgap ir dr crr vv sf icent crpnt i sumang anglist nitem hang
	       tang tpnt sp v ang1 ang2 vr pnt1 pnt2 iang vpent shent)

;  (setq ip '(0 0))  
;  (setq cr 100 ;circle radius
;	irratio 0.35  ;inner radius ratio
;	th 15   ;text height
;	vth 20  ;text height of ����
;	nitem 6
;	titem '("��ȹ��" "ȯ�漺" "�ð���" "������" "��������" "������")
;	vitem '(10 10 8 6 8 6)
;	wfitem '(0.4 0.2 0.1 0.1 0.1 0.1)
;	LCC 1.0
;  );setq

  

  (setq gap 5)   ;line ��
  (setq tgap 5) ;text��
  (setq ir (* irratio cr)) ;inner radius
  
  (setq dr (/ (- cr ir) 5))  	;delta r
  (setq crr cr)

  ;------ ���� ��� �� ����
  (setq vv (/ (* 10 (apply '+ (mapcar '* vitem wfitem))) LCC))
  (setvar "dimzin" 0)
  (command "text" "j" "m" ip vth "0" (rtos vv 2 1) "")

  ;------ ���׸���
  (setvar "cecolor" "1")
  
  (setq sf 1)  		;scale factor
  (command "circle" ip cr)
  
  
  (repeat 5
    (setq crr (- crr dr)) ;current radius)    
    (command "circle" ip crr)
  );repeat
  (setq icent (entlast))  ;���� ��(hatch�� ���)

  ;------ level ����
  (setq crpnt (polar ip (* 0.5 pi) (- ir (* 0.5 dr))))
  (setq i 0)
  (repeat 5
    (setq crpnt (polar  crpnt (* 0.5 pi) dr)) ;current radius
    (command "text" "j" "ml" crpnt lth 0 (itoa (+ i 6)))
    (setq i (1+ i))
  );repeat


  ;------ ���׸���
  (setq i 0)
  (setq sumang (* 0.5 pi))
  (setq anglist (list sumang))
  (repeat nitem
    (setq sumang (- sumang (* 2 pi (nth i wfitem))))
    (setq anglist (append anglist (list sumang)))
    (command "line" (polar ip sumang (- ir gap))
	            (polar ip sumang (+ cr gap)) "")
    (setq i (1+ i))
  );repeat

  (setvar "cecolor" "7")
  
  ;------- item title ����
  (setq i 1)
  (repeat nitem
    (setq hang (* 0.5 (+ (nth (1- i) anglist) (nth i anglist))))  ;�������� �߰���
    ;text����
    (setq tang (- hang (* 0.5 pi)))
    (setq tpnt (polar ip hang (+ cr tgap)))
    (command "text" "j" "c" tpnt  th (rtod tang)  (nth (1- i) titem))
    (setq i (1+ i))
  );repeat
  

  ;--------- ������ �׸���
  (setq i 0)
  (setq sp (polar ip (* pi 0.5) (- cr (* (- 10 (nth 0 vitem)) dr))))
  (command "Pline" sp "a")
  (repeat nitem
    (setq v (nth i vitem)) ;item�� ��
    (setq ang1 (nth i  anglist)		;���� ��
	  ang2 (nth (1+ i) anglist))  ;�̹� ��
    (setq vr (- cr (* dr (- 10 v))))
    (setq pnt1 (polar ip ang1 vr)
	  pnt2 (polar ip ang2 vr))
    
    (setq iang (* (nth i wfitem) 2 pi -1)) ;item angle
    (command "CE" ip "a" (rtod iang))
    (if (= i (1- nitem))
      (command "L" "cl")		;close
      (command "L" (polar ip ang2 (- cr (* (- 10 (nth i vitem)) dr))) "a")
    );  
    (setq i (1+ i))	
  );repeat  
    (command "")
  (setq vpent (entlast)) 	;�� �ܰ��� entity
  
  ;----- hatch�ϱ�
  (command "hatch" "p" "s" icent vpent "" "")
  (setq shent (entlast))	;hatch entity

  ;----- hatch�� �ٲٱ�
  (command "change" shent "" "P" "C" "254" "")

  ;----- hatch �ڷ� ������
  (command "draworder" shent "" "B")

  
  
);defun  