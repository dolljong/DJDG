;--------------------
;  program : ldd
;            load distributed
;            �������� �׸��� �׷���.
;            Yi Suk Jong
;            5/04/16
;--------------------
(defun c:ldd()

  (setq arwlf 0.25   ;ȭ��ǥ�� ����/������ ũ��(����)
	arwwf 0.25   ; ȭ��ǥ�� ��/������ ũ��(����)
	arwpf 1.0)   ;ȭ��ǥ����/������ ũ��(����)

  
  (setq pnt1 (getpoint "\nù��: "))		; ó�� �Է¹ޱ�.
  (setq pnt2 (getpoint pnt1 "\n�ι�°��: "))	; ��°�� �Է¹ޱ�.
  (setq pnt3 (getpoint pnt2 "\n���� ���� �� ũ�� ����: "))  ;����ũ�� �� ���� ����.

  (initget "X Y V")                ;x,y,vertical
  (setq kword (getkword "\nX-direction/Y-direction/Vertical: "))
  (setq text (getstring "\nText: "))

  (setq ang0 (angle pnt1 pnt2))			;ù�������� ���������� ��.
  
  (cond
    ((= kword "X")    ;X������ ��
     (setq crsp (inters pnt3 (mapcar '+ pnt3 '(100 0 0)) pnt1 pnt2 nil))
     (setq ang (angle crsp pnt3))
    )
    ((= kword "Y")	;Y������ ���.
     (setq crsp (inters pnt3 (mapcar '+ pnt3 '(0 100 0)) pnt1 pnt2 nil))
     (setq ang (angle crsp pnt3))
    )
    ((= kword "V")	;Z������ ���.
     (setq ang (v_angle pnt1 pnt2 pnt3))
     (setq crsp (inters pnt1 pnt2 pnt3 (polar pnt3 ang 100) nil))
    )         
  );cond
  
  (setq ldlen (distance pnt3 crsp)) ;������ ũ��(����).
  
  (setq arwl (* ldlen arwlf)		;ȭ��ǥ����.
	arww (* ldlen arwwf)		;ȭ��ǥ��.
	arwp (* ldlen arwpf))		;ȭ��ǥ����

  ; �ܰ��� �׸���(pline)
  (setq opnt1 (polar pnt1 ang ldlen)   ;���� ù��
	opnt2 (polar pnt2 ang ldlen))  ;���� ����.
  (command "pline" pnt1 opnt1 opnt2 pnt2 "")  ;�ܰ��� �׸���.


  ;���ʼ� ���� ���ϱ�.
  (cond
    ((= kword "X") (setq totl (abs (- (cadr pnt2) (cadr pnt1))))) ;X�� ���y���� total ����.
    ((= kword "Y") (setq totl (abs (- (car pnt2) (car pnt1))))) ;y�� ���x���� total ����.
    ((= kword "V") (setq totl (distance pnt1 pnt2)))            ;v�ΰ��  ������ �Ÿ�.
  );cond  
  (setq n (atoi (rtos (/ totl arwp) 3 0)))[		;���ݰ���(�ݿø�)
  (setq narwp  (/ totl n))		;���ο� arwp

  ;line���� �Ÿ� ���ϱ�
  (cond
    ((= kword "X") (setq diap (abs (/ narwp (sin ang0))))) 	;
    ((= kword "Y") (setq diap (abs (/ narwp (cos ang0))))) 	;
    ((= kword "V") (setq diap narwp))       			;
  );cond  
  

  ;ùȭ��ǥ ������ ȭ��ǥ �׸���.
  (setq scl (/ arwl 2.5))
  (setq angd (rtod ang))  ;degree
  (push-os)
  (command "INSERT" (strcat (prefix) "blocks/" "ARW3") pnt1 scl scl angd)  ;ù��° ȭ��ǥ.
  (command "INSERT" "ARW3" pnt2 scl scl angd)				;������ ȭ��ǥ.
  (pop-os)

  ;�߰�ȭ��ǥ/line �׸���.
  (setq idx 1)
  (repeat (1- n)
    (setq pnti (polar pnt1 ang0 (* idx diap))	;insert point
	  pntl (polar pnti ang ldlen))		;load line����.
    (push-os)
    (command "INSERT" "ARW3" pnti scl scl angd) ;�߰�ȭ��ǥ ����.
    (command "LINE" pnti pntl "")		;draw load line.
    (pop-os)
    (setq idx (1+ idx))				;���� ȭ��ǥ��...
  );repeat
  
  ; text����.
  (setq th (getvar "textsize")
	gap (* th 1.25))
  (setq angv (v_angle pnt1 pnt2 pnt3))  ;���� Angle
;  (setq lenv (length pnt3 (inters pnt1 pnt2 pnt3 (polar pnt3 angv 100) nil)))   ;���� ����.
  (setq txtp (polar (mid-point opnt1 opnt2) angv gap))
  (setq ta (ang4text opnt1 opnt2))    ;text angle

  (push-os)
  (command "TEXT" "J" "M" txtp th (rtod ta) text)
  (pop-os)
  
  ;(djdg_wtxtonline opnt1 opnt2 text th (* th 0.5))
  ;(setq pnt (polar crsp ang ldlen))
  ;(command "line" crsp pnt "")
);defun  
