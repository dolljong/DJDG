;-----------------------
; pldim : Pline dimension
;         polyline ġ���� �����.
;         Yi Suk Jong
;         05/03/10
;----------------------
; function : djdg_dimtext2pvert : �־��� �������κ��� ġ�� text �������� ������ ���ϰ� text�� �������.

(defun c:pldim(
	       / ds donutsz dimexe dimexo djdg_objtodimline djdg_objtodimlineds exlinelen
	         sel ent selp objpl defp defponpline deflen offsetdst dimpl objdimpl sp spdst spdp
	         spang spel1 epel1 ep epdst mp dst epdp mpdp epang spel2 epel2 dtxt
	       )

  ;(setq acobject (vla-get-object )

  (vl-load-com)

  (setq ds (getvar "dimscale")
	donutsz (* ds (getvar "dimasz"))   	;���� ����.
        dimexe (* ds (getvar "dimexe"))		;ġ���������� ġ������ �����Ÿ�.
	dimexo (* ds (getvar "dimexo"))
	)	;ġ���������� ��ü�� ������ �Ÿ�.
  
  (setq djdg_objtodimline 20   ;object ���� ġ�������� �Ÿ� mm����.
        djdg_objtodimlineds (* djdg_objtodimline ds)   ;dimscale����
	exlinelen (- djdg_objtodimlineds dimexo))	;ġ���������� ����.
  
 
  (setq sel (entsel "\nSelect Pline: "))
  
  (setq ent (entget (car sel ))     ;���� pline
	selp (cadr sel))            ;������.
  (setq objpl (vlax-ename->vla-object (car sel)))   ; pline�� activex object
  
  (setq defp (getpoint "\nSpecify first extension line origin or <Select Object>: "))
  (if defp
    (progn						;�������� �Է����� ��
;      (setq sdp (getpoint defp "\nPick Side: "))
      (setq defponpline (vlax-curve-getClosestPointTo objpl defp ))	;def-point�� ���� ����� pline���� ��.
      (setq deflen (distance defponpline defp))                         ;def-point�� pline�� �Ÿ�.
      (setq offsetdst (+ djdg_objtodimlineds deflen))
      (command "offset"   offsetdst sel defp "")
      (setq dimpl (entlast)) 	;ġ���� entity
    );progn
    (progn						;ġ������ �̹� ������� ���� ��
      (setq dimpl (car (entsel "\nSelect Dimension Pline: ")))
    );progn
  );if  
  
  (setq objdimpl (vlax-ename->vla-object dimpl))     ;dimension line�� activex object
  
  (setq sp (getpoint "\nPick Start Point: "))
  (setq spdst (vlax-curve-getDistAtPoint objpl sp)) ;�������� distance from start point
  (setq spdp (vlax-curve-getClosestPointTo objdimpl sp ))	;start point dim point
  (setq spang (angle sp spdp))					;���������� ������ ġ�������� ��	
  (setq spel1 (polar spdp (+ spang pi) exlinelen))		;ùġ���������� ������(dimexo����)
  (setq epel1 (polar spdp spang dimexe))			;ùġ���������� ����(dimexe����).
  (push-os)
  (cecolor (if (numberp (getvar "dimclre")) (itoa (getvar "dimclre")) (getvar "dimclre") ))
  (command "line" spel1 epel1 "")		;ù ġ�������� �׸���.
  (popcolor)
  (command "donut" "0"  donutsz spdp "")	;���ӱ׸���(arrow)
  (push-os)

  ;�ι�°������ �ݺ�����
  (while   (setq ep (getpoint "\nPick End point: "))
    (setq epdst (vlax-curve-getDistAtPoint objpl ep))  			;������ �Ÿ�.
    (if (= epdst nil)					;�������� pline�� ���� ��.
      (progn
	(setq ep (vlax-curve-getClosestPointTo objpl ep ))  ;���������� ���� ����� pline���� ���� ep������..
	(setq epdst (vlax-curve-getDistAtPoint objpl ep))
      );progn	 
    );if  
    (setq mp (vlax-curve-getPointAtDist objpl (/ (+ spdst epdst) 2.0)))  ;pline���� ������ �����.
    (setq dst (abs (- epdst spdst)))   				;�������� �������� ���� ����.

    (setq epdp (vlax-curve-getClosestPointTo objdimpl ep )	;end point dim point
	  mpdp (vlax-curve-getclosestpointTo objdimpl mp))      ;mid point dim point

    (setq epang (angle ep epdp))					;�������� ���� ġ�������� ����.
  
    (setq spel2 (polar epdp (+ epang pi) exlinelen)		;�ι�°ġ���������� ������(dimexo����)
	  epel2 (polar epdp epang dimexe))	;�ι�°ġ���������� ����(dimexo����).
    (setq dtxt (rto_dimtxt dst))                  ;ġ�� string���ϱ�.
  
    (push-os)
    (cecolor (if (numberp (getvar "dimclre")) (itoa (getvar "dimclre")) (getvar "dimclre") ))
    (command "line" spel2 epel2 "")		;�ι�° ġ�������� �׸���.
    (command "donut" "0"  donutsz epdp "")	;���ӱ׸���(arrow)
    (popcolor)	
;  (command "donut" "0"  donutsz mp mpdp "")	;���ӱ׸���(arrow).
    (cecolor (if (numberp (getvar "dimclrt")) (itoa (getvar "dimclrt")) (getvar "dimclrt") ))    
    (djdg_dimtext2pvert mp mpdp dtxt)		;ġ������.
    (popcolor)
    (pop-os)

    ;��� �׸� ���� ������ ù�� ������ ����. 
    ;���峻�� Ŀ����� �Ÿ�.
    (setq spdst epdst)				;��ݱ׸� ���� Ŀ����̸� ù�� Ŀ����̷� ����
  ); while
);defun


;------------------------------------
; function : djdg_dimtext2pvert
;            get dim text point
;            �־��� �������κ��� ġ�� text �������� ������ ���ϰ� text�� �������.
; arguments:
;     sp  > ������.
;     ep  > ����.(text���Ա�����) ġ�� �Ʒ���.
;    txt  > text
; Returns:
;  
;------------------------------------
(defun djdg_dimtext2pvert(sp ep txt
			 / th ang wh4 ang2 tang txtpnt
			 )
  (setq th (* (getvar "dimtxt") (getvar "dimscale")))

  (setq ang (rem (+ (angle sp ep) (/ pi 2)) (* 2 pi))) 	;������ �̷�� ��.
  

  (setq wh4 (which4 ang))                              	;���и鿡 �ִ°�?

  (cond                                                	;1~4��и鿡 ���� ��
     ((= wh4 1)
       (setq ang2 ang)
       (setq tang (+ ang (* pi 0.5)))      
     )
     ((= wh4 2)
       (setq ang2 (- ang pi))
       (setq tang (- ang (* pi 0.5)))            
     )
     ((= wh4 3)
       (setq ang2 (- ang pi))
       (setq tang (- ang (* pi 0.5)))            
     )
     ((= wh4 4)
       (setq ang2 (- ang (* 2 pi)))
       (setq tang (+ ang (* pi 0.5)))            
     )
  );of cond
  
  (setq txtpnt (polar ep tang (* th 0.5)))  		;text point

  (command "TEXT" "J" "C" txtpnt th (rtod ang2) txt "")

);defun


;---------------------------
; program : PLN (Pline Length)
;           Pline ���� ������ �Ÿ��� �����ش�.
;           Yi Suk Jong
;           05/03/013
;---------------------------

(defun c:pln(
	    / plent plobj sp ep sp1 ep1 dstsp dstep dst
	    )
  (vl-load-com)
  (setq plent (car (entsel "\nSelect Pline or Arc: ")))   ;entity �Է¹���
  (setq plobj (vlax-ename->vla-object plent))             ;object name���� ��ȯ
  (setq sp (getpoint "\nPick start point: "))              ;������ �Է�.
  (setq ep (getpoint "\nPick end Point: "))		;�����Է�.
  (setq sp1 (vlax-curve-getclosestpointto plobj sp)	;ù������ ����� pline���� ��
	ep1 (vlax-curve-getclosestpointto plobj ep))	;�������� ����� pline���� ��.
  (setq dstsp (vlax-curve-getdistatpoint plobj sp1)     ;ù���� �Ÿ�
	dstep (vlax-curve-getdistatpoint plobj ep1))    ;������ �Ÿ�
  (setq dst (abs (- dstep dstsp)))			;������ �Ÿ�
  (princ "\nLength : ")(princ dst)(princ)
);defun  