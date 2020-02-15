; function List
; djdg_vtcel;            ���ܰ� ���ϱ�.
; djdg_interpolation : �������� �Ͽ� ���� �����ش�.
; 		usage: (interpolation vals sv)
;  		ex) (interpolation '((0 11.5 1) (4 20.0 1) ...) 3)
; djdg_vtcdeltay : ���ܰ delta-y
; djdg_readvtcdata : ��� data(*.eld)������ �о��.
; djdg_readdatafile : data file�� �о��ش�. �̶� ";"���Ĵ� �����ϰ� �����ش�.
; rtosfw : rtos fixed width
; strcopy : string copy
; djdg_drawsec : draw section, x,y,z������ �̷���� �������� �̿��ؼ� �ܸ��� �׸���,
;		 Ⱦ�ܱ׸� �� ������.
; djdg_defelp : define el pnt block, el pnt block�� �����Ѵ�.
; djdg_insertelp : insert el pnt (����ڿ��� ���� �Է¹޾� elpnt���� �μ�Ʈ�Ѵ�.
;	     ���� elpnt block�� ���� �Ǿ����� �ʴٸ� ������ �� �μ�Ʈ�Ѵ�.
;	     �־��� ������ attribute ���� ����Ѵ�.


;---------------------
; program : gel
;           get elevation
;           � ���� �Է� �޾� Elevation�� �����ش�.
;           Yi Suk Jong
;           05/03/20
; 05/08/08 : attribute�� �̿��Ͽ� ����ڰ� ���ϴ� ���� �������� �����ϵ��� ����
;--------------------
(defun c:gel(  /
 		plent aligninfo basepnt basestation unit unitf splited pref num objpl bpdst vtcinfo leftinfo rightinfo
		ellst outlst thispnt tponpl tpdst tptransdst dx tpstation elonline deltay elalign leftslop rightslop
		ang ang90 pnt90 pnt90onpl dstpnt90onpl isleft slop leveldst tptransdst1 dytrans eltp outstring ellst outlst
                numstr elstr stastr	   )
  (vl-load-com)
  (setq plent (car (entsel "\n���� Polyline�� �����Ͻÿ�: ")))  ;����line����.
;  (setq eldatafn (getfiled  "Select VTC Data" "" "eld" 0) )  ;data file�� �Է¹���.
  (setq aligninfo (djdg_readvtcdata))				;���� data file �о��.
  (setq basepnt (getpoint "\nStation�� �˰� �ִ� ���� �����ÿ�: ")) ;�������Է¹���.
  (setq basestation (getreal "\n�������� Station(����=m): "))		;����station
  
  ;-- �˰��� �ϴ� �� �Ϸ¹ޱ�
;  (setq pntlst nil)
;  (while (setq gpnt (getpoint "\n���������� �˰����ϴ� ���� �����ÿ�: ")) ;�˰����ϴ� ���� �����ؼ� �Է¹���.
;    (setq pntlst (append pntlst (list gpnt)))   ;point list�� �� �߰�..
;  );while
  
;  (setq npnt (length pntlst)) 			;�˰� ���� ���� ����.
  ;-- ������ �Է¹ޱ�
  (setq unit (getstring "\n������ �����踦 �Է��Ͻÿ�<Meter(M),MiliMeter(MM)>: "))  ;��������Է¹���.
  (if (= (strcase unit) "MM")
    (setq unitf 0.001)
    (setq unitf 1.0)                       ;unitfactor
    );if  ;�빮�ڷ� ��ȯ.
    
  ;-- ���۹�ȣ �Է¹ޱ�
  (if (not startstr) (setq startstr "N1")) 
  (setq startstr (getstringold startstr "������ ��ȣ"))
  (setq splited (djdg_splitprenum startstr))  ;������ ��ȣ�� prefix�� ��ȣ�θ� �и�
  (setq pref (car splited)		;prefix
	num (atoi (cadr splited)))		;����.(string)

  ;���� ���� ���
  (setq objpl (vlax-ename->vla-object plent))   ; pline�� activex object
  (setq bpdst (vlax-curve-getDistAtPoint objpl basepnt)) ;�������� distance from start point
  (setq vtcinfo (car aligninfo))  ;���ܰ����.
  (setq leftinfo (nth 1 aligninfo)) ;������������.
  (setq rightinfo (nth 2 aligninfo)) ;������������.

  ;����ڰ� ���ϴ� �� �Է¹ް� �� ���� ���ؼ� �۾��ϱ�
  (setq ellst nil)			;���� El list ����.
  (setq outlst nil)			;��� list ����
;  (setq npnt 0)  			;�˰���� ���� ����
  (while (/= (setq thispnt (getpoint "\n���������� �˰����ϴ� ���� �����ÿ�: ")) nil)
  
    (setq tponpl (vlax-curve-getClosestPointTo objpl thispnt))	;�˰����ϴ� ���� pline���� ��.
    (setq tpdst (vlax-curve-getDistAtPoint objpl tponpl)) ;�˰����ϴ� ���� distance from start point.
    (setq tptransdst (* (distance tponpl thispnt) unitf))		;�˰����ϴ� ���� �������������� Ⱦ���� �Ÿ�.
  								;cad���� �Ÿ��̹Ƿ� unit factor����.

    (setq dx (- tpdst bpdst))    ;�˰����ϴ� ���� station- �������� station
    (if (> dx 0) (setq sgn 1) (setq sgn -1))       ;�˰����ϴ� ���� �Ÿ��� �� �ָ� ��ȣ=+.

    (setq tpstation (+ basestation (* dx unitf)))
  

    (setq elonline (djdg_interpolation vtcinfo tpstation 1)    ;�������� el(slop������ ���� elevation)
	  deltay (djdg_vtcdeltay vtcinfo tpstation)) 	  ;��ȭ� dletay
    (setq elalign (+ elonline deltay))			;�����߽ɿ����� elevation
  
    (setq leftslop (djdg_interpolation leftinfo tpstation 1))     ;�������豸�ϱ�
    (setq rightslop (djdg_interpolation rightinfo tpstation 1))	;�������豸�ϱ�

    ;������� �Ǵ�. �����󿡼� �־��� �������� ������ 90���� ���� ������ ���������� �Ÿ��� �־����� ����
    (setq ang (angle tponpl thispnt)                   ;angle from tponpl to tp
	  ang90 (+ ang (* 0.5 pi))
	  pnt90 (polar thispnt ang90 (* 1000 unitf))
	  pnt90onpl (vlax-curve-getclosestpointto objpl pnt90)
	  dstpnt90onpl (vlax-curve-getdistatpoint objpl pnt90onpl))
    (if (> dstpnt90onpl tpdst) (setq isleft nil) (setq isleft T))

    (if isleft
      (setq slop leftslop
	    leveldst (djdg_interpolation leftinfo tpstation 2)) ;level ���� ���� ���ϱ�.
      (setq slop rightslop
	    leveldst (djdg_interpolation rightinfo tpstation 2))
    );if
    (if (> tptransdst leveldst) 			;Ⱦ���� �Ÿ��� level���� ���̺��� ���
       (setq tptransdst1 (- tptransdst leveldst))	;�����߽�-�˰����ϴ� �� �Ÿ����� level�� ����
       (setq tptransdst1 0)
    );if  
    (setq dytrans (* slop tptransdst1 0.01))		;���迡 ���� dy
    (setq eltp (+ elalign dytrans)) 			;���� elevation
    

    (setq outstring (strcat "\n"(rtosfw tpstation 10 3) " "
     				(rtosfw elonline 7  3)  " "
		      		(rtosfw deltay 6 3) " "
		      		(rtosfw elalign 7 3) " "
    				(if isleft "  L " "  R ") " "
		      		(rtosfw slop 6 3) " "
		      		(rtosfw tptransdst 6 3) " "
		   		(rtosfw leveldst 5 3) " "
		      		(rtosfw tptransdst1 6 3) " "
		      		(rtosfw dytrans 6 3) " "
    				(rtosfw eltp 7 3)))
;    (princ outstring)
    (setq ellst (append ellst (list (list (car thispnt) (cadr thispnt) eltp)))) ;���� EL��Ͽ� �߰�
    (setq outlst (append outlst (list outstring)))   ;��� string�� list�� �߰�.
    
    ;--- ����� att block�� ǥ���ϱ�    
    (setq numstr (strcat pref (itoa num)))   ;point��ȣ string�����
    (setq elstr (strcat "EL=" (rtos eltp 2 3)))
    (setq stastr (strcat "STA." (rtos tpstation 2 3)))
    (djdg_insertelp thispnt (list (list "Pnum" numstr)
				  (list "elv" elstr)
				  (list "sta" stastr)))
;  (setq flaglist '(("Pnum" "Point Number")
;		   ("Elv" "Elevation")
;		   ("Sta" "Station")))

    (setq num (1+ num))  				;���� ����ȣ ����
  );while

  ;el����ٰ� textâ�� ���
  (princ "\n  STATION     VEL     Dy     EL   SIDE T-SLOP  DST   LEVEL  DST1   DY1    ELV") ;output headline���.  
  (foreach outstr outlst
    (princ outstr)
  );foreach
  
;    (setq lenpl (vla-get-length objpl))    	;pline�� �ѱ���.
  
;    (if (> lenpl 100000) (setq unit "MM") (setq unit "M")) 
;    (setq objdimpl (vlax-ename->vla-object dimpl))     ;dimension line�� activex object

;    (setq epdst (vlax-curve-getDistAtPoint objpl ep))
;    (princ  (strcat "\nSTATION: " (rtos tpstation 2 3)))
  
;  (djdg_drawsec ellst)   ;section �׸���.
  
 (princ)
);defun


;------------------
; function : djdg_vtcel
;            ���ܰ� ���ϱ�.
;            Yi Suk Jong
;            05/03/20
;-------------------
(defun djdg_vtcel( vtcinfo station
		  / return )
  (setq return (+ (djdg_interpolation vtcinfo station 1) (djdg_vtcdeltay vtcinfo station )))
);defun  

;------------------
; function : djdg_interpolation
;            �������� �Ͽ� ���� �����ش�.
;            Yi Suk Jong
;            05/03/20
;------------------
; usage: (interpolation vals sv)
;  ex) (interpolation '((0 11.5 1) (4 20.0 1) ...) 3)
; return:  �������� ��.
; arguments:
;    vals : '((0 11.5 1) (4 20 1).....) : ������ �̷���� ������.
;    sv : searching value : ã���� �ϴ� ��.
; index : �����ϰ� ���� ���� list index ex) (0 11.5 1) �� �̷���� list�� ���������Ͽ� ������ ���� ���� ��� index=2
;------------------
(defun djdg_interpolation(vals sv index
			  / n return i lastx lasty thisx thisy 
			  )
  (setq n (length vals))  ;data�� ����.
  (setq return nil)       ;�ʱⰪ�� nil��... ���� ���� ã�� ���ϸ� nil�� return
  (setq i 1)
  (repeat (1- n)     ;�ι�° data(i=1)���� �˻�.
    (setq lastx (car (nth (1- i) vals))		;���� x��.
	  lasty (nth index (nth (1- i) vals))) 	;���� y��  
    (setq thisx (car (nth i vals))		;�̹� x��.
	  thisy (nth index (nth i vals))) 	;�̹� y�� 
    (if (and (>= sv lastx) (<= sv thisx))
      (setq return (+ lasty (* (/ (- thisy lasty) (- thisx lastx)) (- sv lastx)))) 
    );if  
    (setq i (1+ i))  ;����data��
  );repeat  
  return
);defun


;---------------------
; function : djdg_vtcdeltay
;            vertical translation curve delta-y
;            ���ܰ delta-y
;            Yi Suk Jong
;            05/03/20
;---------------------
; usage: (djdg_ctcdeltay vtcinfo xval)
;  ex)  djdg_vtcdeltay '((0  10  0) (100 20 60)...) 50) 
; arguments:
;      vtcinfo : ��� data (station vip_elevation curve_length)
;      xval : deltay�� ���ϰ� ���� station
; return:
;      deltay : delta-y��.
;      nil : error�� ���.
;      0   : ���������ΰ��.
;-----------------------------

(defun djdg_vtcdeltay(vtcinfo xval
       			/ n i thisx thisy thiscl thiscl2 lastx lasty s1 s2 dx sgn return
		      )
  (setq return 0)
  (setq n (length vtcinfo))   ;��� ������ ����.
  (setq i 1)   		;�ι�° ��������.
  (repeat (- n 2)
    (setq thisx (car (nth i vtcinfo))		;�̹� x��.
	  thisy (cadr (nth i vtcinfo))) 	;�̹� y�� .
    (setq thiscl (nth 2 (nth i vtcinfo))   ;�̹����� �����.
          thiscl2 (/ thiscl 2.0))		;�̹�������� 1/2
    (if (and (>= xval (- thisx thiscl2)) (<= xval (+ thisx thiscl2)))  ;xval�� ��������� ������...
      (progn
      	(setq lastx (car (nth (1- i) vtcinfo))		;���� x��.
	      lasty (cadr (nth (1- i) vtcinfo))) 	;���� y��  .
        (setq nextx (car (nth (1+ i) vtcinfo))	;���� x��.
	      nexty (cadr (nth (1+ i) vtcinfo)))  ;���� y��.
        (setq s1 (/ (- thisy lasty) (- thisx lastx))  ;�ձ��� slop.
	      s2 (/ (- thisy nexty) (- thisx nextx))) ;�ޱ��� slop
        (if (< xval thisx)    ;vip�յ� �Ǵ�--> dx����.
	  (setq dx (- xval (- thisx thiscl2)))    ;vip�ձ����ΰ��.
	  (setq dx (- (+ thisx thiscl2) xval))    ;vip�ޱ����ΰ��.
        );if
        (if (< s2 s1)
	  (setq sgn -1)    ;�ޱ��� slop�� �ձ��� slop���� �������
	  (setq sgn 1)
        );if	
        (setq return (* sgn (/ (* (abs (- s1 s2)) (* dx dx)) (* 2 thiscl))))
      );progn 	
    );if  
    (setq i (1+ i))
  );repeat
  return
);defun

;------------------------
; function : djdg_readvtcdata
;            read vtcdata
;            ��� data(*.eld)������ �о��.
;            Yi Suk Jong
;            05/03/20
;------------------------
; usage: (djdg_readvtcdata)
; return: '(vtcinfo leftslop rightslop)
;        > '(�������,��������,��������)
;        vtcinfo: '((0 3.0 0) (10 1.5 5) (20 1.0 0))  '((station vip-elevation curve-length)...)
;        leftslop: '((0 2.0) (15 3.0) (40 -2.0))      '((station slop)...)
;        ritghtslop: '((0 -2.0) (20 -3.0) (50 2.0))   '((station slop)...)
;-------------------------
(defun djdg_readvtcdata(
			/ vtcinfo fn readd n ch citem leftslop rightslop 
			)
  (setq vtcinfo nil leftslop nil rightslop nil)       ;list�ʱ�ȭ
  
  (setq fn (getfiled "Select VTC Data" (getvar "dwgprefix") "eld" 0) )  ;file����.
  (if fn
    (progn
      (setq readd (djdg_readdatafile fn))
      (setq n (length readd))
      (foreach ch readd
        (setq ch (strcase (car (sp-trunc ch)))) 		;�յ� ���� ���ְ� �빮�ڷ�...
        (cond
	  ((= ch "VTC") (setq citem "VTC"))
	  ((= ch "LEFT") (setq citem "LEFT"))
	  ((= ch "RIGHT") (setq citem "RIGHT"))
	  ( T (cond
	       ((= citem "VTC") (setq vtcinfo (append vtcinfo (list (mapcar 'atof (divide_str ch ","))))))  ;���� vtc�� ���
	       ((= citem "LEFT") (setq leftslop (append leftslop (list (mapcar 'atof (divide_str ch ","))))))  ;���� left�� ��
	       ((= citem "RIGHT") (setq rightslop (append rightslop (list (mapcar 'atof (divide_str ch ","))))))
	      );cond
	  );subcond 
        );cond
      );foreach  
      (list vtcinfo leftslop rightslop)
    );progn  
  );if opf  
);defun

;-------------------------
; function : djdg_readdatafile
; 		read data file
;            data file�� �о��ش�. �̶� ";"���Ĵ� �����ϰ� �����ش�.
;            Yi Suk Jong
;            05/03/20
;------------------------
; usage : (djdg_readdatafile fn)
; arguments: fn : file name
; return: data���� ����  (";"���Ĵ� ���õ� ����� list�� ��� �����ش�.
;------------------------
(defun djdg_readdatafile( fn
			 / return opf ch divstr
			 )
  (setq return nil)
  (setq opf (open fn "r"))   ; open file
  (if opf
    (progn
      (while (setq ch (read-line opf))
	(setq divstr (divide_str ch ";"))   ; ";"�� �и�.
	(setq ch (car divstr)) 			;�� string������.
	(if (> (strlen ch) 0)				;string ���̰� 0�� �ƴѰ�츸 ����.
	  (progn
	    (setq return (append return (list ch)))     ;string �߰�
	  );progn  
	);if  
      );while
      (close opf)        ;close file
      return
    );progn
  );if opf  
);defun


;---------------------------------
; function : rtosfw
;            rtos fixed width
;            By Yi Seok Jong (dolljong@dreamwiz.com)
;            2000/7/29
;---------------------------------
; �־��� ���ڸ� �������� ���� string���� ��ȯ�Ͽ� �������ش�.
; fortran���� f10.3�� ���� ������ �Ѵ�.
; ��½� ���� ���� �� �ʿ�
; ex) (rtosfw 3.456 10 3)  --> "     3.456")
;-----------------------------------
(defun rtosfw(num width dec / num width dec str1 lenstr errmsg)
  (setq str1 (rtos num 2 dec))
  (setq lenstr (strlen str1))
  (if (> lenstr width)
    (progn
      (setq errmsg (strcat "String too long (" (rtos lenstr 2 0) ") > " (rtos width 2 0) "(Width)"))
      (alert errmsg)(exit)
    );progn
    (progn
      (strcat (strcopy " " (- width lenstr)) str1)
    );progn
  );if
);defun rtosfw
  
;---------------------------
;function : strcopy
;           string copy
;           By Yi Seok Jong (dolljong@dreamwiz.com)
;---------------------------
; �־��� ���ڿ��� �־��� Ƚ����ŭ ���Ѵ�. �ڸ����� ���� �� ����
; ex) (strcopy " " 5)  --> "     "
;--------------------------------------------
(defun strcopy(str num / str num return)
  (setq return "")
  (if (> num 0)
    (repeat num
      (setq return (strcat return str))
    );repeat
    (setq rerurn "")
  );if  
);defun  


;------------------------
; function : djdg_drawsec
;            draw section, x,y,z������ �̷���� �������� �̿��ؼ� �ܸ��� �׸���
;            Ⱦ�ܱ׸� �� ������.
;            Yi Suk Jong
;            05/03/20
;------------------------
; argument : ((1 2 3) (2 3 4)) : x,y,z�� �̷���� ��������...
;            ((x y z) (x y z))
(defun djdg_drawsec(plst
		    / ipnt i lastpnt pntlst dx dy hisel thispnt pntlst eltxt
		    )
  (setq ipnt (getpoint "\nPick insert point: "))
  (setq i 0)
;  (setq xylst nil)
  (setq lastpnt ipnt)
  (setq pntlst nil)
  (command "pline" )
  (repeat (length plst)
    (if (= i 0)
      (setq dx 0 dy 0)
      (progn
        (setq dx (distance (cdr (reverse (nth (1- i) plst)))
		           (cdr (reverse (nth i      plst)))))
        (setq dy (- (nth 2 (nth i plst)) (nth 2 (nth (1- i) plst))))
      );progn
    );if  
    (setq thisel (nth 2 (nth i plst)))   ;�̹����� elevation
    (setq thispnt (list (+ (car lastpnt) dx) (+ (cadr lastpnt) dy)))
    (setq pntlst (append pntlst (list thispnt)))   ;point list�� �̹��� �߰��ϱ�.
    (setq lastpnt thispnt)
    (command thispnt)
    (setq i (1+ i))   ;���� point��
  );repeat
  (command "")  ;pline��� ��
  (setq i 0)
  (repeat (length pntlst)
    (setq thisel (nth 2 (nth i plst)))
    (setq thispnt (nth i pntlst))
    (setq eltxt (rtos thisel 2 3))
    (command "text" thispnt (* (getvar "dimtxt") (getvar "dimscale")) "0" eltxt "")
    (setq i (1+ i))  ;���� point��
  );repeat  
);defun


;---------------------------------------
; function : djdg_defelp
;            define el pnt block 
;            el pnt block�� �����Ѵ�.
;            Yi Suk Jong
;            05/08/08
;---------------------------------------

(defun	djdg_defelp( /
		     blockn radius gapfromcircle texth flaglist natt i tagstr promptstr y  )
  (setq blockn "djdg_elp")
  (setq radius 1)
  (setq gapfromcircle 2)
  (setq texth 2.0)
  (setq flaglist '(("Pnum" "Point Number")
		   ("Elv" "Elevation")
		   ("Sta" "Station")))
  
  (setq natt (length flaglist))  ;att ����
  
  (if (= nil (tblsearch "block" blockn))
    (progn
  	(entmake (list (cons 0 "BLOCK")(cons 2 blockn)(cons 70 2)
                       (cons 10 (list 0 0 0))))
  	(entmake (list (cons 0 "CIRCLE")(cons 8 "0")(cons 10 (list 0 0 0))
                       (cons 40 radius)
                       (cons 210 (list 0 0 1))(cons 62 256)(cons 39 0)
                       (cons 6 "BYLAYER")))
        (setq i 0)
        (repeat natt
          (setq tagstr (car (nth i flaglist)))
          (setq promptstr (cadr (nth i flaglist)))
          (setq y (- (* i texth -1.2) (* 0.5 texth)))
          (entmake (list (cons 0 "ATTDEF")(cons 8 "0")
  		         (cons 10 (list gapfromcircle y 0))   ; insertion point
                         (cons 40 texth)
                         (cons 1 tagstr)(cons 3 promptstr)
                         (cons 2 Tagstr)
                         (cons 70 0)(cons 73 0)(cons 50 0)(cons 41 1)(cons 51 0)
                         (cons 7 "STANDARD")(cons 71 0)(cons 72 0)
                         (cons 11 (list 0 0 0))
                         ;(cons 210 (list 0 0 1))
		         (cons 74 0)(cons 62 256)
                         (cons 39 0)
                         (cons 6 "BYLAYER")))

          (setq i (1+ i))
        );repeat
      (entmake (list (cons 0 "ENDBLK")))
      (princ (strcat "\nBlock <" blockn "> created."))
    );progn
    (alert (strcat "Block <" blockn "> aleady exist."))
  );if  
);defun



;------------------------------
; function : djdg_insertelp
;            insert el pnt (����ڿ��� ���� �Է¹޾� elpnt���� �μ�Ʈ�Ѵ�.
;	     ���� elpnt block�� ���� �Ǿ����� �ʴٸ� ������ �� �μ�Ʈ�Ѵ�.
;	     �־��� ������ attribute ���� ����Ѵ�.
;            Yi Suk Jong
;	     05/08/08
;------------------------------
(defun djdg_insertelp( ipnt flaglist /
		      	 blockn gapfromcircle texth dimscale i tagstr valuestr x y )
  (setq blockn "djdg_elp")

  (setq gapfromcircle 2)
  (setq texth 2.0)

  ;dimscale����
  (setq dimscale (getvar "dimscale"))
  (setq gapfromcircle (* gapfromcircle dimscale)
	texth (* texth dimscale))
  
  (if (/= nil (tblsearch "block" blockn))
    (progn
      (entmake (list (cons 0 "INSERT")(cons 8 "0")(cons 66 1)(cons 2 "djdg_elp")
                     (cons 10 ipnt)   ;������
       		     (cons 41 dimscale)(cons 42 dimscale)(cons 43 dimscale)
                     (cons 50 0)(cons 70 0)(cons 71 0)(cons 44 0)(cons 45 0)
                     (cons 210 (list 0 0 1))(cons 62 256)(cons 39 0)
                     (cons 6 "BYLAYER")))
      (setq i 0)
      (foreach flag flaglist
        (setq tagstr (car flag)	;flag string
	    valuestr (cadr flag)) ;value string

	(setq x (+ (car ipnt) gapfromcircle)) 			;attribute ������ x
	(setq y (- (cadr ipnt) (* i texth 1.2) (* 0.5 texth)))   ;attribute ������ y
         
	(entmake (list (cons 0 "ATTRIB")(cons 8 "0")
		       (cons 10 (list x y 0))   ; insertion point
                       (cons 40 texth)				;text height
                       ;(cons 1 "")(cons 2 "TagName")(cons 70 0)(cons 73 0)
		       (cons 1 valuestr)(cons 2 tagstr)(cons 70 0)(cons 73 0)
                       (cons 50 0)
                       (cons 41 1)(cons 51 0)(cons 7 "STANDARD")(cons 71 0)
                       (cons 72 0)
                       (cons 11 (list 0 0 0))(cons 210 (list 0 0 1))(cons 74 0)
                       (cons 62 256)(cons 39 0)(cons 6 "BYLAYER")))
	(setq i (1+ i))   		 
      );foreach
      (entmake (list (cons 0 "SEQEND")(cons 8 "0")))
    );progn
    (progn
;      (alert (strcat "Block<" blockn "> Not exist."))
      (djdg_defelp)
    );progn  
  );if  
);defun

