;;;' function :devlen1 (net ��������) ����: mm2
;;;' function :devlen (��������,10mm������ �ø�) ����: mm2
;;;' function :laplen (����������,10mm������ �ø�) ����: mm2
;;;' function :rebarndia (ö�ٰ�Ī����) ����: mm
;;;' function : djdg_lapnum (���������Ҽ�)
;;;' function : calcrlen (ö�ٱ��� ���ϱ�)
; function : djdg_defratt : ö�� attribute ����
; function : djdg_calctrlen:ö���� �� ���̸� ����(joint���)
; function : djdg_rattmod;            �־��� attƯ������ att block�� �����Ѵ�(entmod�� ���)
; program : ER (Edit Rebar);	Rebar Detail Attribute Block�� ����.
; function : NV_dialog (New Value);		= ���·� �̷���� ���� ������ �� �ִ� ���̾�� �ڽ�����.
; function : ERA_dialog (Edit Rebar All);		��� rebar attribute�� ��ĥ �� �ִ� ���̾�α� �ڽ� ����
; program : ERA (Edit Rebar attribute (All);	���̾�α� �ڽ��� ���� ��� Attribute�� �Ѳ����� ����
; function : djdg_arrangemark;	arrange marking, djdg_ratt�� marking�� ���ȿ�;	�������� ����(�����ϰ��� ������ ��� ����)
; function : djdg_getattenti ;	attibute entity������ code �� value�� �ش��ϴ� entity ������ ����
; function : djdg_entinblk(block table�� entity ������ return)

;---------------------------------------------------------------
; program : erebar
;           Enhenced Rebar
;           attribute�� �̿��Ͽ� �ڵ����� ö�ٱ���/������ ���
;           Yi Suk Jong
;           05/12/20
;--------------------------------------------------------------



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


;;;'------------------------
;;;' function :devlen1 (net ��������) ����: mm2
;;;'           Yi Suk Jong
;;;'           05/12/20
;;;' ����: fck : ��ũ��Ʈ���谭�� (MPa)
;;;'       rebarn : ö�ٸ�Ī("D25", "H25")
;;;'       gap : ö�ٰ���(mm)
;;;'       cover : ö���Ǻ�(mm)
;;;        sang : ���ö�� ����(T/nil)
;;;'------------------------
(defun djdg_devlen1( fck rebarn gap cover sang
		    / c rdia fy gamma rdb cdb return)
  (if (< cover (/ gap 2.0))
    (setq c cover)
    (setq c (/ gap 2.0))
  );if

  (setq rdia (atof (substr rebarn 2 2)))     ;ö������.

  (if (= (strcase (substr rebarn 1 1)) "H")
    (setq fy 400)
    (setq fy 300)
  );if  
    
  (if (<= rdia 19)
    (setq gamma 0.8)
    (setq gamma 1.0)
  );if

  (setq rdb (djdg_rebarndia rebarn))   ;ö���� ��Ī ����.

  (setq cdb (/ c  rdb))  ;(c/db)

  (If (> cdb  2.5) (setq  cdb  2.5))

  (if (= sang "UP") (setq up 1.3) (setq up 1.0))   ;���ö���� ��� x1.3	 
	 
  (setq return (/ (* 0.9  rdb  fy gamma up)  (expt fck  0.5)  cdb))

) ;defun  


;;;'------------------------
;;;' function :devlen (��������,10mm������ �ø�) ����: mm2
;;;'           Yi Suk Jong
;;;'           05/12/15
;;;' ����: fck : ��ũ��Ʈ���谭�� (MPa)
;;;'       rebarn : ö�ٸ�Ī("D25", "H25")
;;;'       gap : ö�ٰ���(mm)
;;;'       cover : ö���Ǻ�(mm)
;;;'------------------------
(defun djdg_devlen(fck rebarn gap cover sang
		   / devlen2 mok rem10 return)

  (setq devlen2 (djdg_devlen1 fck rebarn gap cover sang))

  (setq mok (fix (/ devlen2 10.0)))

  (setq rem10 (- devlen2 (* mok 10)))   ;10���� ���� ������

  (if (> rem10  0)
    (setq return (* (+ mok 1) 10))
    (setq return devlen2)
  );if

  (if (< return 300) (setq return 300))

  return
);defun  


;;;'------------------------
;;;' function :laplen (����������,10mm������ �ø�) ����: mm2
;;;'           Yi Suk Jong
;;;'           05/12/15
;;;' ����: fck : ��ũ��Ʈ���谭�� (MPa)
;;;'       rebarn : ö�ٸ�Ī("D25", "H25")
;;;'       gap : ö�ٰ���(mm)
;;;'       cover : ö���Ǻ�(mm)
;;;'       snag : ���ö�� ���� (���ö��: "��", �ƴѰ��: "")
;;;'---------------------------------
(defun djdg_laplen(fck rebarn gap cover sang
		   /  devlen2 mok rem10 return)
  
;;;Function laplen(fck As Double, rebarn As String, gap As Double, cover As Double, sang As String)
  (setq devlen2 (* (djdg_devlen1 fck rebarn gap cover sang) 1.3))
  
;;;devlen2 = devlen1(fck, rebarn, gap, cover) * 1.3
;;;If sang = "��" Then devlen2 = devlen2 * 1.3
  (setq mok (fix (/ devlen2 10.0)))
  (setq rem10 (- devlen2 (* mok 10)))
  (if(>  rem10 0)
    (setq return (* (+ mok 1) 10.0))
    (setq return devlen2)
  );if
  (if (< return 300) (setq return 300))
  return
);defun


;;;'------------------------
;;;' function :rebarndia (ö�ٰ�Ī����) ����: mm
;;;'           Yi Suk Jong
;;;'           05/12/20
;;;' ����: dia : ö�ٸ�Ī(string) �Ǵ� dia(double) (Variant)
;;;'------------------------
;;;Function rebarndia(dia As Variant)
(defun djdg_rebarndia( dia / ddia return)
  (setq ddia (atof (substr dia 2 2)))   ;ö������.
  (cond
    ((= ddia 10) (setq return 9.53))
    ((= ddia 13) (setq return 12.7))   
    ((= ddia 16) (setq return 15.9))
    ((= ddia 19) (setq return 19.1))
    ((= ddia 22) (setq return 22.2))
    ((= ddia 25) (setq return 25.4))
    ((= ddia 29) (setq return 28.6))
    ((= ddia 32) (setq return 31.8))
  );cond
  return 
);defun


;;;'---------------------------
;;;' function : djdg_lapnum (���������Ҽ�)
;;;'            Yi Suk Jong
;;;'            2005.12.15
;;;'  ����: tlen : �ѱ��� m
;;;'        rlen : ö�ٱ���(ex:8���� ö���� ��� 8) m
;;;'        llen : ���������� m
;;;'---------------------------
(defun djdg_lapnum(tlen rlen llen / return)
  (if (<= tlen rlen)
    (setq return 0)
    (setq return (fix (/ (- tlen llen) (- rlen llen))))
  );if  
  return
);defun  
;;;Function lapnum(tlen As Double, rlen As Double, llen As Double)
;;; lapnum = Fix((tlen - llen) / (rlen - llen))
;;;End Function




;;;'---------------------------
;;;' function : calcrlen (ö�ٱ��� ���ϱ�)
;;;'            Yi Suk Jong
;;;'            2005.12.16
;;;'  ����: data : �������� ����ִ� list ("A=234" "B=234~345")
;;;'        func : ������ (ex:"A+B*2") string
;;;'---------------------------
(defun djdg_calcrlen(data func
		     / id datalist spdata tlen arp narp i sp arxx narx tx j sx sxvstr arm sxv)

  ;--------- data�� �޾Ƽ� ("A" . "234")�������� �������.
  (setq id 0
	datalist nil)
  (repeat (length data)
    (setq spdata (djdg_splitstr (nth id data) "="));split�� data
    (setq datalist (append datalist (list (cons (strcase (nth 0 spdata)) (nth 1 spdata)))))
    (setq id (1+ id)) ;���� id��
  );repeat

  
  (setq tlen 0)
  (setq arp (djdg_splitstr func "+")) ;func�� +�� �������� ����
  (setq narp (length arp))		;+�� ������ ����.
  (setq i 0)

  (repeat narp
    (setq sp (strcase (nth i arp)))  ;+�� �������� text�� �빮�ڷ�
    (setq arxx (djdg_splitstr sp "*")) ;*�� �������� ����
    (setq narx (length arxx))
    (setq tx 1)
    (setq j 0)
    (repeat narx
	  
      (setq sx (nth j arxx))
      (if (is-num sx)
	(setq sxvstr sx)			   ;�����ΰ��
        (setq sxvstr (cdr (assoc sx datalist)))    ;������ ��� ������ �����ϴ� ��string
      );if	
      ;---- ����� �������.
      (if (/= sxvstr nil)    ;sxvstr�� nil�� �ƴѰ�츸 ����ȭ. nil�� ���� ������ ���� ���� �����.
	(progn
      	  (setq arm (djdg_splitstr sxvstr "~"))
          (if (> (length arm) 1)
	    (setq sxv (/ (+ (djdg_dimtxtoreal (nth 0 arm)) (djdg_dimtxtoreal (nth 1 arm))) 2.0))  ;�����ִ� ��� ��հ�����.
	    (setq sxv (djdg_dimtxtoreal (nth 0 arm)))  ;������� ��� string�� �Ǽ���..
          );if	
          (setq tx (* tx sxv))
	);progn
	(progn		;������ �����ϴ� ���� nil�϶�
	  (setq tx 0)	;������ ����� 0����..
	  (alert (strcat "���� " sx " �� ���� �������� �ʽ��ϴ�."))
	);progn  
      );if	
      (setq j (1+ j))
    );repeat 
    (setq tlen (+ tlen tx))
    (setq i (1+ i))
  );repeat
  tlen
);defun


;---------------------------------------
; function : djdg_defratt
;            define rebar attribute
;            rebar attribute block�� �����Ѵ�.
;            Yi Suk Jong
;            05/12/20
;---------------------------------------

(defun	djdg_defratt( /
		      blockn ullen cirdia texth gapfromline gapfromcir gapmark
		     flaglist cirx mark1x mark1y mark2y diax diay lenx lentx numx
		     ullen5 var1x var1y var2x var3x var4x var5x jointx fckx fcky
		     ullen6 coverx ctcx unitlx upx eqx poslist natt i tagstr promptstr posi y)
  (setq blockn "djdg_ratt")
  ;------ �������� ����
  (setq ullen 100   	;���� ����(mm)
	cirdia 10
	texth 2.5)
;  (setq radius 1)
  (setq gapfromline 1)    ;���ٺ��� text���� �Ÿ�(mm)  y
  (setq gapfromcir 1)  ;�� ������ ���������� ùtext�������� �Ÿ�(mm) x
  (setq gapmark 1)

  ;  (setq texth 2.0)
  (setq flaglist '(("Mark1" "ö���̸�1")
		   ("Mark2" "ö���̸�2")
		   ("Dia" "ö������(ex:H29)")
		   ("Len" "ö�ٱ���")
		   ("Lent" "�������� ��� ö�ٱ���")
		   ("Num" "ö�ٰ���")
		   ("Var1" "����1")
		   ("Var2" "����2")
		   ("Var3" "����3")
		   ("Var4" "����4")
		   ("Var5" "����5")
		   ("Joint" "����������x����")
  		   ("fck" "fck")
		   ("Cover" "�Ǻ�(mm)")
		   ("ctc" "C.T.C(mm)")
		   ("UnitL" "ö�ٴ�������(mm)")
		   ("Up" "���ö�ٿ���(ex:UP)")
		   ("Eq" "������")
	 ))
  (setq cirx (/ cirdia 2.0))
  (setq mark1x cirx
	mark1y (+ (/ cirdia 2.0) (/ texth 2.0) (/ gapmark 2.0))
	mark2y (- (/ cirdia 2.0) (/ texth 2.0) (/ gapmark 2.0)))
  (setq diax (+ cirdia gapfromcir)
	diay (+ gapfromline))
  (setq lenx  (+ cirdia (/ (- ullen diax) 4)))
  (setq lentx (+ cirdia (* 2 (/ (- ullen diax) 4))))
  (setq numx ullen)
  (setq ullen5 (/ (- ullen diax) 5.0))
  (setq var1x  diax
	var1y (- 0 texth gapfromline)
	var2x (+ diax (* ullen5 1))
	var3x (+ diax (* ullen5 2))
	var4x (+ diax (* ullen5 3))
	var5x (+ diax (* ullen5 4))
	)
  (setq jointx ullen)
  (setq fckx diax
	fcky (+ diay texth gapfromline))
  (setq ullen6 (/ (- ullen diax) 6.0))
  (setq coverx (+ diax (* ullen6 1))
	ctcx   (+ diax (* ullen6 2))  ;ctc(mm)
	unitlx   (+ diax (* ullen6 3))  ;ö�ٴ�������(m)
	upx    (+ diax (* ullen6 4))
	eqx    (+ diax (* ullen6 5)))
  
  (setq poslist (list   (list mark1x mark1y 4)	;text������ list(flaglist�� ¦�� �̷��. ��ǥ�� �������� ������ ���?������..
	  	  	(list mark1x mark2y 4)	;���Ĺ�� 0: left, 1: center 2: right 4: middle
		  	(list diax diay 0)
		  	(list lenx diay 0)
			(list lentx diay 0)
		  	(list numx diay 2)
		  	(list var1x var1y 0)
		  	(list var2x var1y 0)
		  	(list var3x var1y 0)
		  	(list var4x var1y 0)
		  	(list var5x var1y 0)
		  	(list jointx var1y 2)
			(list fckx fcky 0)
			(list coverx fcky 0)
			(list ctcx fcky 0)
			(list unitlx fcky 0)
			(list upx fcky 0)
			(list eqx fcky 0)
		  ))
	 
  (setq natt (length flaglist))  ;att ����
  
  (if (= nil (tblsearch "block" blockn))
    (progn
  	(entmake (list (cons 0 "BLOCK")(cons 2 blockn)(cons 70 2)
                       (cons 10 (list 0 0 0))))
  	(entmake (list (cons 0 "CIRCLE")(cons 8 "0")(cons 10 (list cirx cirx 0))
                       (cons 40 (/ cirdia 2))
                       (cons 210 (list 0 0 1))(cons 62 256)(cons 39 0)
                       (cons 6 "BYLAYER")))
	(entmake (list (cons 0 "LINE")(cons 8 "0")
		       (cons 10 (list 0 0 0)) (cons 11 (list ullen 0 0))
                       (cons 210 (list 0 0 1))(cons 62 256)(cons 39 0)
                       (cons 6 "BYLAYER")))      
        (setq i 0)
        (repeat natt
          (setq tagstr (car (nth i flaglist)))
          (setq promptstr (cadr (nth i flaglist)))
	  (setq posi (nth i poslist))    ;(x y "���ɹ��")
          (setq y (- (* i texth -1.2) (* 0.5 texth)))
	  
          (entmake (list (cons 0 "ATTDEF")(cons 8 "0")
  		         ;(cons 10 (list gapfromcircle y 0))   ; insertion point
			 (cons 10 (list (car posi) (cadr posi))) ; insertion point
			 (if (> (nth 2 posi)) (cons 11 (list (car posi) (cadr posi))))
                         (cons 40 texth)
                         (cons 1 tagstr)(cons 3 promptstr)
                         (cons 2 Tagstr)
                         (cons 70 0)(cons 73 0)(cons 50 0)(cons 41 1)(cons 51 0)
                         (cons 7 "STANDARD")(cons 71 0)
			 (cons 72 (nth 2 posi))   ;horizontal text���� 0:left, 1:center 2: right 4:middle
                         
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



;------------------
; function : djdg_calctrlen
;            ö���� �� ���̸� ����(joint���)
;	Yi Suk Jong
;	05/12/20
;------------------
; (djdg_calctrlen attv)  --> (netlen totlen jointn jointl)
;  attv : attribute���� ����� ��list.(("Mark1" . "A") ("Var1" . "A=500") ...)
(defun djdg_calctrlen(attv
		      / v1 v2 v3 v4 v5 eqstr temp fckv corverv ctcv unitlv upv
		      	diastr dl netlen jlen jnum totlen netlencon totlencon jointcon
		      	oldlen oldlent oldjoint newattv )
  (setq v1 (cdr (assoc "VAR1" attv))
	v2 (cdr (assoc "VAR2" attv))
	v3 (cdr (assoc "VAR3" attv))
	v4 (cdr (assoc "VAR4" attv))
	v5 (cdr (assoc "VAR5" attv)))
  (setq eqstr (cdr (assoc "EQ" attv)))   ;����.
  
  (setq temp (djdg_splitstr (cdr (assoc "FCK" attv)) ":"))
  (if (> (length temp) 1) (setq fckv (atof (nth 1 temp))) (setq fckv (atof (nth 0 temp))))
  
  (setq temp (djdg_splitstr (cdr (assoc "COVER" attv)) ":"))
  (if (> (length temp) 1) (setq coverv (atof (nth 1 temp))) (setq corverv (atof (nth 0 temp))))

  (setq temp (djdg_splitstr (cdr (assoc "CTC" attv)) ":"))
  (if (> (length temp) 1) (setq ctcv (atof (nth 1 temp))) (setq ctcv (atof (nth 0 temp))))
  
  (setq temp (djdg_splitstr (cdr (assoc "UNITL" attv)) ":"))
  (if (> (length temp) 1) (setq unitlv (djdg_dimtxtoreal (nth 1 temp))) (setq unitlv (djdg_dimtxtoreal (nth 0 temp))))

  (setq upv (strcase (cdr (assoc "UP" attv))))
  (if (= upv "UP") (setq upv "UP") (setq upv "NUP"))
  
  (setq diastr (cdr (assoc "DIA" attv)))
  
  (setq dl nil)
  (if (/= v1 "") (setq dl (append dl (list v1)))) ;data list ("A=450" "B=345")
  (if (/= v2 "") (setq dl (append dl (list v2)))) ;data list ("A=450" "B=345")
  (if (/= v3 "") (setq dl (append dl (list v3)))) ;data list ("A=450" "B=345")
  (if (/= v4 "") (setq dl (append dl (list v4)))) ;data list ("A=450" "B=345")
  (if (/= v5 "") (setq dl (append dl (list v5)))) ;data list ("A=450" "B=345")
  
  (setq netlen (djdg_calcrlen dl eqstr))			;ö�ٱ���(������)
  (setq jlen (djdg_laplen fckv diastr ctcv coverv upv))   ;���������� ;(fck rebarn gap cover sang)
  (setq jnum (djdg_lapnum netlen unitlv jlen))   ;������ ���Ҽ�.
  (setq totlen (+ netlen (* jlen jnum)))
  
  (setq netlencon (cons "LEN" (strcat "L=" (rto_dimtxt netlen)))) ;(rto_dimtxt 1000)  --> "1.000"
  
  (if (= jnum 0)				;�������� ���� ��.
    (progn
      (setq totlencon (cons "LENT" ""))
      (setq jointcon (cons "JOINT" ""))
    );progn
    (progn
      (setq totlencon (cons "LENT" (strcat "L'=" (rto_dimtxt totlen))))
      (setq jointcon (cons "JOINT" (strcat "J=" (itoa (fix jnum)) "X" (rto_dimtxt jlen))))      
    );progn  
  );if
  (setq oldlen (assoc "LEN" attv)
	oldlent (assoc "LENT" attv)
	oldjoint (assoc "JOINT" attv))
	
  
  (setq newattv (subst  netlencon oldlen attv))
  (setq newattv (subst totlencon oldlent newattv))
  (setq newattv (subst jointcon oldjoint newattv))
  
;  (list netlencon totlencon jointcon)

  
  
);defun


;------------------------
; function : djdg_rattmod
;            �־��� attƯ������ att block�� �����Ѵ�(entmod�� ���)
;     	Yi Suk Jong
;	05/12/20
;------------------------
; (djdg_rattmod ename att) --> ����.
;    ename : entity name (insert��)
;    att   : att����(("MARK1" . "A1") ("LEN" . "L=12.000") ....)
(defun djdg_rattmod( ename att / tvlist)
  
  (setq tvlist (append nil (list (list "LEN" (cdr (assoc "LEN" att))))))
  (setq tvlist (append tvlist (list (list "LENT" (cdr (assoc "LENT" att))))))
  (setq tvlist (append tvlist (list (list "JOINT" (cdr (assoc "JOINT" att))))))
  (djdg_attwrite ename tvlist)
  
);defun

;---------------------------------
; program : ER (Edit Rebar)
;	Rebar Detail Attribute Block�� ����.
;	Yi Suk Jong
;	05/12/21
;---------------------------------
; ������ ���ϴ� tag�� �����ϸ� ������ �� �ִ� â�� �߰�.
; ������ ������ ����������, �ѱ��� ���� �ٽ� ����Ͽ� update�Ѵ�.
;---------------------------------
(defun c:er( / s_ent ename edatresult txt tag oldatt newatt )
  (setq s_ent (entsel "\n������ �׸��� �����ϼ���: "))
  
       (setq ename (car s_ent))   		;block name
       (setq edatresult (djdg_gettag s_ent))		;edat�Լ���� --> (tag value)
       (if (/= edatresult nil)
	 (progn
           (setq txt (cadr edatresult))             ; �� TEXT���ϱ�.
           (setq tag (car edatresult))		;tag
           (NV_DIALOG txt)                          ; Dialog Box���Ͽ� text�Է�.
           (djdg_attwrite ename (list (list tag (strcase new_txt))))  ;attribute ����.
	);progn 
      );if
  
  (setq oldatt (djdg_getattv ename))    ;�� att���� ���ϱ�. 
  (setq newatt (djdg_calctrlen oldatt)) ;���������� �� �ѱ��� �ٽ� ���.
  (djdg_rattmod ename newatt)		;attribute ����.
  (djdg_arrangemark ename)		;marking����.
  
);defun  



;------------------------------------------
; function : NV_dialog (New Value)
;		= ���·� �̷���� ���� ������ �� �ִ� ���̾�� �ڽ�����.
;	Yi Suk Jong
;	05/12/21
;------------------------------------------
; (nv_dialog old_txt)
; old_txt : ���� text
;------------------------------------------
(defun NV_DIALOG ( old_txt / splited var value dcl_id old_txt )
;---- funtion set_txt ���̾�α� �ڽ����� ���޾Ƽ� new_txt(��������)�� ����.
  (defun SET_TXT( / in tagstr valuestr )
    (setq tagstr (get_tile "tag_edit")
	  valuestr (get_tile "value_edit"))
    (if (/= tagstr "")
      (setq in (strcat tagstr "=" valuestr))
      (setq in valuestr)
    );if  
    (setq new_txt in)			
  ) ; of defun SET_TXT

  
  (setq splited (djdg_splitstr old_txt "="))  ;���� text�� "="�������� ����.
  (if (> (length splited) 1)
    (setq var (nth 0 splited)			;����
	  value (nth 1 splited))		;��
    (setq var ""
          value old_txt)
  );if   
  (setq dcl_id (load_dialog "DJDG"))
  (if (not (new_dialog "nvdialog" dcl_id)) (exit))

  (set_tile "tag_edit" var)
  (set_tile "value_edit" value)

  (action_tile "tag_edit" "(set_txt)")
  (action_tile "value_edit" "(set_txt)")
  (action_tile "accept" "(set_txt)(done_dialog)")
  (action_tile "cancel" "(set_txt)")
  (mode_tile "value_edit" 2)
  (start_dialog)
  (unload_dialog dcl_id)

) ;of defun NT_DIALOG



;------------------------------------------
; function : ERA_dialog (Edit Rebar All)
;		��� rebar attribute�� ��ĥ �� �ִ� ���̾�α� �ڽ� ����
;	Yi Suk Jong
;	05/12/22
;------------------------------------------
; (nv_dialog attv)
; attv  : ���� attribut��
;------------------------------------------
(defun ERA_DIALOG ( attv / splited var value dcl_id old_txt )
;  (setq splited (djdg_splitstr old_txt "="))  ;���� text�� "="�������� ����.

;---- funtion set_txt ���̾�α� �ڽ����� ���޾Ƽ� new_txt(��������)�� ����.
  (defun SET_ERA( / fck cover ctc unitl mark dia var1 var2 var3 var4 var5 num eqv temp mark1 mark2 natt)
    (setq fck (get_tile "fck")
	  cover (get_tile "cover")
	  ctc (get_tile "ctc")
	  unitl (get_tile "unitl")
      	  mark (get_tile "mark")
	  dia (get_tile "dia")
	  ;--------������ ���� ������.
	  var1 (strcat (get_tile "var1l") "=" (get_tile "var1v"))
	  var2 (strcat (get_tile "var2l") "=" (get_tile "var2v"))
	  var3 (strcat (get_tile "var3l") "=" (get_tile "var3v"))
	  var4 (strcat (get_tile "var4l") "=" (get_tile "var4v"))
	  var5 (strcat (get_tile "var5l") "=" (get_tile "var5v"))
	  num (get_tile "num")
	  eqv (get_tile "eq")
	  )
    ;------ mark�� mark1 mark2 2�ٷ� �и�.
    (if (and (> (strlen mark) 2)  (> (str_position mark "-") 0))
      (setq temp (djdg_splitstr mark "-")	;-�� �ִ� ���
	    mark1 (car temp)
	    mark2 (strcat "-" (cadr temp)))
      (setq mark1 mark				;-�� ���� ���
	    mark2 "")
    );if
    ;------ ����ִ� ���� ó��
    (if (or (= var1 "=") (< (length (djdg_splitstr var1 "=")) 2 )) (setq var1 ""))	;var1�� =�ΰ�� (�� ����� ���) ""��
    (if (or (= var2 "=") (< (length (djdg_splitstr var2 "=")) 2 )) (setq var2 ""))
    (if (or (= var3 "=") (< (length (djdg_splitstr var3 "=")) 2 )) (setq var3 ""))
    (if (or (= var4 "=") (< (length (djdg_splitstr var4 "=")) 2 )) (setq var4 ""))
    (if (or (= var5 "=") (< (length (djdg_splitstr var5 "=")) 2 )) (setq var5 ""))
    ;------- num�� N=�߰�
    (setq num (strcat "N=" num))
    
    ;------- cons�� ����
    (setq natt attv)
    (setq natt  (subst  (cons "FCK" fck) 	(assoc "FCK" natt) natt))
    (setq natt  (subst  (cons "COVER" cover) 	(assoc "COVER" natt) natt))
    (setq natt  (subst  (cons "CTC" ctc) 	(assoc "CTC" natt) natt))
    (setq natt  (subst  (cons "UNITL" unitl) 	(assoc "UNITL" natt) natt))
    (setq natt  (subst  (cons "MARK1" mark1) 	(assoc "MARK1" natt) natt))
    (setq natt  (subst  (cons "MARK2" mark2) 	(assoc "MARK2" natt) natt))    
    (setq natt	(subst	(cons "DIA" dia ) 	(assoc "DIA" natt) natt))
    (setq natt	(subst	(cons "VAR1" var1)	(assoc "VAR1" natt) natt))
    (setq natt	(subst	(cons "VAR2" var2)	(assoc "VAR2" natt) natt))
    (setq natt	(subst	(cons "VAR3" var3)	(assoc "VAR3" natt) natt))
    (setq natt	(subst	(cons "VAR4" var4)	(assoc "VAR4" natt) natt))
    (setq natt	(subst	(cons "VAR5" var5)	(assoc "VAR5" natt) natt))
    (setq natt	(subst	(cons "NUM" num)	(assoc "NUM" natt) natt))
    (setq natt	(subst	(cons "EQ" eqv)	(assoc "EQ" natt) natt))
    (setq #natt natt)  ;���������� ����.
  ) ; of defun SET_TXT
  
  ;;-------  main program ����


  (setq fck (cdr (assoc "FCK" attv)))
  (setq cover (cdr (assoc "COVER" attv)))
  (setq ctc (cdr (assoc "CTC" attv)))
  (setq unitl (cdr (assoc "UNITL" attv)))
  (setq up (cdr (assoc "UP" attv)))
  
  (setq mark (strcat (cdr (assoc "MARK1" attv)) (cdr (assoc "MARK2" attv))))  
  (setq dia (cdr (assoc "DIA" attv)))
  (setq var1 (djdg_splitstr (cdr (assoc "VAR1" attv)) "="))
  (setq var2 (djdg_splitstr (cdr (assoc "VAR2" attv)) "="))
  (setq var3 (djdg_splitstr (cdr (assoc "VAR3" attv)) "="))
  (setq var4 (djdg_splitstr (cdr (assoc "VAR4" attv)) "="))
  (setq var5 (djdg_splitstr (cdr (assoc "VAR5" attv)) "="))  
  (if (= var1 nil) (setq var1l "" var1v "") (setq var1l (car var1) var1v (cadr var1))) ;VAR�� ����ִ� ��� 
  (if (= var2 nil) (setq var2l "" var2v "") (setq var2l (car var2) var2v (cadr var2)))
  (if (= var3 nil) (setq var3l "" var3v "") (setq var3l (car var3) var3v (cadr var3)))
  (if (= var4 nil) (setq var4l "" var4v "") (setq var4l (car var4) var4v (cadr var4)))
  (if (= var5 nil) (setq var5l "" var5v "") (setq var5l (car var5) var5v (cadr var5)))  
  (setq num (cadr (djdg_splitstr (cdr (assoc "NUM" attv)) "=")))  ;���ڸ� ����
  (setq eqv (cdr (assoc "EQ" attv))) 



  
;;;  (if (> (length splited) 1)
;;;    (setq var (nth 0 splited)			;����
;;;	  value (nth 1 splited))		;��
;;;    (setq var ""
;;;          value old_txt)
;;;  );if
  
  (setq dcl_id (load_dialog "DJDG"))
  (if (not (new_dialog "ERA_DLG" dcl_id)) (exit))
  
  ;---- �ʱ�ȭ
  (set_tile "fck" fck)
  (set_tile "cover" cover)
  (set_tile "ctc" ctc)
  (set_tile "unitl" unitl)
  (if (= up "UP")
    (set_tile "up" "1")
    (set_tile "up" "0")
  );if  
  
  (set_tile "mark" mark)
  (set_tile "dia" dia)
  (set_tile "var1l" var1l) (set_tile "var1v" var1v)
  (set_tile "var2l" var2l) (set_tile "var2v" var2v)
  (set_tile "var3l" var3l) (set_tile "var3v" var3v)
  (set_tile "var4l" var4l) (set_tile "var4v" var4v)
  (set_tile "var5l" var5l) (set_tile "var5v" var5v)
  (set_tile "num" num)
  (set_tile "eq" eqv) 
  
  
  (action_tile "mark1" "")
  (action_tile "mark2" "")
  (action_tile "dia" "")
  (action_tile "var1" "")
  (action_tile "var2" "")
  (action_tile "var3" "")
  (action_tile "var4" "")
  (action_tile "var5" "")
  (action_tile "num" "")
  (action_tile "eq" "")
  
  (action_tile "accept" "(set_era)(done_dialog)")
  
  (action_tile "cancel" "(done_dialog)")

  (start_dialog)
  (unload_dialog dcl_id)


  
) ;of defun NT_DIALOG

;-------------------------------------------
; program : ERA (Edit Rebar attribute (All)
;	���̾�α� �ڽ��� ���� ��� Attribute�� �Ѳ����� ����
;	Yi Suk Jong
;	05/12/23
;-------------------------------------------
(defun c:era(
	     / att newatt newatt1)
  (setq ename (car (entsel "\n������ Entity�� �����ϼ���: ")))
  (setq att (djdg_getattv ename))
  
  (ERA_DIALOG att)		;dialog box�� �� �� �ޱ�.
  
  (setq newatt att)
  (foreach acc #natt		;attribute list�� ��ȭ���ڿ��� �Է¹��� ��(�������� #natt)���� ����.
    (setq newatt (subst acc (assoc (car acc) newatt) newatt))
  );foreach
  
  (setq newatt (djdg_calctrlen newatt)) ;���������� �� �ѱ��� �ٽ� ���.
  (setq newatt1 nil)
  (foreach acc newatt		;attribute list ����
    (setq newatt1 (append newatt1 (list (list (car acc) (cdr acc)))))
  );foreach
  
  (djdg_attwrite ename newatt1)  	;���� attribute ����.
  (djdg_arrangemark ename)		;marking ����.
;  (djdg_rattmod ename newatt)		;attribute ����.
	
);defun


;------------------------
; function : djdg_arrangemark
;	arrange marking, djdg_ratt�� marking�� ���ȿ�
;	�������� ����(�����ϰ��� ������ ��� ����)
;	Yi Suk Jong
;	05/12/24
;------------------------
; (djdg_arrangemark ename)
; ename: djdg_ratt insert block�� entity name
(defun djdg_arrangemark(ename
			/ ienti ip sc bp ceni ccp pfrb m1 m2 m1p m2p m2s cp nm1 m1b m1bd mr1)
  (setq ienti (entget ename))		;insert block�� entity����.
  (setq ip (cdr (assoc 10 ienti)))      ;insert point
  (setq sc (cdr (assoc 41 ienti)))	;scale factor

  (setq bp (cdr (assoc 10 (tblsearch "block" "djdg_ratt")))) ;djdg_ratt block������ base point
  
  (setq ceni (entget (car (djdg_entinblk "djdg_ratt" 0 "CIRCLE"))))		;circle entity����.
  (setq ccp (cdr (assoc 10 ceni)))	;circle center point

  (setq pfrb (mapcar '- ccp bp))	;circle center point�� basepoint�κ��� ������ �Ÿ�.
  
  (setq m1 (djdg_getattenti ename "MARK1"))  ;mark1�� entity����
  (setq m2 (djdg_getattenti ename "MARK2"))

  (setq m1p (cdr (assoc 11 m1))			;mark1 , mark2�� insert point
        m2p (cdr (assoc 11 m2)))

  (setq m2s (cdr (assoc 1 m2)))		;mark2�� ����.

  (if (= m2s "")			;mark2�� ���� ���
    (progn
      (setq cp (mapcar '+ ip (mapcar '(lambda (x) (* x sc)) pfrb)))  ; ���� circle�� center point(scale����).
      (setq nm1 (subst (cons 11 cp) (assoc 11 m1) m1))
      (entmod nm1)   					;m1 modify.
      (entupd ename) 					;insert��ü update
    );progn
    (progn
      (setq m1b (cdr (assoc 11 (entget (car (djdg_entinblk "djdg_ratt" 2 "MARK1"))))))  ;block table�������� mark1 point
      (setq m1bd (mapcar '- m1b bp)) 		;block table���� mark1�� base point�� �Ÿ�
      (setq mr1 (mapcar '+ ip (mapcar '(lambda (x) (* x sc)) m1bd)))  ;block table ���� mark1�� insert point
      (setq nm1 (subst (cons 11 mr1) (assoc 11 m1) m1))
      (entmod nm1)   					;m1 modify.
      (entupd ename) 					;insert��ü update
    );  
  );if
);defun   


;------------------------
; function : djdg_getattenti
;	attibute entity������ code �� value�� �ش��ϴ� entity ������ ����
;	Yi Suk Jong
;	05/12/24
;------------------------
; (djdg_getattenti ename tag)
; ename : entity name
; code: ���ϰ��� �ϴ� attribute�� code
; value: ���ϰ��� �ϴ� attribute�� code�� �ش��ϴ� value
;  ex) (djdg_getattenti ename 0 "CIRCLE"), (djdg_getattenti ename 2 "MARK"), 
(defun djdg_getattenti(ename tag
			/ en eni )
  (setq en ename)
  (while (and (/= (cdr (assoc 2 (setq eni (entget (setq en (entnext en)))))) tag)
	      (/= (cdr (assoc 0 eni)) "SEQEND"))
    (setq eni nil)
  );while  
  (if (= (cdr (assoc 0 eni)) "SEQEND") nil eni)
);defun   

;-----------------------------
; function : djdg_entinblk(block table�� entity ������ return)
;		entity infomation in block
;	Yi Suk Jong
;	05/12/26
;-----------------------------
; (djdg_entinblk blockname code value)  --> ((-1 . <Entity name: 7ef72198>) ...�ݺ�... )
;	blockname : block name
;	code : �� code	ex) 0
;	value: ã�ƾ��� code�� ex) "LINE"
;	ex) entity type�� LINE�� (djdg_entinblk "djdg_ratt" 0 "LINE")
; return: �߰ߵ� entity name lists
;-----------------------------
(defun djdg_entinblk( blkname code value / return blk fen feni nen)
  (setq return nil)			;return �ʱ�ȭ.
  (setq blk (tblsearch "block" blkname))  ;block table ����.
  (setq fen (cdr (assoc -2 blk)))		;ù��° entity name
  (setq feni (entget fen))		;ù��° entity ����.
  (if (= (cdr (assoc code feni)) value)	;ù��° entity�� ã�� entity�϶�.
    (setq return (append return (list fen)))
  );if
  (setq nen fen)		
  (while (setq nen (entnext nen))	 ;���� entity
    (if (= (cdr (assoc code (entget nen))) value)
      (setq return (append return (list nen)))
    );if
  );while
  return
);defun

      