;;;' function :devlen1 (net 정착길이) 단위: mm2
;;;' function :devlen (정착길이,10mm단위로 올림) 단위: mm2
;;;' function :laplen (겹이음길이,10mm단위로 올림) 단위: mm2
;;;' function :rebarndia (철근공칭직경) 단위: mm
;;;' function : djdg_lapnum (겹이음개소수)
;;;' function : calcrlen (철근길이 구하기)
; function : djdg_defratt : 철근 attribute 정의
; function : djdg_calctrlen:철근의 총 길이를 구함(joint고려)
; function : djdg_rattmod;            주어진 att특성으로 att block을 수정한다(entmod와 비슷)
; program : ER (Edit Rebar);	Rebar Detail Attribute Block을 수정.
; function : NV_dialog (New Value);		= 형태로 이루어진 값을 수정할 수 있는 다이얼로 박스제공.
; function : ERA_dialog (Edit Rebar All);		모든 rebar attribute를 고칠 수 있는 다이얼로그 박스 제공
; program : ERA (Edit Rebar attribute (All);	다이얼로그 박스를 통해 모든 Attribute를 한꺼번에 수정
; function : djdg_arrangemark;	arrange marking, djdg_ratt의 marking을 원안에;	최적으로 정렬(한줄일경우와 두줄일 경우 구분)
; function : djdg_getattenti ;	attibute entity내에서 code 가 value에 해당하는 entity 정보를 취함
; function : djdg_entinblk(block table내 entity 정보를 return)

;---------------------------------------------------------------
; program : erebar
;           Enhenced Rebar
;           attribute를 이용하여 자동으로 철근길이/이음장 계산
;           Yi Suk Jong
;           05/12/20
;--------------------------------------------------------------



;------------------------------
; function : djdg_insertelp
;            insert el pnt (사용자에게 점을 입력받아 elpnt블럭을 인서트한다.
;	     만일 elpnt block이 정의 되어있지 않다면 정의한 후 인서트한다.
;	     주어진 값으로 attribute 값을 기록한다.
;            Yi Suk Jong
;	     05/08/08
;------------------------------
(defun djdg_insertelp( ipnt flaglist /
		      	 blockn gapfromcircle texth dimscale i tagstr valuestr x y )
  (setq blockn "djdg_elp")

  (setq gapfromcircle 2)
  (setq texth 2.0)

  ;dimscale적용
  (setq dimscale (getvar "dimscale"))
  (setq gapfromcircle (* gapfromcircle dimscale)
	texth (* texth dimscale))
  
  (if (/= nil (tblsearch "block" blockn))
    (progn
      (entmake (list (cons 0 "INSERT")(cons 8 "0")(cons 66 1)(cons 2 "djdg_elp")
                     (cons 10 ipnt)   ;삽입점
       		     (cons 41 dimscale)(cons 42 dimscale)(cons 43 dimscale)
                     (cons 50 0)(cons 70 0)(cons 71 0)(cons 44 0)(cons 45 0)
                     (cons 210 (list 0 0 1))(cons 62 256)(cons 39 0)
                     (cons 6 "BYLAYER")))
      (setq i 0)
      (foreach flag flaglist
        (setq tagstr (car flag)	;flag string
	    valuestr (cadr flag)) ;value string

	(setq x (+ (car ipnt) gapfromcircle)) 			;attribute 삽입점 x
	(setq y (- (cadr ipnt) (* i texth 1.2) (* 0.5 texth)))   ;attribute 삽입점 y
         
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
;;;' function :devlen1 (net 정착길이) 단위: mm2
;;;'           Yi Suk Jong
;;;'           05/12/20
;;;' 변수: fck : 콘크리트설계강도 (MPa)
;;;'       rebarn : 철근명칭("D25", "H25")
;;;'       gap : 철근간격(mm)
;;;'       cover : 철근피복(mm)
;;;        sang : 상부철근 여부(T/nil)
;;;'------------------------
(defun djdg_devlen1( fck rebarn gap cover sang
		    / c rdia fy gamma rdb cdb return)
  (if (< cover (/ gap 2.0))
    (setq c cover)
    (setq c (/ gap 2.0))
  );if

  (setq rdia (atof (substr rebarn 2 2)))     ;철근직경.

  (if (= (strcase (substr rebarn 1 1)) "H")
    (setq fy 400)
    (setq fy 300)
  );if  
    
  (if (<= rdia 19)
    (setq gamma 0.8)
    (setq gamma 1.0)
  );if

  (setq rdb (djdg_rebarndia rebarn))   ;철근의 공칭 지름.

  (setq cdb (/ c  rdb))  ;(c/db)

  (If (> cdb  2.5) (setq  cdb  2.5))

  (if (= sang "UP") (setq up 1.3) (setq up 1.0))   ;상부철근인 경우 x1.3	 
	 
  (setq return (/ (* 0.9  rdb  fy gamma up)  (expt fck  0.5)  cdb))

) ;defun  


;;;'------------------------
;;;' function :devlen (정착길이,10mm단위로 올림) 단위: mm2
;;;'           Yi Suk Jong
;;;'           05/12/15
;;;' 변수: fck : 콘크리트설계강도 (MPa)
;;;'       rebarn : 철근명칭("D25", "H25")
;;;'       gap : 철근간격(mm)
;;;'       cover : 철근피복(mm)
;;;'------------------------
(defun djdg_devlen(fck rebarn gap cover sang
		   / devlen2 mok rem10 return)

  (setq devlen2 (djdg_devlen1 fck rebarn gap cover sang))

  (setq mok (fix (/ devlen2 10.0)))

  (setq rem10 (- devlen2 (* mok 10)))   ;10으로 나눈 나머지

  (if (> rem10  0)
    (setq return (* (+ mok 1) 10))
    (setq return devlen2)
  );if

  (if (< return 300) (setq return 300))

  return
);defun  


;;;'------------------------
;;;' function :laplen (겹이음길이,10mm단위로 올림) 단위: mm2
;;;'           Yi Suk Jong
;;;'           05/12/15
;;;' 변수: fck : 콘크리트설계강도 (MPa)
;;;'       rebarn : 철근명칭("D25", "H25")
;;;'       gap : 철근간격(mm)
;;;'       cover : 철근피복(mm)
;;;'       snag : 상부철근 여부 (상부철근: "상", 아닌경우: "")
;;;'---------------------------------
(defun djdg_laplen(fck rebarn gap cover sang
		   /  devlen2 mok rem10 return)
  
;;;Function laplen(fck As Double, rebarn As String, gap As Double, cover As Double, sang As String)
  (setq devlen2 (* (djdg_devlen1 fck rebarn gap cover sang) 1.3))
  
;;;devlen2 = devlen1(fck, rebarn, gap, cover) * 1.3
;;;If sang = "상" Then devlen2 = devlen2 * 1.3
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
;;;' function :rebarndia (철근공칭직경) 단위: mm
;;;'           Yi Suk Jong
;;;'           05/12/20
;;;' 변수: dia : 철근명칭(string) 또는 dia(double) (Variant)
;;;'------------------------
;;;Function rebarndia(dia As Variant)
(defun djdg_rebarndia( dia / ddia return)
  (setq ddia (atof (substr dia 2 2)))   ;철근직경.
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
;;;' function : djdg_lapnum (겹이음개소수)
;;;'            Yi Suk Jong
;;;'            2005.12.15
;;;'  인자: tlen : 총길이 m
;;;'        rlen : 철근길이(ex:8미터 철근인 경우 8) m
;;;'        llen : 겹이음길이 m
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
;;;' function : calcrlen (철근길이 구하기)
;;;'            Yi Suk Jong
;;;'            2005.12.16
;;;'  인자: data : 변수들이 들어있는 list ("A=234" "B=234~345")
;;;'        func : 계산수식 (ex:"A+B*2") string
;;;'---------------------------
(defun djdg_calcrlen(data func
		     / id datalist spdata tlen arp narp i sp arxx narx tx j sx sxvstr arm sxv)

  ;--------- data를 받아서 ("A" . "234")형식으로 만들어줌.
  (setq id 0
	datalist nil)
  (repeat (length data)
    (setq spdata (djdg_splitstr (nth id data) "="));split된 data
    (setq datalist (append datalist (list (cons (strcase (nth 0 spdata)) (nth 1 spdata)))))
    (setq id (1+ id)) ;다음 id로
  );repeat

  
  (setq tlen 0)
  (setq arp (djdg_splitstr func "+")) ;func를 +를 기준으로 나눔
  (setq narp (length arp))		;+로 나눠진 갯수.
  (setq i 0)

  (repeat narp
    (setq sp (strcase (nth i arp)))  ;+로 나누어진 text를 대문자로
    (setq arxx (djdg_splitstr sp "*")) ;*를 기준으로 나눔
    (setq narx (length arxx))
    (setq tx 1)
    (setq j 0)
    (repeat narx
	  
      (setq sx (nth j arxx))
      (if (is-num sx)
	(setq sxvstr sx)			   ;숫자인경우
        (setq sxvstr (cdr (assoc sx datalist)))    ;변수인 경우 변수에 대항하는 값string
      );if	
      ;---- 물결로 나뉜경우.
      (if (/= sxvstr nil)    ;sxvstr이 nil이 아닌경우만 숫자화. nil인 경우는 변수에 값이 없는 경우임.
	(progn
      	  (setq arm (djdg_splitstr sxvstr "~"))
          (if (> (length arm) 1)
	    (setq sxv (/ (+ (djdg_dimtxtoreal (nth 0 arm)) (djdg_dimtxtoreal (nth 1 arm))) 2.0))  ;물결있는 경우 평균값취함.
	    (setq sxv (djdg_dimtxtoreal (nth 0 arm)))  ;물결없는 경우 string을 실수로..
          );if	
          (setq tx (* tx sxv))
	);progn
	(progn		;변수에 대항하는 값이 nil일때
	  (setq tx 0)	;곱셈의 결과를 0으로..
	  (alert (strcat "변수 " sx " 의 값이 존재하지 않습니다."))
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
;            rebar attribute block을 정의한다.
;            Yi Suk Jong
;            05/12/20
;---------------------------------------

(defun	djdg_defratt( /
		      blockn ullen cirdia texth gapfromline gapfromcir gapmark
		     flaglist cirx mark1x mark1y mark2y diax diay lenx lentx numx
		     ullen5 var1x var1y var2x var3x var4x var5x jointx fckx fcky
		     ullen6 coverx ctcx unitlx upx eqx poslist natt i tagstr promptstr posi y)
  (setq blockn "djdg_ratt")
  ;------ 각종변수 정의
  (setq ullen 100   	;밑줄 길이(mm)
	cirdia 10
	texth 2.5)
;  (setq radius 1)
  (setq gapfromline 1)    ;밑줄부터 text까지 거리(mm)  y
  (setq gapfromcir 1)  ;원 오른쪽 끝에서부터 첫text좌측까지 거리(mm) x
  (setq gapmark 1)

  ;  (setq texth 2.0)
  (setq flaglist '(("Mark1" "철근이름1")
		   ("Mark2" "철근이름2")
		   ("Dia" "철근직경(ex:H29)")
		   ("Len" "철근길이")
		   ("Lent" "겹이음장 고려 철근길이")
		   ("Num" "철근갯수")
		   ("Var1" "변수1")
		   ("Var2" "변수2")
		   ("Var3" "변수3")
		   ("Var4" "변수4")
		   ("Var5" "변수5")
		   ("Joint" "겹이음개수x길이")
  		   ("fck" "fck")
		   ("Cover" "피복(mm)")
		   ("ctc" "C.T.C(mm)")
		   ("UnitL" "철근단위길이(mm)")
		   ("Up" "상부철근여부(ex:UP)")
		   ("Eq" "계산수식")
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
	unitlx   (+ diax (* ullen6 3))  ;철근단위길이(m)
	upx    (+ diax (* ullen6 4))
	eqx    (+ diax (* ullen6 5)))
  
  (setq poslist (list   (list mark1x mark1y 4)	;text포지션 list(flaglist와 짝을 이룬다. 좌표는 삽입점인 라인의 욎�?끝부터..
	  	  	(list mark1x mark2y 4)	;정렬방식 0: left, 1: center 2: right 4: middle
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
	 
  (setq natt (length flaglist))  ;att 갯수
  
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
	  (setq posi (nth i poslist))    ;(x y "정령방법")
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
			 (cons 72 (nth 2 posi))   ;horizontal text정렬 0:left, 1:center 2: right 4:middle
                         
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
;            철근의 총 길이를 구함(joint고려)
;	Yi Suk Jong
;	05/12/20
;------------------
; (djdg_calctrlen attv)  --> (netlen totlen jointn jointl)
;  attv : attribute에서 추출된 쌍list.(("Mark1" . "A") ("Var1" . "A=500") ...)
(defun djdg_calctrlen(attv
		      / v1 v2 v3 v4 v5 eqstr temp fckv corverv ctcv unitlv upv
		      	diastr dl netlen jlen jnum totlen netlencon totlencon jointcon
		      	oldlen oldlent oldjoint newattv )
  (setq v1 (cdr (assoc "VAR1" attv))
	v2 (cdr (assoc "VAR2" attv))
	v3 (cdr (assoc "VAR3" attv))
	v4 (cdr (assoc "VAR4" attv))
	v5 (cdr (assoc "VAR5" attv)))
  (setq eqstr (cdr (assoc "EQ" attv)))   ;수식.
  
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
  
  (setq netlen (djdg_calcrlen dl eqstr))			;철근길이(순길이)
  (setq jlen (djdg_laplen fckv diastr ctcv coverv upv))   ;겹이음길이 ;(fck rebarn gap cover sang)
  (setq jnum (djdg_lapnum netlen unitlv jlen))   ;겹이음 개소수.
  (setq totlen (+ netlen (* jlen jnum)))
  
  (setq netlencon (cons "LEN" (strcat "L=" (rto_dimtxt netlen)))) ;(rto_dimtxt 1000)  --> "1.000"
  
  (if (= jnum 0)				;겹이음이 없을 때.
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
;            주어진 att특성으로 att block을 수정한다(entmod와 비슷)
;     	Yi Suk Jong
;	05/12/20
;------------------------
; (djdg_rattmod ename att) --> 개정.
;    ename : entity name (insert블럭)
;    att   : att정보(("MARK1" . "A1") ("LEN" . "L=12.000") ....)
(defun djdg_rattmod( ename att / tvlist)
  
  (setq tvlist (append nil (list (list "LEN" (cdr (assoc "LEN" att))))))
  (setq tvlist (append tvlist (list (list "LENT" (cdr (assoc "LENT" att))))))
  (setq tvlist (append tvlist (list (list "JOINT" (cdr (assoc "JOINT" att))))))
  (djdg_attwrite ename tvlist)
  
);defun

;---------------------------------
; program : ER (Edit Rebar)
;	Rebar Detail Attribute Block을 수정.
;	Yi Suk Jong
;	05/12/21
;---------------------------------
; 수정을 원하는 tag를 선택하면 수정할 수 있는 창이 뜨고.
; 수정이 끝나면 겹이음길이, 총길이 등을 다시 계산하여 update한다.
;---------------------------------
(defun c:er( / s_ent ename edatresult txt tag oldatt newatt )
  (setq s_ent (entsel "\n수정할 항목을 선택하세요: "))
  
       (setq ename (car s_ent))   		;block name
       (setq edatresult (djdg_gettag s_ent))		;edat함수결과 --> (tag value)
       (if (/= edatresult nil)
	 (progn
           (setq txt (cadr edatresult))             ; 옛 TEXT구하기.
           (setq tag (car edatresult))		;tag
           (NV_DIALOG txt)                          ; Dialog Box통하여 text입력.
           (djdg_attwrite ename (list (list tag (strcase new_txt))))  ;attribute 갱신.
	);progn 
      );if
  
  (setq oldatt (djdg_getattv ename))    ;구 att정보 취하기. 
  (setq newatt (djdg_calctrlen oldatt)) ;겹이음길이 및 총길이 다시 계산.
  (djdg_rattmod ename newatt)		;attribute 갱신.
  (djdg_arrangemark ename)		;marking정렬.
  
);defun  



;------------------------------------------
; function : NV_dialog (New Value)
;		= 형태로 이루어진 값을 수정할 수 있는 다이얼로 박스제공.
;	Yi Suk Jong
;	05/12/21
;------------------------------------------
; (nv_dialog old_txt)
; old_txt : 이전 text
;------------------------------------------
(defun NV_DIALOG ( old_txt / splited var value dcl_id old_txt )
;---- funtion set_txt 다이얼로그 박스에서 값받아서 new_txt(전역변수)에 넣음.
  (defun SET_TXT( / in tagstr valuestr )
    (setq tagstr (get_tile "tag_edit")
	  valuestr (get_tile "value_edit"))
    (if (/= tagstr "")
      (setq in (strcat tagstr "=" valuestr))
      (setq in valuestr)
    );if  
    (setq new_txt in)			
  ) ; of defun SET_TXT

  
  (setq splited (djdg_splitstr old_txt "="))  ;기존 text를 "="기준으로 나눔.
  (if (> (length splited) 1)
    (setq var (nth 0 splited)			;변수
	  value (nth 1 splited))		;값
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
;		모든 rebar attribute를 고칠 수 있는 다이얼로그 박스 제공
;	Yi Suk Jong
;	05/12/22
;------------------------------------------
; (nv_dialog attv)
; attv  : 기존 attribut값
;------------------------------------------
(defun ERA_DIALOG ( attv / splited var value dcl_id old_txt )
;  (setq splited (djdg_splitstr old_txt "="))  ;기존 text를 "="기준으로 나눔.

;---- funtion set_txt 다이얼로그 박스에서 값받아서 new_txt(전역변수)에 넣음.
  (defun SET_ERA( / fck cover ctc unitl mark dia var1 var2 var3 var4 var5 num eqv temp mark1 mark2 natt)
    (setq fck (get_tile "fck")
	  cover (get_tile "cover")
	  ctc (get_tile "ctc")
	  unitl (get_tile "unitl")
      	  mark (get_tile "mark")
	  dia (get_tile "dia")
	  ;--------변수와 값을 묶어줌.
	  var1 (strcat (get_tile "var1l") "=" (get_tile "var1v"))
	  var2 (strcat (get_tile "var2l") "=" (get_tile "var2v"))
	  var3 (strcat (get_tile "var3l") "=" (get_tile "var3v"))
	  var4 (strcat (get_tile "var4l") "=" (get_tile "var4v"))
	  var5 (strcat (get_tile "var5l") "=" (get_tile "var5v"))
	  num (get_tile "num")
	  eqv (get_tile "eq")
	  )
    ;------ mark를 mark1 mark2 2줄로 분리.
    (if (and (> (strlen mark) 2)  (> (str_position mark "-") 0))
      (setq temp (djdg_splitstr mark "-")	;-가 있는 경우
	    mark1 (car temp)
	    mark2 (strcat "-" (cadr temp)))
      (setq mark1 mark				;-가 없는 경우
	    mark2 "")
    );if
    ;------ 비어있는 변수 처리
    (if (or (= var1 "=") (< (length (djdg_splitstr var1 "=")) 2 )) (setq var1 ""))	;var1이 =인경우 (즉 비었을 경우) ""로
    (if (or (= var2 "=") (< (length (djdg_splitstr var2 "=")) 2 )) (setq var2 ""))
    (if (or (= var3 "=") (< (length (djdg_splitstr var3 "=")) 2 )) (setq var3 ""))
    (if (or (= var4 "=") (< (length (djdg_splitstr var4 "=")) 2 )) (setq var4 ""))
    (if (or (= var5 "=") (< (length (djdg_splitstr var5 "=")) 2 )) (setq var5 ""))
    ;------- num에 N=추가
    (setq num (strcat "N=" num))
    
    ;------- cons로 묶기
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
    (setq #natt natt)  ;전역변수에 저장.
  ) ; of defun SET_TXT
  
  ;;-------  main program 시작


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
  (if (= var1 nil) (setq var1l "" var1v "") (setq var1l (car var1) var1v (cadr var1))) ;VAR이 비어있는 경우 
  (if (= var2 nil) (setq var2l "" var2v "") (setq var2l (car var2) var2v (cadr var2)))
  (if (= var3 nil) (setq var3l "" var3v "") (setq var3l (car var3) var3v (cadr var3)))
  (if (= var4 nil) (setq var4l "" var4v "") (setq var4l (car var4) var4v (cadr var4)))
  (if (= var5 nil) (setq var5l "" var5v "") (setq var5l (car var5) var5v (cadr var5)))  
  (setq num (cadr (djdg_splitstr (cdr (assoc "NUM" attv)) "=")))  ;숫자만 추출
  (setq eqv (cdr (assoc "EQ" attv))) 



  
;;;  (if (> (length splited) 1)
;;;    (setq var (nth 0 splited)			;변수
;;;	  value (nth 1 splited))		;값
;;;    (setq var ""
;;;          value old_txt)
;;;  );if
  
  (setq dcl_id (load_dialog "DJDG"))
  (if (not (new_dialog "ERA_DLG" dcl_id)) (exit))
  
  ;---- 초기화
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
;	다이얼로그 박스를 통해 모든 Attribute를 한꺼번에 수정
;	Yi Suk Jong
;	05/12/23
;-------------------------------------------
(defun c:era(
	     / att newatt newatt1)
  (setq ename (car (entsel "\n수정할 Entity를 선택하세요: ")))
  (setq att (djdg_getattv ename))
  
  (ERA_DIALOG att)		;dialog box로 새 값 받기.
  
  (setq newatt att)
  (foreach acc #natt		;attribute list를 대화상자에서 입력받은 값(전역변수 #natt)으로 갱신.
    (setq newatt (subst acc (assoc (car acc) newatt) newatt))
  );foreach
  
  (setq newatt (djdg_calctrlen newatt)) ;겹이음길이 및 총길이 다시 계산.
  (setq newatt1 nil)
  (foreach acc newatt		;attribute list 갱신
    (setq newatt1 (append newatt1 (list (list (car acc) (cdr acc)))))
  );foreach
  
  (djdg_attwrite ename newatt1)  	;도면 attribute 갱신.
  (djdg_arrangemark ename)		;marking 정렬.
;  (djdg_rattmod ename newatt)		;attribute 갱신.
	
);defun


;------------------------
; function : djdg_arrangemark
;	arrange marking, djdg_ratt의 marking을 원안에
;	최적으로 정렬(한줄일경우와 두줄일 경우 구분)
;	Yi Suk Jong
;	05/12/24
;------------------------
; (djdg_arrangemark ename)
; ename: djdg_ratt insert block의 entity name
(defun djdg_arrangemark(ename
			/ ienti ip sc bp ceni ccp pfrb m1 m2 m1p m2p m2s cp nm1 m1b m1bd mr1)
  (setq ienti (entget ename))		;insert block의 entity정보.
  (setq ip (cdr (assoc 10 ienti)))      ;insert point
  (setq sc (cdr (assoc 41 ienti)))	;scale factor

  (setq bp (cdr (assoc 10 (tblsearch "block" "djdg_ratt")))) ;djdg_ratt block정의의 base point
  
  (setq ceni (entget (car (djdg_entinblk "djdg_ratt" 0 "CIRCLE"))))		;circle entity정보.
  (setq ccp (cdr (assoc 10 ceni)))	;circle center point

  (setq pfrb (mapcar '- ccp bp))	;circle center point의 basepoint로부터 떨어진 거리.
  
  (setq m1 (djdg_getattenti ename "MARK1"))  ;mark1의 entity정보
  (setq m2 (djdg_getattenti ename "MARK2"))

  (setq m1p (cdr (assoc 11 m1))			;mark1 , mark2의 insert point
        m2p (cdr (assoc 11 m2)))

  (setq m2s (cdr (assoc 1 m2)))		;mark2의 내용.

  (if (= m2s "")			;mark2가 없는 경우
    (progn
      (setq cp (mapcar '+ ip (mapcar '(lambda (x) (* x sc)) pfrb)))  ; 실제 circle의 center point(scale적용).
      (setq nm1 (subst (cons 11 cp) (assoc 11 m1) m1))
      (entmod nm1)   					;m1 modify.
      (entupd ename) 					;insert전체 update
    );progn
    (progn
      (setq m1b (cdr (assoc 11 (entget (car (djdg_entinblk "djdg_ratt" 2 "MARK1"))))))  ;block table내에서의 mark1 point
      (setq m1bd (mapcar '- m1b bp)) 		;block table에서 mark1과 base point의 거리
      (setq mr1 (mapcar '+ ip (mapcar '(lambda (x) (* x sc)) m1bd)))  ;block table 내의 mark1의 insert point
      (setq nm1 (subst (cons 11 mr1) (assoc 11 m1) m1))
      (entmod nm1)   					;m1 modify.
      (entupd ename) 					;insert전체 update
    );  
  );if
);defun   


;------------------------
; function : djdg_getattenti
;	attibute entity내에서 code 가 value에 해당하는 entity 정보를 취함
;	Yi Suk Jong
;	05/12/24
;------------------------
; (djdg_getattenti ename tag)
; ename : entity name
; code: 취하고자 하는 attribute의 code
; value: 취하고자 하는 attribute의 code에 해당하는 value
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
; function : djdg_entinblk(block table내 entity 정보를 return)
;		entity infomation in block
;	Yi Suk Jong
;	05/12/26
;-----------------------------
; (djdg_entinblk blockname code value)  --> ((-1 . <Entity name: 7ef72198>) ...반복... )
;	blockname : block name
;	code : 비교 code	ex) 0
;	value: 찾아야할 code값 ex) "LINE"
;	ex) entity type이 LINE인 (djdg_entinblk "djdg_ratt" 0 "LINE")
; return: 발견된 entity name lists
;-----------------------------
(defun djdg_entinblk( blkname code value / return blk fen feni nen)
  (setq return nil)			;return 초기화.
  (setq blk (tblsearch "block" blkname))  ;block table 정보.
  (setq fen (cdr (assoc -2 blk)))		;첫번째 entity name
  (setq feni (entget fen))		;첫번째 entity 정보.
  (if (= (cdr (assoc code feni)) value)	;첫번째 entity가 찾는 entity일때.
    (setq return (append return (list fen)))
  );if
  (setq nen fen)		
  (while (setq nen (entnext nen))	 ;다음 entity
    (if (= (cdr (assoc code (entget nen))) value)
      (setq return (append return (list nen)))
    );if
  );while
  return
);defun

      