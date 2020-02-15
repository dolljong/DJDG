; function List
; djdg_vtcel;            종단고 구하기.
; djdg_interpolation : 선형보간 하여 값을 구해준다.
; 		usage: (interpolation vals sv)
;  		ex) (interpolation '((0 11.5 1) (4 20.0 1) ...) 3)
; djdg_vtcdeltay : 종단곡선 delta-y
; djdg_readvtcdata : 종곡선 data(*.eld)파일을 읽어옴.
; djdg_readdatafile : data file을 읽어준다. 이때 ";"이후는 무시하고 돌려준다.
; rtosfw : rtos fixed width
; strcopy : string copy
; djdg_drawsec : draw section, x,y,z값으로 이루어진 점정보를 이용해서 단면을 그린다,
;		 횡단그릴 때 유용함.
; djdg_defelp : define el pnt block, el pnt block을 정의한다.
; djdg_insertelp : insert el pnt (사용자에게 점을 입력받아 elpnt블럭을 인서트한다.
;	     만일 elpnt block이 정의 되어있지 않다면 정의한 후 인서트한다.
;	     주어진 값으로 attribute 값을 기록한다.


;---------------------
; program : gel
;           get elevation
;           어떤 점을 입력 받아 Elevation을 구해준다.
;           Yi Suk Jong
;           05/03/20
; 05/08/08 : attribute를 이용하여 사용자가 원하는 점에 선형정보 저장하도록 수정
;--------------------
(defun c:gel(  /
 		plent aligninfo basepnt basestation unit unitf splited pref num objpl bpdst vtcinfo leftinfo rightinfo
		ellst outlst thispnt tponpl tpdst tptransdst dx tpstation elonline deltay elalign leftslop rightslop
		ang ang90 pnt90 pnt90onpl dstpnt90onpl isleft slop leveldst tptransdst1 dytrans eltp outstring ellst outlst
                numstr elstr stastr	   )
  (vl-load-com)
  (setq plent (car (entsel "\n선형 Polyline을 선택하시오: ")))  ;선형line선택.
;  (setq eldatafn (getfiled  "Select VTC Data" "" "eld" 0) )  ;data file명 입력받음.
  (setq aligninfo (djdg_readvtcdata))				;선형 data file 읽어옴.
  (setq basepnt (getpoint "\nStation을 알고 있는 점을 찍으시오: ")) ;기준점입력받음.
  (setq basestation (getreal "\n기준점의 Station(단위=m): "))		;기준station
  
  ;-- 알고자 하는 점 일력받기
;  (setq pntlst nil)
;  (while (setq gpnt (getpoint "\n선형정보를 알고자하는 점을 찍으시오: ")) ;알고자하는 점을 연속해서 입력받음.
;    (setq pntlst (append pntlst (list gpnt)))   ;point list에 점 추가..
;  );while
  
;  (setq npnt (length pntlst)) 			;알고 싶은 점의 갯수.
  ;-- 단위계 입력받기
  (setq unit (getstring "\n도면의 단위계를 입력하시오<Meter(M),MiliMeter(MM)>: "))  ;도면단위입력받음.
  (if (= (strcase unit) "MM")
    (setq unitf 0.001)
    (setq unitf 1.0)                       ;unitfactor
    );if  ;대문자로 변환.
    
  ;-- 시작번호 입력받기
  (if (not startstr) (setq startstr "N1")) 
  (setq startstr (getstringold startstr "시작점 번호"))
  (setq splited (djdg_splitprenum startstr))  ;시작점 번호의 prefix와 번호부를 분리
  (setq pref (car splited)		;prefix
	num (atoi (cadr splited)))		;숫자.(string)

  ;선형 정보 얻기
  (setq objpl (vlax-ename->vla-object plent))   ; pline의 activex object
  (setq bpdst (vlax-curve-getDistAtPoint objpl basepnt)) ;시작점의 distance from start point
  (setq vtcinfo (car aligninfo))  ;종단곡선정보.
  (setq leftinfo (nth 1 aligninfo)) ;좌측편경사정보.
  (setq rightinfo (nth 2 aligninfo)) ;우측편경사정보.

  ;사용자가 원하는 점 입력받고 그 점에 대해서 작업하기
  (setq ellst nil)			;최종 El list 비우기.
  (setq outlst nil)			;출력 list 비우기
;  (setq npnt 0)  			;알고싶은 점의 갯수
  (while (/= (setq thispnt (getpoint "\n선형정보를 알고자하는 점을 찍으시오: ")) nil)
  
    (setq tponpl (vlax-curve-getClosestPointTo objpl thispnt))	;알고자하는 점의 pline상의 점.
    (setq tpdst (vlax-curve-getDistAtPoint objpl tponpl)) ;알고자하는 점의 distance from start point.
    (setq tptransdst (* (distance tponpl thispnt) unitf))		;알고자하는 점의 선형에서부터의 횡방향 거리.
  								;cad상의 거리이므로 unit factor적용.

    (setq dx (- tpdst bpdst))    ;알고자하는 점의 station- 기준점의 station
    (if (> dx 0) (setq sgn 1) (setq sgn -1))       ;알고자하는 점의 거리가 더 멀면 부호=+.

    (setq tpstation (+ basestation (* dx unitf)))
  

    (setq elonline (djdg_interpolation vtcinfo tpstation 1)    ;직선상의 el(slop만으로 구한 elevation)
	  deltay (djdg_vtcdeltay vtcinfo tpstation)) 	  ;완화곡선 dletay
    (setq elalign (+ elonline deltay))			;선형중심에서의 elevation
  
    (setq leftslop (djdg_interpolation leftinfo tpstation 1))     ;좌측편구배구하기
    (setq rightslop (djdg_interpolation rightinfo tpstation 1))	;우측편구배구하기

    ;좌향우향 판단. 선형상에서 주어진 점까지의 각도에 90도를 더한 각도로 진행했을대 거리가 멀어지면 우향
    (setq ang (angle tponpl thispnt)                   ;angle from tponpl to tp
	  ang90 (+ ang (* 0.5 pi))
	  pnt90 (polar thispnt ang90 (* 1000 unitf))
	  pnt90onpl (vlax-curve-getclosestpointto objpl pnt90)
	  dstpnt90onpl (vlax-curve-getdistatpoint objpl pnt90onpl))
    (if (> dstpnt90onpl tpdst) (setq isleft nil) (setq isleft T))

    (if isleft
      (setq slop leftslop
	    leveldst (djdg_interpolation leftinfo tpstation 2)) ;level 구간 길이 구하기.
      (setq slop rightslop
	    leveldst (djdg_interpolation rightinfo tpstation 2))
    );if
    (if (> tptransdst leveldst) 			;횡방향 거리가 level구간 길이보다 길면
       (setq tptransdst1 (- tptransdst leveldst))	;선형중심-알고자하는 점 거리에서 level폭 빼기
       (setq tptransdst1 0)
    );if  
    (setq dytrans (* slop tptransdst1 0.01))		;편구배에 의한 dy
    (setq eltp (+ elalign dytrans)) 			;최종 elevation
    

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
    (setq ellst (append ellst (list (list (car thispnt) (cadr thispnt) eltp)))) ;최종 EL목록에 추가
    (setq outlst (append outlst (list outstring)))   ;출력 string을 list에 추가.
    
    ;--- 결과를 att block에 표시하기    
    (setq numstr (strcat pref (itoa num)))   ;point번호 string만들기
    (setq elstr (strcat "EL=" (rtos eltp 2 3)))
    (setq stastr (strcat "STA." (rtos tpstation 2 3)))
    (djdg_insertelp thispnt (list (list "Pnum" numstr)
				  (list "elv" elstr)
				  (list "sta" stastr)))
;  (setq flaglist '(("Pnum" "Point Number")
;		   ("Elv" "Elevation")
;		   ("Sta" "Station")))

    (setq num (1+ num))  				;다음 점번호 증가
  );while

  ;el산출근거 text창에 출력
  (princ "\n  STATION     VEL     Dy     EL   SIDE T-SLOP  DST   LEVEL  DST1   DY1    ELV") ;output headline출력.  
  (foreach outstr outlst
    (princ outstr)
  );foreach
  
;    (setq lenpl (vla-get-length objpl))    	;pline의 총길이.
  
;    (if (> lenpl 100000) (setq unit "MM") (setq unit "M")) 
;    (setq objdimpl (vlax-ename->vla-object dimpl))     ;dimension line의 activex object

;    (setq epdst (vlax-curve-getDistAtPoint objpl ep))
;    (princ  (strcat "\nSTATION: " (rtos tpstation 2 3)))
  
;  (djdg_drawsec ellst)   ;section 그리기.
  
 (princ)
);defun


;------------------
; function : djdg_vtcel
;            종단고 구하기.
;            Yi Suk Jong
;            05/03/20
;-------------------
(defun djdg_vtcel( vtcinfo station
		  / return )
  (setq return (+ (djdg_interpolation vtcinfo station 1) (djdg_vtcdeltay vtcinfo station )))
);defun  

;------------------
; function : djdg_interpolation
;            선형보간 하여 값을 구해준다.
;            Yi Suk Jong
;            05/03/20
;------------------
; usage: (interpolation vals sv)
;  ex) (interpolation '((0 11.5 1) (4 20.0 1) ...) 3)
; return:  선형보간 값.
; arguments:
;    vals : '((0 11.5 1) (4 20 1).....) : 쌍으로 이루어진 데이터.
;    sv : searching value : 찾고자 하는 값.
; index : 보간하고 싶은 값의 list index ex) (0 11.5 1) 로 이루어진 list를 선형보간하여 세번재 값을 구할 경우 index=2
;------------------
(defun djdg_interpolation(vals sv index
			  / n return i lastx lasty thisx thisy 
			  )
  (setq n (length vals))  ;data의 갯수.
  (setq return nil)       ;초기값을 nil로... 만일 값을 찾기 못하면 nil을 return
  (setq i 1)
  (repeat (1- n)     ;두번째 data(i=1)부터 검색.
    (setq lastx (car (nth (1- i) vals))		;지난 x값.
	  lasty (nth index (nth (1- i) vals))) 	;지난 y값  
    (setq thisx (car (nth i vals))		;이번 x값.
	  thisy (nth index (nth i vals))) 	;이번 y값 
    (if (and (>= sv lastx) (<= sv thisx))
      (setq return (+ lasty (* (/ (- thisy lasty) (- thisx lastx)) (- sv lastx)))) 
    );if  
    (setq i (1+ i))  ;다음data로
  );repeat  
  return
);defun


;---------------------
; function : djdg_vtcdeltay
;            vertical translation curve delta-y
;            종단곡선 delta-y
;            Yi Suk Jong
;            05/03/20
;---------------------
; usage: (djdg_ctcdeltay vtcinfo xval)
;  ex)  djdg_vtcdeltay '((0  10  0) (100 20 60)...) 50) 
; arguments:
;      vtcinfo : 종곡선 data (station vip_elevation curve_length)
;      xval : deltay를 구하고 싶은 station
; return:
;      deltay : delta-y값.
;      nil : error인 경우.
;      0   : 직선구간인경우.
;-----------------------------

(defun djdg_vtcdeltay(vtcinfo xval
       			/ n i thisx thisy thiscl thiscl2 lastx lasty s1 s2 dx sgn return
		      )
  (setq return 0)
  (setq n (length vtcinfo))   ;곡선장 정보의 갯수.
  (setq i 1)   		;두번째 정보부터.
  (repeat (- n 2)
    (setq thisx (car (nth i vtcinfo))		;이번 x값.
	  thisy (cadr (nth i vtcinfo))) 	;이번 y값 .
    (setq thiscl (nth 2 (nth i vtcinfo))   ;이번점의 종곡선장.
          thiscl2 (/ thiscl 2.0))		;이번종곡선장의 1/2
    (if (and (>= xval (- thisx thiscl2)) (<= xval (+ thisx thiscl2)))  ;xval이 종곡선구간에 있으면...
      (progn
      	(setq lastx (car (nth (1- i) vtcinfo))		;지난 x값.
	      lasty (cadr (nth (1- i) vtcinfo))) 	;지난 y값  .
        (setq nextx (car (nth (1+ i) vtcinfo))	;다음 x값.
	      nexty (cadr (nth (1+ i) vtcinfo)))  ;다음 y값.
        (setq s1 (/ (- thisy lasty) (- thisx lastx))  ;앞구간 slop.
	      s2 (/ (- thisy nexty) (- thisx nextx))) ;뒷구간 slop
        (if (< xval thisx)    ;vip앞뒤 판단--> dx구함.
	  (setq dx (- xval (- thisx thiscl2)))    ;vip앞구간인경우.
	  (setq dx (- (+ thisx thiscl2) xval))    ;vip뒷구간인경우.
        );if
        (if (< s2 s1)
	  (setq sgn -1)    ;뒷구간 slop이 앞구간 slop보다 작은경우
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
;            종곡선 data(*.eld)파일을 읽어옴.
;            Yi Suk Jong
;            05/03/20
;------------------------
; usage: (djdg_readvtcdata)
; return: '(vtcinfo leftslop rightslop)
;        > '(종곡선정보,좌측편구배,우측편구배)
;        vtcinfo: '((0 3.0 0) (10 1.5 5) (20 1.0 0))  '((station vip-elevation curve-length)...)
;        leftslop: '((0 2.0) (15 3.0) (40 -2.0))      '((station slop)...)
;        ritghtslop: '((0 -2.0) (20 -3.0) (50 2.0))   '((station slop)...)
;-------------------------
(defun djdg_readvtcdata(
			/ vtcinfo fn readd n ch citem leftslop rightslop 
			)
  (setq vtcinfo nil leftslop nil rightslop nil)       ;list초기화
  
  (setq fn (getfiled "Select VTC Data" (getvar "dwgprefix") "eld" 0) )  ;file열기.
  (if fn
    (progn
      (setq readd (djdg_readdatafile fn))
      (setq n (length readd))
      (foreach ch readd
        (setq ch (strcase (car (sp-trunc ch)))) 		;앞뒤 공백 없애고 대문자로...
        (cond
	  ((= ch "VTC") (setq citem "VTC"))
	  ((= ch "LEFT") (setq citem "LEFT"))
	  ((= ch "RIGHT") (setq citem "RIGHT"))
	  ( T (cond
	       ((= citem "VTC") (setq vtcinfo (append vtcinfo (list (mapcar 'atof (divide_str ch ","))))))  ;현재 vtc인 경우
	       ((= citem "LEFT") (setq leftslop (append leftslop (list (mapcar 'atof (divide_str ch ","))))))  ;현재 left인 우
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
;            data file을 읽어준다. 이때 ";"이후는 무시하고 돌려준다.
;            Yi Suk Jong
;            05/03/20
;------------------------
; usage : (djdg_readdatafile fn)
; arguments: fn : file name
; return: data파일 내용  (";"이후는 무시된 결과를 list로 묶어서 돌려준다.
;------------------------
(defun djdg_readdatafile( fn
			 / return opf ch divstr
			 )
  (setq return nil)
  (setq opf (open fn "r"))   ; open file
  (if opf
    (progn
      (while (setq ch (read-line opf))
	(setq divstr (divide_str ch ";"))   ; ";"로 분리.
	(setq ch (car divstr)) 			;앞 string만취함.
	(if (> (strlen ch) 0)				;string 길이가 0이 아닌경우만 실행.
	  (progn
	    (setq return (append return (list ch)))     ;string 추가
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
; 주어진 숫자를 일정폭을 가진 string으로 변환하여 리턴해준다.
; fortran에서 f10.3과 같은 역할을 한다.
; 출력시 열을 맞출 때 필요
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
; 주어진 문자열을 주어진 횟수만큼 더한다. 자릿수를 맞출 때 유용
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
;            draw section, x,y,z값으로 이루어진 점정보를 이용해서 단면을 그린다
;            횡단그릴 때 유용함.
;            Yi Suk Jong
;            05/03/20
;------------------------
; argument : ((1 2 3) (2 3 4)) : x,y,z로 이루어진 점정보들...
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
    (setq thisel (nth 2 (nth i plst)))   ;이번점의 elevation
    (setq thispnt (list (+ (car lastpnt) dx) (+ (cadr lastpnt) dy)))
    (setq pntlst (append pntlst (list thispnt)))   ;point list에 이번점 추가하기.
    (setq lastpnt thispnt)
    (command thispnt)
    (setq i (1+ i))   ;다음 point로
  );repeat
  (command "")  ;pline명령 기
  (setq i 0)
  (repeat (length pntlst)
    (setq thisel (nth 2 (nth i plst)))
    (setq thispnt (nth i pntlst))
    (setq eltxt (rtos thisel 2 3))
    (command "text" thispnt (* (getvar "dimtxt") (getvar "dimscale")) "0" eltxt "")
    (setq i (1+ i))  ;다음 point로
  );repeat  
);defun


;---------------------------------------
; function : djdg_defelp
;            define el pnt block 
;            el pnt block을 정의한다.
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
  
  (setq natt (length flaglist))  ;att 갯수
  
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

