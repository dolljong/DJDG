; Program: WLEN : 라인 길이 써주기

; Program: WCPL : 교차점 길이 써주기(호, Pline과 직선이 만나는 점의 길이를 써준다ex)수평보강재길이 )

; Program: WCPL2 : 두 곡선과 한직선이 만나는 점의 길이를 적어준다. ex) 크로스빔 길이 등..

;***********************************
; Program : WLEN
;           Write line LENGTH
;           By Suk-Jong Yi
;           95/5/10
;***********************************

(defun C:WLEN(/
              th        ent     dst     txt     pnt
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)

  (setq th (getvar "textsize"))
  (princ "\nPick text/Text height<")
  (princ th)
  (initget "Pick")
  (setq txth (getdist ">: "))
  (cond
      ((= txth nil) (setq txth th))
      ((= txth "Pick")
          (progn
              (setq txth (cdr (assoc 40 (entget (car (entsel "\nPick text"))))))
              (princ "\nText height is ")
              (princ txth)
          ) ;of progn
      ) ;of cond txth="Pick"
  ) ;of cond

  (if sf
    (progn
      (princ "\nScale factor <")(princ sf)
      (setq tmp (getreal ">: "))
      (if (/= tmp nil)
	(setq sf  tmp)
      );end if
    );then
    (setq sf (getreal "\nScale factor: "))
  );endif  

  
  (setq ang (getangold oldang "Text angle [Align]" "Align"))
  (setq oldang ang)

;;;  (if ang
;;;    (progn
;;;      (princ "\nText angle <")(princ ang)
;;;      (setq tmp (getangle ">: "))
;;;      (if (/= tmp nil)
;;;	(setq ang (rtod tmp))
;;;      );end if
;;;    );then
;;;    (setq ang (rtod (getangle "\nText angle :")))
;;;  );endif  
    
  
;  (setq sf (getreal "\nScale factor: "))
  (princ "\nSelect Line: ")
  (while (setq selent (entsel))
    (setq ent (entget (car selent)))
    (setq sp (cdr (assoc 10 ent))
	  ep (cdr (assoc 11 ent)))
    (setq dst (distance sp ep))
    (if (= ang "Align")
      (setq tang (rtod (ang4text sp ep)))
      (if (= ang nil) (setq tang 0) (setq tang (rtod ang)))
    );if  
    (setq txt (strcat "L=" (rtos (* dst sf) 2 (getvar "LUPREC"))))
    (setq pnt (getpoint "\nPick insert point"))
    (command "TEXT" "J" "M" pnt txth tang txt)
  ) ;of while

  (pop-env)

  (setq *error* oer seterr nil)
  (princ)

) ;of defun



;------------------------
; Program: WCPL : 교차점 길이 써주기(호, Pline과 직선이 만나는 점의 길이를 써준다ex)수평보강재길이 )
; 	Yi Suk Jong
;	06/07/31
;------------------------
; 호,pline입력받기
; 기준선들입력받기(기준 layer만 처리)
; 기준선과 pline의 교차점 찾기
; 교차점들의 pline상의 시점에서부터의 거리구하기
; 교차점들 기준선의 시작점에서부터의 거리를 기준으로 sorting하기
; 교차점별로 길이 적기(기준길이 이하 거리는 skip)
;------------------------
; 광역변수(옵션값)
; #WC_TH : text Height
; #WC_Pref
; #WC_Layer
; #WC_Minusl
; #WC_Mind
; #WC_TextA
;-------------------
(defun c:WCPL( / pl ssfilter ss ety nss i llst len p1 p2 plst lp1 lp2
	         crspt objpl dstlst p dst sdstlst sp ep dstse text )
  ;--- 초기화
  ;
  
  (setq ;WC_TH 75
	;WC_Pref "12X320X"
	;WC_Layer "center"
	;WC_Minusl 50
	;WC_Mind 70
	;WC_TextA 1
	gapf 1
	)
  
  (WCPL-Dialog) 	;dialog로 입력받기
  
  ;--- 호, pline입력 받기
  (setq pl (car (entsel "\nSelect Pline or Arc: ")))  ;pl entity name
  (if (= #WC_layer "All")
    (setq ssfilter (list (cons 0 "LINE")))
    (setq ssfilter (list (cons 0 "LINE") (cons 8 #WC_Layer)))
  );end  
;  (setq ss (ssget ssfilter))
;  (setq ss nil)
  (while (=  nil (setq ss (ssget ssfilter)))
    (princ "\nLINE을 선택해주세요")
   ; (setq ss (ssget ssfilter))
  );

  ;--- 기준선과 pline의 교차점 찾기(기준 layer선들만 처리)
  (setq ety (cdr (assoc 0 (entget pl)))) ;entity type
  (setq nss (sslength ss))  ;line의 갯수
  (setq i 0)
  
  ;-- line list만들기
  (setq llst nil)
  (repeat nss
    (setq len (entget (ssname ss i)))
    (setq p1 (cdr (assoc 10 len))
	   p2 (cdr (assoc 11 len)))
    (setq llst (append llst (list (list p1 p2))));line list
    (setq i (1+ i))
  );repeat
  
  (setq plst nil)
  (if (= ety "LWPOLYLINE")  		;pline인 경우
    (setq vlst (car (mk_vertlist pl)))  ;vertex list
  );
  (if (= ety "LINE")			;line인 경우
    (setq eline (entget pl)
	  lpt1 (cdr (assoc 10 eline))
	  lpt2 (cdr (assoc 11 eline)))
  ) ;if
  
  (setq i 0)
  (repeat nss
    (setq lp1 (nth 0 (nth i llst))
          lp2 (nth 1 (nth i llst)))
    (setq ang (angle lp1 lp2)				;extend 
	  dist (distance lp1 lp2)
          lp2ex (polar lp2 ang (* dist 0.5))   		;선길이의 반을 extend
	  lp1ex (polar lp1 (+ ang pi) (* dist 0.5))) 
    (cond
      ((= ety "LWPOLYLINE")
        (setq crspt (car (cp_line_pline lp1ex lp2ex vlst)))
      );subcond 
      ((= ety "ARC")
;        (setq crspt (cross (entget pl) lp1ex lp2ex))
       (setq crspt (car (crossent pl (ssname ss i) 2)))
      );subcond
      ((= ety "LINE")
        (setq crspt (inters lp1ex lp2ex lpt1 lpt2))
      ) 
    );cond
    (if (= crspt nil)
       (progn (alert "No Cross Point!") (exit))
       (setq plst (append  plst (list crspt)))
    );  
    (setq i (1+ i))
  );repeat	
    
  ; 교차점들의 pline상의 시점에서부터의 거리구하기
  (setq objpl (vlax-ename->vla-object pl))   ; pline의 activex object
  (setq i 0)
  (setq dstlst nil)
  (repeat (length plst)
    (setq p (nth i plst))
    (if (= ety "LINE")
      (setq dst (distance p lpt1))			;LIne인 경우 getdistatpoint거 nil나오는 경우가 많으므로 직접구함
      (setq dst (vlax-curve-getDistAtPoint objpl p)) ;시작점의 distance from start point
    );if	   
    
    (setq dstlst (append dstlst (list (list dst p))))
    (setq i (1+ i))
  );repeat

  ; 교차점들 기준선의 시작점에서부터의 거리를 기준으로 sorting하기  
  (setq sdstlst (vl-sort	dstlst
    				'(lambda (s1 s2)
    				(< (car  s1) (car s2)))))

  ; 교차점별로 길이 적기(기준길이 이하 거리는 skip)
  (setq i 1)
  
  (repeat (1- (length sdstlst))
    (setq sp (cadr (nth (1- i) sdstlst))	;시작점 좌표
	  ep (cadr (nth i      sdstlst)))	;끝점 좌표
    (setq dstse (abs (- (car (nth (1- i) sdstlst)) (car (nth i sdstlst))))) ;두점의 거리
    (if (> dstse #WC_Mind)			;최소거리보가 긴경우만 실행
      (progn
	(setq dstse (- dstse #WC_Minusl))	; 이격거리 빼기
        (setq text (rto_dimtxt dstse))		; 두점간의 거리를 text로
        (setq text (strcat #WC_pref text))	; prefix더하기
	(if (= #WC_TextA 1)			;각도 적용/비적용
          (djdg_wtxtonline sp ep text (* (getvar "dimscale") #WC_TH) (* gapf #wc_th)) ;두점위에 text쓰기
	  (progn
	    (push-os)
	    (command "TEXT" "J" "M" (mid-point sp ep) (* (getvar "dimscale") #WC_TH) "0" text)
	    (pop-os)
	  );pron
	);if  
      );progn	
    );if ;(princ dstse)
    (setq i (1+ i))
  );repeat
  

); defun WCPL


(defun WCPL-DIALOG( /  
                  dcl_id lay )  
  
  (setq dcl_id (load_dialog "DJDG"))                 ;ddscl.dcl load  
  (if (not (new_dialog "WCPL" dcl_id)) (exit))         ;ddscl.dcl 안의 WCPL
  
  ; 초기화
  ;; list다이얼로그에 layer list 뿌려주기
  (setq #laylst nil)
  (setq #laylst (append #laylst (list "All")))
  (setq lay (tblnext "layer" T))
  (setq #laylst (append #laylst (list (cdr (assoc 2 lay)))))
  (while (setq lay (tblnext "layer"))
    (setq #laylst (append #laylst (list (cdr (assoc 2 lay)))))
  );while
  
  
  (start_list "laylst")
  (mapcar 'add_list #laylst)
  (end_list)

  (if (= nil #WC_TH) (set_tile "th" (rtos (getvar "dimtxt") 2 1))
    		     (set_tile "th" (rtos #WC_TH 2 1))) ;text높이 초기화
  (if (= nil #WC_Pref) (set_tile "pref" "")
    		     (set_tile "pref" #WC_Pref)) ;Prefix 초기화
  
  (if (= #WC_layer nil) (set_tile "laylst" "0")
      (set_tile "laylst" (itoa (vl-position #WC_layer #laylst)))); layer list초기화
    
  (if (= nil #WC_Minusl) (set_tile "minusl" "0")
    		     (set_tile "minusl" (rtos #WC_Minusl 2 3))) ;Minusl 초기화  
  (if (= nil #WC_Mind) (set_tile "mind" "0")
    		     (set_tile "mind" (rtos #WC_Mind 2 3))) ;Prefix 초기화  
  (if (= nil #WC_TextA) (set_tile "ta" "0")
    		     (set_tile "ta" (rtos #WC_TextA 2 0))) ;Prefix 초기화  
  
  (action_tile "th" "(set_val $key)")               
  (action_tile "pref" "(set_val $key)")           
  (action_tile "minusl" "(set_val $key)")           
  (action_tile "mind" "(set_val $key)")
  (action_tile "laylst" "(set_val $key)") ;layer setting
  (action_tile "ta" "(set_val $key)")                     
  (action_tile "accept" "(do-dialog)")                
  (action_tile "cancel" "(do-cancel)")                
  
  (mode_tile "th" 2)  
  (start_dialog)  
  (unload_dialog dcl_id)  
  (princ)  
) ;of defun WCPL-dialog

(defun set_val(key / val )
  (setq val (atof (get_tile key)))
;  (if (<= val 0.0)
;    (progn
;      (set_tile "error" "Invalid Input")
;      (mode_tile key 2)
;      (mode_tile key 3)
;    );progn
;    (progn
   (cond
     ((= key "th") (setq #WC_TH val))
     ((= key "pref") (setq #WC_Pref (get_tile key)))
     ((= key "minusl") (setq #WC_Minusl val))
     ((= key "mind") (setq #WC_Mind val))
     ((= key "ta") (setq #WC_TextA val))
     ((= key "laylst")
      (progn
	(setq #WC_layer (nth (fix val)  #laylst)) 
      );progn
     );sub cond 
   );cond
;      (set_tile "error" "")
;    );progn
;  );if
);defun

(defun do-dialog()    ;모든 값 광역변수에 넣기
  (set_val "th")
  (set_val "pref")
  (set_val "minusl")
  (set_val "mind")
  (set_val "ta")
  (set_val "laylst")
  (done_dialog) ;dialog종료
  (unload_dialog dcl_id)
);defun

(defun do-cancel( )
  (unload_dialog dcl_id)
  (exit)
);defun




;------------------------
; Program: WCPL2 : 두 곡선과 교차하는 직선의 길이 써주기ex)크로스빔 길이)
; 	Yi Suk Jong
;	06/08/8
;------------------------
; 호,pline들 입력받기
; 기준선들입력받기(기준 layer만 처리)
; 기준선과 pline의 교차점들 찾기(곡선이 여러개인 경우 여러점 발생)
; 교차점들의 line상의 시점에서부터의 거리구하기(2개일경우 필요없음)
; 교차점들 기준선의 시작점에서부터의 거리를 기준으로 sorting하기(2개일경우 필요없음)
; 교차점별로 길이 적기(기준길이 이하 거리는 skip)
;------------------------
; 광역변수(옵션값) - WCPL과 혼선을 피하기 위해 "WC2"로 시작
; #WC2_TH : text Height
; #WC2_Pref :prefix
; #WC2_Layer : 기준선들 선택시 적용할 layer
; #WC2_delta : 구해진 길이에 적용할 차감길이
; #WC2_Mind : cut-off 기준길이
; #WC2_TextA : text각도의 선각도에 맞출 것인가?
;-------------------
(defun c:WCPL2( / gapf ssplf sspl ssfilter index plilst sspln ety lpt1 lpt2 eni nss i
	         llst len p1 p2 plst pl eti plp1 plp2 crspt dstlst cp sdstlst idx idst idstxt pp1 pp2 )

  
  ;--- 초기화
  ;
  
;  (setq #WC2_TH 75
;  	#WC2_Pref "12X320X"
;	#WC2_Layer "center"
;	#WC2_delta 50
;	#WC2_Mind 70
;	#WC2_TextA 1
	
;	)

  (setq gapf 1)		; line에서 text까지 gap
  
  (WCPL2-Dialog) 	;dialog로 입력받기
  
  ;--- 호, pline 두개이상 . 입력 받기
  (princ "\nPline, Arc, line을 두개이상 선택하세요: ")
  (setq ssplf '((-4 . "<OR") (0 . "LINE") (0 . "ARC") (0 . "LWPOLYLINE") (-4 . "OR>"))) ;ss pline filter
  (setq sspl (ssget ssplf))  ;pl entity name
  (while (< (sslength sspl) 2)  ;2개 이하가 선택되면 계속 반복해서 입력받음.
    (princ "\nPline, Arc, line을 두개이상 선택하세요: ")    
    (setq sspl (ssget ssplf))
  );while  
  
  ; line들 입력받기  
  (if (= #WC2_layer "All")
    (setq ssfilter (list (cons 0 "LINE")))
    (setq ssfilter (list (cons 0 "LINE") (cons 8 #WC2_Layer)))
  );end
  
  (princ "\nLINE을 선택해주세요")
  (while (=  nil (setq ss (ssget ssfilter)))
    (princ "\nLINE을 선택해주세요")
   ; (setq ss (ssget ssfilter))
  );

  ;--- 기준선과 pline의 교차점 찾기
  ;곡선들 list만들기
  (setq index 0)
  (setq plilst nil)  ;pline정보 list만들기 '(("line" (P1 p2)) ("arc" entget) ("LWPLINE" vertlist)...)
  (repeat (sslength sspl)
    (setq sspln (ssname sspl index))		;entity name
    (setq ety (cdr (assoc 0 (entget sspln)))) ;entity type
    (cond
      ((= ety "LWPOLYLINE")  		;pline인 경우
        (setq eni (car (mk_vertlist sspln)))  ;vertex list
      );subcond
      ((= ety "LINE")			;line인 경우 시작점 끝점
        (setq eline (entget sspln)
          lpt1 (cdr (assoc 10 eline))
          lpt2 (cdr (assoc 11 eline))
	  eni (list lpt1 lpt2))
      ) ;subcond
      ((= ety "ARC")			;arc인 경우 ent information
        (setq eni (entget sspln))
      ) ;subcond
    );cond
    (setq plilst (append plilst (list (list ety eni))))	;'((type 정보)..) 리스트 만들기
    (setq index (1+ index))
  );
  
  ;-- line list만들기
  (setq nss (sslength ss))  ;line의 갯수
  (setq i 0)
  (setq llst nil)         ;llst 라인들의 시종점 list '((시점 종점) (시점 종점) ...)
  (repeat nss
    (setq len (entget (ssname ss i)))
    (setq p1 (cdr (assoc 10 len))
	   p2 (cdr (assoc 11 len)))
    (setq llst (append llst (list (list p1 p2))));line list
    (setq i (1+ i))
  );repeat
  
  (foreach line llst   ;line갯수만큼 실행
    (setq p1 (car line)		;이번 line의 시점, 종점	
	  p2 (cadr line))
    (setq plst nil)    ;교차점 list
    (foreach pl plilst  ;pline list별로 별로 실행
      (setq ety (car pl)
	    eti (cadr pl));entity정보 종류별로 다르다.
      (cond
	((= ety "LINE")				;LINE인 경우
	  (setq plp1 (car eti)
		 plp2 (cadr eti))
          (setq crspt (inters p1 p2 plp1 plp2))	 
	);subcodn
	((= ety "LWPOLYLINE")			;LWPOLYLINE인 경우
          (setq crspt (car (cp_line_pline p1 p2 eti)))	 
	);subcodn
	((= ety "ARC")				;arc인 경우
          (setq crspt (cross eti p1 p2))	 
	);subcodn
      );cond
      (setq plst (append plst (list crspt)))   ;교차점 list에 추가
    );foreach sspln
    ;직선상 교차점 들 list만들기
    (setq dstlst nil)				;거리+교차점 list'((거리 교차점)...)
    (foreach cp plst
      (setq dstlst (append dstlst (list (list (distance p1 cp) cp))))
    );foreach cp plst
    ;직선상 교차점들 list sort
    (setq sdstlst (vl-sort dstlst '(lambda (s1 s2)
    				(< (car  s1) (car s2)))))

    (setq idx 1)
    (repeat (1- (length sdstlst))
      (setq idst (- (car (nth idx sdstlst)) (car (nth (1- idx) sdstlst))))
      (setq idst (+ idst #WC2_delta))		;delta적용
      (setq idstxt (rto_dimtxt idst))		;길이 text
      (setq idstxt (strcat #WC2_pref idstxt))	;prefix 삽입
      (setq pp1 (cadr (nth (1- idx) sdstlst))	;시작점
	     pp2 (cadr (nth idx sdstlst)))	;점
      (if (> idst #WC2_mind)  			;cut-off 길이보다 긴경우만 표시
        (djdg_wtxtonline pp1 pp2 idstxt  (* (getvar "dimscale") #WC2_th) (* gapf #WC2_th (getvar "dimscale")))
      );if  	
      (setq idx (1+ idx))			;다음 교차길이로...
    );repeat  
    
  );foreach llst   				;다음 line으로... 

); defun WCPL2

(defun WCPL2-DIALOG( /  
                  dcl_id lay )  

  (defun wc2set_val(key / val )
    (setq val (atof (get_tile key)))
     (cond
       ((= key "wc2th") (setq #WC2_TH val))
       ((= key "wc2pref") (setq #WC2_Pref (get_tile key)))
       ((= key "wc2delta") (setq #WC2_delta val))
       ((= key "wc2mind") (setq #WC2_Mind val))
       ((= key "wc2ta") (setq #WC2_TextA val))
       ((= key "wc2laylst")
        (progn
	  (setq #WC2_layer (nth (fix val)  #laylst)) 
        );progn
       );sub cond 
     );cond
  );defun

  (defun wc2do-dialog()    ;모든 값 광역변수에 넣기
    (wc2set_val "wc2th")
    (wc2set_val "wc2pref")
    (wc2set_val "wc2delta")
    (wc2set_val "wc2mind")
    (wc2set_val "wc2ta")
    (wc2set_val "wc2laylst")
    (done_dialog) ;dialog종료
    (unload_dialog dcl_id)
  );defun

  (defun wc2do-cancel( )
    (unload_dialog dcl_id)
    (exit)
  );defun

  ;--------------------------
  ;--- WCPL2-Dialog main ----
  ;--------------------------
  
  (setq dcl_id (load_dialog "DJDG"))                 ;ddscl.dcl load  
  (if (not (new_dialog "WCPL2" dcl_id)) (exit))         ;ddscl.dcl 안의 WCPL
  
  ; 초기화
  ;; list다이얼로그에 layer list 뿌려주기
  (setq #laylst nil)
  (setq #laylst (append #laylst (list "All")))
  (setq lay (tblnext "layer" T))
  (setq #laylst (append #laylst (list (cdr (assoc 2 lay)))))
  (while (setq lay (tblnext "layer"))
    (setq #laylst (append #laylst (list (cdr (assoc 2 lay)))))
  );while
  
  
  (start_list "wc2laylst")
  (mapcar 'add_list #laylst)
  (end_list)

  (if (= nil #WC2_TH) (set_tile "wc2th" (rtos (getvar "dimtxt") 2 1))
    		     (set_tile "wc2th" (rtos #WC2_TH 2 1))) ;text높이 초기화
  (if (= nil #WC2_Pref) (set_tile "wc2pref" "")
    		     (set_tile "wc2pref" #WC2_Pref)) ;Prefix 초기화
  
  (if (= #WC2_layer nil) (set_tile "wc2laylst" "0")
      (set_tile "wc2laylst" (itoa (vl-position #WC2_layer #laylst)))); layer list초기화
    
  (if (= nil #WC2_delta) (set_tile "delta" "0")
    		     (set_tile "wc2delta" (rtos #WC2_delta 2 3))) ;delta 초기화  
  (if (= nil #WC2_Mind) (set_tile "wc2mind" "0")
    		     (set_tile "wc2mind" (rtos #WC2_Mind 2 3))) ;Prefix 초기화  
  (if (= nil #WC2_TextA) (set_tile "wc2ta" "0")
    		     (set_tile "wc2ta" (rtos #WC2_TextA 2 0))) ;Prefix 초기화  
  
  (action_tile "wc2th" "(wc2set_val $key)")               
  (action_tile "wc2pref" "(wc2set_val $key)")           
  (action_tile "wc2delta" "(wc2set_val $key)")           
  (action_tile "wc2mind" "(wc2set_val $key)")
  (action_tile "wc2laylst" "(wc2set_val $key)") ;layer setting
  (action_tile "wc2ta" "(wc2set_val $key)")                     
  (action_tile "accept" "(wc2do-dialog)")                
  (action_tile "cancel" "(wc2do-cancel)")                
  
  (mode_tile "wc2th" 2)  
  (start_dialog)  
  (unload_dialog dcl_id)  
  (princ)
  
) ;of defun WCPL-dialog

