; Program : CTXT : 원본 text와 목적 text를 선택하여 내용을 복사하기
; Program : meq  : 여러개의 text를 순서에 맞춰서 동일하게 만든다. (Multi Equilize)
; function : meq-dialog : meq명령의 dialog 입력

;****************************************
;*    ctxt
;*              Copye TeXT
;*              By Suk-Jong Yi
;*              99/2/9
;****************************************

(defun C:ctxt(
              / num srctxt ss1 entl ass1 co entl1 cnum)     ;지역변수정의

  (defun SETERR(s)                          ;내장에러루틴 정의
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)         ;내장에러루틴 가동

  (setq srctxt (entget (car (entsel "\nPick Source Text: ")))) ;원본text선택
  (if (= (cdr (assoc 0 srctxt)) "TEXT")
    (progn
      (setq srctxt (cdr (assoc 1 srctxt)))                     ;원본text내용
      (princ "\nSelct destination text: ")
      (setq ss1 (ssget))                                    ;대상text선택
      (setq num (sslength ss1))                             ;text갯수

      (princ num)
      (princ " Text Found")                             ;발견된 text갯수 표시

      (setq index 0)                                    ;초기화
      (setq cnum 0)

      (repeat num                                       ;text 갯수만큼 반복
        (setq entl (entget (ssname ss1 index)))         ;대상엔티티정보확보
        (if (= (cdr (assoc 0 entl)) "TEXT")             ;text인경우에만
          (progn
            (setq ass1 (assoc 1 entl))                ;대상text assoc데이타 확보
            (setq co (cons (car ass1) srctxt))        ;원본text assoc데이타 작성
            (setq entl1 (subst co ass1 entl))         ;새로운 entity정보작성
            (entmod entl1)                            ;새로운 text로 업데이트
            (setq cnum (1+ cnum))                     ;고쳐진 text갯수
          );progn

          (princ "\nNon-Text entity was filtered")    ;text아닌 경우
        );if
        (setq index (1+ index))                   ;다음 text로
      ) ;of repeat
    );progn

    (princ "\Text not found")

  );if

   (terpri)
   (princ cnum)
   (princ " Text Modified")

  (setq *error* oer seterr nil)

   (princ)

) ;of defun



;****************************************
;* Program :    meq
;*              Multi Equalize Text
;               여러개의 text를 순서에 맞춰서 동일하게 만든다. 
;*              By Suk-Jong Yi
;*              06/08/01
;****************************************
; 소스 text 그룹 선택
; 소스 text 그룹의 기준점 찍기
; 타겟 text 그룹 선택
; 타겟 text 그룹의 기준점 찍기
; 두 그룹 숫자 체크, 숫자 다르면 프로그램 끝내기
; 소스그룹, 타켓그룹 기준점에서부터의 거리 구하기
; 소스, 타켓 그룹 거리기준으로 소팅
; 순서대로 내용 복사 또는 만들기 시작,완료
;  복사인 경우 : 소스그룹에서 타겟그룹으로 복사진행
;  만들기인 경우
;      - 타켓거리들을 이용하여 point list만들기시 ali_Pdim를 부를 때 사용
;      - 삽입점, 각도, sidepoint 입력받음. 
;      - ali_Pdim함수를 불러서 만들기
;         :textout, prefix, postfix, factor, delta, gol, realdist 옵션 함수내에서 처리됨
;----------------------------------------
(defun c:meq( / sss spt  i slst en eni ipnt tb tb1 tb2 mpnt
	        dst sslst sst tpt tlst stlst index srctxt entl ass1 modtxt co entl1
		ipnt ang p1 p2 dirang sidepnt plst dstsum txt txt0 dsti dstsum factor 
	     	delta prefix postfix oridist gapfactor)


  (meq-dialog)
  
;  (setq #MEQ_create T		;만들기/고치여부
;	#MEQ_modify nil		;고치기
;	#MEQ_textout T	;text출력/dimension출력
;	#MEQ_dimension T		;수평치수선
;	#MEQ_prefix "("		;prefix
;	#MEQ_postfix ")"	;postfix
;	#MEQ_factor 1.0		;factor
;	#MEQ_delta -70		;delta
;	#MEQ_gol nil		;골뱅이 적용여부 똑같은 값이 있으면 @로 묶을 것인가?
;	#MEQ_oridist T		;원본길이대로치수선을 만들 것인가?
;	#MEQ_chval T		;원본값에 수정을 가할 것인지 여부.
;  );setq
  
  ;----- 소스 text 그룹 선택
  (princ "\n원본 Text 또는 Dimension 들을 선택하세요: ")
  (setq sss (ssget '((-4 . "<OR") (0 . "TEXT") (0 . "DIMENSION") (-4 . "OR>"))))
  (setq spt (getpoint "\n원본 기준점을 찍으세요: "))

  
  ;----- 소스그룹 기준점에서부터의 거리 구하기
  (setq i 0
	slst nil)  ; 거리 ename 묶음 '((거리 ename) (거리 ename))
  
  (repeat (sslength sss)
    (setq en (ssname sss i))	;entity name
    (setq eni (entget en))
    (setq ety (cdr (assoc 0 eni))) 		;entity type
    (if (= ety "TEXT")
      (progn						;text일때.
	(setq mpnt (textboxm en))		;중심좌표구하기
        (setq dst (distance mpnt spt))		;기준점에서부터의 거리
      );progn
      (progn
	(setq mpnt (cdr (assoc 11 eni)))	;dimension text의 중심점
	(setq dst (distance mpnt spt))
      );progn	
    );if  
    (setq slst (append slst (list (list dst en))))
    (setq i (1+ i))
  );repeat  

  
  ;----- 소스 그룹 거리기준으로 소팅
  (setq sslst (vl-sort slst			;sort된 원본 list
		       '(lambda (s1 s2)
    				(< (car  s1) (car s2)))))  ;첫번째 요소를 기준으로 sort

  ; 순서대로 내용 복사 시작,완료
  ; 복사옵션이 ON인 경우
  (if #MEQ_modify	
    (progn
  	;----- 본사본 text 그룹 선택
  	(princ "\n복사본 text 들을 선택하세요: ")
  	(setq sst (ssget '((-4 . "<OR") (0 . "TEXT") (0 . "DIMENSION") (-4 . "OR>"))))
  	(setq tpt (getpoint "\n복사본 기준점을 찍으세요: "))

  	;----- 두 그룹 숫자 체크, 숫자 다르면 프로그램 끝내기
  	(if (/= (sslength sss) (sslength sst)) (progn (alert "텍스트 갯수가 다릅니다!") (exit)))

      ;----- 타켓그룹 기준점에서부터의 거리 구하기

      (setq i 0
	tlst nil)  ; 거리 ename 묶음 '((거리 ename) (거리 ename))

      (repeat (sslength sst)
        (setq en (ssname sst i))	;entity name
        (setq eni (entget en))
        (setq ety (cdr (assoc 0 eni))) 		;entity type
        (if (= ety "TEXT")
          (progn						;text일때.
	    (setq mpnt (textboxm en))		;중심좌표구하기
            (setq dst (distance mpnt spt))		;기준점에서부터의 거리
          );progn
          (progn
	    (setq mpnt (cdr (assoc 11 eni)))	;dimension text의 중심점
	    (setq dst (distance mpnt spt))
          );progn	
        );if
	(setq tlst (append tlst (list (list dst en))))
        (setq i (1+ i))
      );repeat

      ;----- 타켓 그룹 거리기준으로 소팅
      (setq stlst (vl-sort tlst			;sort된 타겟 list
		       '(lambda (s1 s2)
    				(< (car  s1) (car s2)))))  ;첫번째 요소를 기준으로 sort

      ;----- 복사시작
      (setq index 0)
      (repeat (sslength sst)
	(setq enti (entget (cadr (nth index sslst))))		;원본엔티티 정보
	(setq enty (cdr enti))					;entity type
        (setq srctxt (cdr (assoc 1 enti))) ;원본 text내용 찾기
	(if (and (= enty "DIMENSION") (= srctxt ""))		;default 치수일때
          (setq srctxt (rto_dimtxt (cdr (assoc 42 enti))))
	);if
        (setq entl (entget (cadr (nth  index stlst))))         ;대상엔티티정보확보
        (setq ass1 (assoc 1 entl))                	;대상text assoc데이타 확보
	(if #MEQ_chval
	  (setq modtxt (meq_txtopt srctxt #MEQ_prefix #MEQ_postfix #MEQ_factor #MEQ_delta));option적용
	  (setq modtxt srctxt)
	);if  
        (setq co (cons 1 modtxt))        		;수정된 원본text assoc데이타 작성
        (setq entl1 (subst co ass1 entl))         ;새로운 entity정보작성
        (entmod entl1)                            ;새로운 text로 업데이트
        (setq index (1+ index))
      );repeat
    );progn
  );if #MEQ_modify
  
  ;치수선을 만들기가 켜져있을 경우
  ; 삽입점, 방향, side점 입력받기 --> 간격list만들기 -->  point리스트 만들기 
  ; --> dimensin/text 쓰기(f_datext적용, option은 f_datext에서 적용됨)
  (if (or (and #MEQ_create #MEQ_textout) (and #MEQ_create #MEQ_dimension))
    (progn
      (setq ipnt (getpoint "\nPick Insert Point: "))  ;삽입점입력
      (initget "2P")
      (setq ang (getangle ipnt "\nSpecify Direction angle or [2Point] : "))
      (cond
        ((= ang "2P")				;2p를 선택했을 때 두점 입력받아 각도 결정
          (setq p1 (getpoint "\nSpecify first point: ")
	        p2 (getpoint p1 "\nSpecify second point: "))
          (setq dirang (angle p1 p2)) 		;진행방향 각도 계산
        );subcond
        ; line을 이용한 refrence는 line의 각도가 0,180도 두가지가 나오기 때문에 UCS변환후
        ; 입력을 받아야함. 따라서 사용자가 cancel를 눌렀을 때 WCS로 복귀하는 error처리를
        ; 해야하므로 이번작업에서 제외함.
;      ((= ang "Object")
;        (setq rent (entget (car (entsel "\nSelect Reference Object(Only Line): "))))
;        (setq p10 (cdr (assoc 10 rent))   ;시작점;
;	      p11 (cdr (assoc 11 rent)))  ;끝점
;        (setq enang (angle (cdr (assoc 10 ;line의 각도
;      );subcond
        ((/= ang "2P")
          (setq dirang ang)
        );subcond
      );cond
    
      (setq sidepnt (getpoint "\nPick side point: "))  ;side point입력받음.
    
      ;------ 간격list만들기 및 pointlist만들기
      (setq plst nil
  	  plst (append plst (list ipnt))  ;첫점은 0,0,0 으로
  	  dstsum 0)				;누적거리를 0으로
      (foreach dst sslst   ;dst '(거리 ename)
	(setq enti (entget (cadr dst)))			;entity 정보
	(setq enty (cdr (assoc 0 enti)))		;entity type
        (setq txt (cdr (assoc 1 enti)))  		;치수 추출
	(if (and (= enty "DIMENSION") (= txt ""))       ;치수가 demension이며 default("")일경우
          (setq txt (rto_dimtxt (cdr (assoc 42 enti))))
	);if  
        (if (and (= "(" (substr txt 1 1)) (= ")" (substr txt (strlen txt) 1))) 
          (setq txt0 (substr txt 2 (- (strlen txt) 2)))  ;( )가 있는 경우 ( ) 제거
	  (setq txt0 txt)				;( )가 없는 경우 기존 text그대로...
        );if
        (setq dsti (djdg_dimtxtoreal txt0))		;text에 해당하는 거리.
        (setq dstsum (+ dstsum dsti))			;누적거리 구하기
        (setq plst (append plst			;누적거리를 plis에 추가.
			   (list (polar ipnt dirang dstsum))))
      );foreach
      ;(ali_Pdim plst defpnt dirpnt sidepnt textout factor delta prefix postfix oridist gol
      (if #MEQ_chval (setq factor #MEQ_factor
			    delta #MEQ_delta
			    prefix #MEQ_prefix
			    postfix #MEQ_postfix
			    oridist #MEQ_oridist)
		     (setq factor 1.0
			    delta 0.0
			    prefix ""
			    postfix ""
			    oridist T)
	);if
      (ali_Pdim plst ipnt (polar ipnt dirang 100) sidepnt
		#MEQ_textout factor delta prefix postfix oridist #MEQ_gol)
    );progn  
  );if #MEQ_create

  ;---- text list를 만들때...
  (if (and #MEQ_create #MEQ_txtlist)
    (progn
      (setq gapfactor  1.25)					;줄간격 factor
      (setq th (getvar "dimtxt"))				;문자높이
      (setq ipnt (getpoint "\nPick insert point: "))		;삽입점 일벽받음
      (setq index 0)
      (repeat (length sslst)
	(setq enti (entget (cadr (nth index sslst))))		;원본엔티티 정보
	(setq enty (cdr enti))					;entity type
        (setq srctxt (cdr (assoc 1 enti))) ;원본 text내용 찾기
	(if (and (= enty "DIMENSION") (= srctxt ""))		;default 치수일때
          (setq srctxt (rto_dimtxt (cdr (assoc 42 enti))))
	);if
;        (setq entl (entget (cadr (nth  index stlst))))         ;대상엔티티정보확보
;        (setq ass1 (assoc 1 entl))                	;대상text assoc데이타 확보
	(if #MEQ_chval
	  (setq modtxt (meq_txtopt srctxt #MEQ_prefix #MEQ_postfix #MEQ_factor #MEQ_delta));option적용
	  (setq modtxt srctxt)
	);if
;        (setq co (cons 1 modtxt))        		;수정된 원본text assoc데이타 작성
;        (setq entl1 (subst co ass1 entl))         ;새로운 entity정보작성
;        (entmod entl1)                            ;새로운 text로 업데이트
	(command "text" (list (car ipnt) (- (cadr ipnt) (* index th gapfactor)))
		 (* (getvar "dimscale") th) "0"
		 modtxt)
        (setq index (1+ index))
      );repeat
    );progn
  );if
;  (princ "OK")
);defun meq


;---------------------
; function: meq_txtopt
;           원본텍스트에 meq의 옵션을 적용하여 돌려준다.
;---------------------
(defun meq_txtopt( txt prefix postfix factor delta / )
;  (setq 
;	prefix "("		;prefix
;	postfix ")"	;postfix
;	factor 1.0		;factor
;	delta -70		;delta
;  );
  (if (and (= "(" (substr txt 1 1)) (= ")" (substr txt (strlen txt) 1)))
    (setq txt0 (substr txt 2 (- (strlen txt) 2)))  ;( )가 있는 경우 ( ) 제거
    (setq txt0 txt)					;없는 경우 원래대로...
  );if
  (setq dst (djdg_dimtxtoreal txt0))	;실제길이 구함
  (setq fdst (* factor dst))		;factor적용
  (setq dfdst (+ delta fdst))		;delta적용
  (setq txt1 (rto_dimtxt dfdst))	;최종길이를  text로
  (setq ftxt (strcat prefix txt1 postfix))
)  ;defun

;--------------------------
; function : meq-dialog
;           06/08/05
;--------------------------
(defun meq-dialog( / dcl_id lay )  
  
  (setq dcl_id (load_dialog "DJDG"))                 ;ddscl.dcl load  
  (if (not (new_dialog "MEQ" dcl_id)) (exit))         ;ddscl.dcl 안의 MEQ
  
  ; 초기화
  ;; 
  (if (and (= #MEQ_create nil) (= #MEQ_modify nil))
    (progn (setq #MEQ_modify T) (set_tile "create" "0") (set_tile "modify" "1"))
    (if (= #MEQ_create T) (progn (set_tile "create" "1") (mode_tile "createbox" 0))
 		          (progn (set_tile "modify" "1") (mode_tile "createbox" 1)))
  );if
  (if (and (= #MEQ_textout nil) (= #MEQ_dimension nil) (= #MEQ_txtlist nil))
    (progn (setq #MEQ_textout T) (set_tile "text" "1") (set_tile "dimension" "0"))
    (if (= #MEQ_textout T)
      (set_tile "text" "1")
      (if (= #MEQ_dimension T)
	(set_tile "dimension" "1")
	(set_tile "txtlist" "1")
      );if
    );if  
  );if  
  (if #MEQ_chval (progn (set_tile "chval" "1") (mode_tile "chvalbox" 0))
    		 (progn (set_tile "chval" "0") (mode_tile "chvalbox" 1)))
  (if (= #MEQ_prefix nil) (set_tile "pref" "") (set_tile "pref" #MEQ_prefix))
  (if (= #MEQ_postfix nil) (set_tile "postf" "") (set_tile "postf" #MEQ_postfix))
  (if (= #MEQ_factor nil) (set_tile "factor" "1.0") (set_tile "factor" (rtos #MEQ_factor 2 3)))
  (if (= #MEQ_delta nil) (set_tile "delta" "0.0") (set_tile "delta" (rtos #MEQ_delta 2 3)))
  (if (= #MEQ_oridist nil)  (set_tile "oridist" "0") (set_tile "oridist" "1"))
  (if (= #MEQ_gol nil) (set_tile "gol" "0") (set_tile "gol" "1"))
  
  
  (action_tile "create" "(set_val $key)")               
  (action_tile "modify" "(set_val $key)")           
  (action_tile "text" "(set_val $key)")           
  (action_tile "dimension" "(set_val $key)")
  (action_tile "txtlist" "(set_val $key)")
  (action_tile "chval" "(set_val $key)")
  (action_tile "prefix" "(set_val $key)")
  (action_tile "postfix" "(set_val $key)")
  (action_tile "factor" "(set_val $key)")
  (action_tile "delta" "(set_val $key)")
  (action_tile "gol" "(set_val $key)")
  (action_tile "oridist" "(set_val $key)")
  
  (action_tile "accept" "(do-dialog)")                
  (action_tile "cancel" "(do-cancel)")                
  
;  (mode_tile "th" 2)  
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
     ((= key "create") (if (= 1 val) (setq #MEQ_create T #MEQ_modify nil))
      			(if #MEQ_create (mode_tile "createbox" 0) (mode_tile "createbox" 1)));subcond
     ((= key "modify") (if (= 1 val) (setq #MEQ_modify T #MEQ_create nil))
      			(mode_tile "createbox" 1))
     ((= key "text") (if (= 1 val) (setq #MEQ_textout T #MEQ_dimension nil #MEQ_txtlist nil)))
     ((= key "dimension") (if (= 1 val) (setq #MEQ_dimension T #MEQ_textout nil #MEQ_txtlist nil)))
     ((= key "txtlist") (if (= 1 val) (setq #MEQ_txtlist T #MEQ_textout nil #MEQ_dimension nil)))
     ((= key "chval") (setq #MEQ_chval (= 1 val))
      		      (if #MEQ_chval (mode_tile "chvalbox" 0) (mode_tile "chvalbox" 1)))	
     ((= key "pref") (setq #MEQ_prefix (get_tile key)))
     ((= key "postf") (setq #MEQ_postfix (get_tile key)))
     ((= key "factor") (setq #MEQ_factor val))
     ((= key "delta") (setq #MEQ_delta val))
     ((= key "gol") (setq #MEQ_gol (= 1 val)))
     ((= key "oridist") (setq #MEQ_oridist (= 1 val)))
   );cond
);defun

(defun do-dialog()    ;모든 값 광역변수에 넣기
  (set_val "create")
  (set_val "modify")
  (set_val "text")
  (set_val "dimension")
  (set_val "txtlist")
  (set_val "chval")
  (set_val "pref")
  (set_val "postf")
  (set_val "factor")
  (set_val "delta")
  (set_val "gol")
  (set_val "oridist")
  (done_dialog) ;dialog종료
  (unload_dialog dcl_id)
);defun

(defun do-cancel()
  (unload_dialog dcl_id)
  (exit)
);  