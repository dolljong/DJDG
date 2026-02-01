;*******************************************
; Program : TRIMB2
;           TRIM B2
;           By Suk-Jong Yi
;           1995/7/13
;*******************************************
; 두 직선이나 호 사이의 물체를 TRIM

(defun C:TRIMB2(/
)

;(push-env)
  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

(setq ent1 (entget (car (entsel "\nSelect first entity: "))))  ;기준선 선택
(setq ent2 (entget (car (entsel "\nSelect second entity: "))))
(if (= offdst nil)
  (setq offdst_old 0.0)
  (setq offdst_old offdst)
) ;of IF
(princ "\nOffset distance<")
(princ offdst_old)
(setq offdst (getreal "\>: "))
(if (= offdst nil) (setq offdst offdst_old))

(while (setq p1 (getpoint "\nPick first point: "))         ;짤려질 엔티티 선택
  (setq p2 (getpoint p1 "\nPick second point: "))
  (setq ssent (ssget "F" (list p1 p2)))                 ;ssget "F"옵션
  (setq ssnum (sslength ssent))                         ;ssget의 엔티티 갯수
  (setq bent1 (entget (ssname ssent 0)))
  (setq bent2 (entget (ssname ssent 1)))

  (setq ent1s (cdr (assoc 10 ent1))                   ;기준선1의 시작과 끝점
        ent1e (cdr (assoc 11 ent1))
        ent2s (cdr (assoc 10 ent2))                   ;기준선2의 시작과 끝점
        ent2e (cdr (assoc 11 ent2)))

  (setq bent1s (cdr (assoc 10 bent1))               ;첫째 line break
        bent1e (cdr (assoc 11 bent1)))
  (setq crsp1s (inters ent1s ent1e bent1s bent1e))
  (setq crsp1e (inters ent2s ent2e bent1s bent1e))
  (setq ang (angle crsp1s crsp1e))
  (setq brk1s (polar crsp1s (+ pi ang) offdst))
  (setq brk1e (polar crsp1e ang offdst))

  (setq bent2s (cdr (assoc 10 bent2))               ;둘째 line break
        bent2e (cdr (assoc 11 bent2)))
  (setq crsp2s (inters ent1s ent1e bent2s bent2e))
  (setq crsp2e (inters ent2s ent2e bent2s bent2e))
  (setq ang (angle crsp2s crsp2e))
  (setq brk2s (polar crsp2s (+ pi ang) offdst))
  (setq brk2e (polar crsp2e ang offdst))

  (command "BREAK" (list (ssname ssent 0) crsp1s) "F" brk1s brk1e)
  (command "BREAK" (list (ssname ssent 1) crsp2s) "F" brk2s brk2e)
  (if (/= offdst 0.0)
    (progn
      (command "LINE" brk1s brk2s "")
      (command "LINE" brk1e brk2e "")
    ) ;of progn
  ) ;of IF
) ;of while

;(pop-env)

  (setq *error* oer seterr nil)
(princ)
) ;of defun



;--------------------------
; Program : mtrimb
;	    Multi Trimb2
;		수직보강재 위치에서 수평보강재 잘라주기
; 		06/08/11
;--------------------------
; 수직보강재, 수평보강재 선택
; 수평보강재 그룹별로 교점을 찾고
; 순서대로 그린다. (trim 명령을 내리는 것이 아님, trim이 되면 기존 entity가 변하기 때문)
; 
(defun c:mtrimb( / ssvf ssv sshf ssh index elist selist cplist dsten hen eni a10 a11 i
		clist veni v10 v11 fpnt sclist gap h1 cahplist hplist ang
		ang1 chplist p pt hs p1 p2 id id1 plist1 plist2 nv iv ihs dst dstxt )
  
  (setq #MTR_th	    2.5
     	#MTR_hlayer "0"
	#MTR_vlayer "0"
	#MTR_pref "H-stiff 13x340x"
	#MTR_wlen T
  	#MTR_gap 35
	)

  (MTR-DIALOG)				;dialog입력
  
  ;----- 기초 데이터 setting
  (setq th (getvar "dimtxt")					;text높이 dimscale미적용
        thds (* (getvar "dimscale") th)				;text높이(dimscale적용)
	tgap (* 1.5 thds))					;기준선에서 text까지 gap 

  ;----수직보강재 입력받기
  (princ "\n수직보강재를 선택하세요: ")
  (if (= #MTR_vlayer "ALL")				;ALL이면 layer관계없이 모두 선택
    (setq ssvf '((0 . "LINE")))	;line엔티티만 filter
    (setq ssvf (list (cons 0 "LINE") (cons 8 #MTR_vlayer)))
  );if  
  (setq ssv (ssget ssvf))	;수직보강재 선택
  
  ;---- 수평보강재 입력받기
  (princ "\n수평보강재를 선택하세요: ")
  (if (= #MTR_hlayer "ALL")  				;All이면 layer관계없이 모두 선택
    (setq sshf '((0 . "LINE")))	;line엔티티만 filter
    (setq sshf (list (cons 0 "LINE") (cons 8 #MTR_hlayer)))
  );if  
  (setq ssh (ssget sshf))	;수평보강재 선택
  
  (setq index 0)
  (setq elist nil)		;수평보강재 entity list
  (repeat (sslength ssh)
    (setq elist (append elist (list (ssname ssh index))))
    (setq index (1+ index))
  );repeat


  ;수평보강재 선 entity sort
  (setq selist (entity-sort elist 10 nil)); entity sort

  (setq cplist nil);교차점 list)
  (foreach dsten selist
    (setq hen (cadr dsten))	;수평보강재 entity name
    (setq eni (entget hen))
    (setq a10 (cdr (assoc 10 eni))
	  a11 (cdr (assoc 11 eni)))
    (setq i 0)
    (setq clist nil)		;수직보강재 교차점 list
    (setq clist (append clist (list a10))	;시작점과 끝점 추가
	  clist (append clist (list a11)))
    (repeat (sslength ssv)
      (setq veni (entget (ssname  ssv i)))
      (setq v10 (cdr (assoc 10 veni))
	    v11 (cdr (assoc 11 veni)))
      (setq clist (append clist (list (inters a10 a11 v10 v11)))) ;교차점list
      (setq i (1+ i))			;다음 수직보강재선으로.
    );repeat
    (setq fpnt (farest clist));
    (setq sclist (point-sort clist 3 nil))		;sort된 point list
	  
    (setq cplist (append cplist (list sclist)))	;수평보강재별 list
    (setq i (1+ i))
  );foreach

  ; 잘라진 plist만들기

  
  (setq h1 0)
  (setq cahplist nil)		;잘려진 수평보강재별 list, 잘려진 수평보강재 point list의 모음
  (foreach hplist cplist
    (setq ang (angle (nth 0 hplist) (nth 1 hplist)))	;진행방향 각도
    (setq ang1 (+ ang pi))				;반대방향 각도
    (setq chplist nil)		;잘려진 수평보강재 point list
    (setq chplist (append (list (nth 0 hplist))))  ;첫 point 추가
    (setq index 1)
    (repeat (- (length hplist) 2)
      (setq p (nth index hplist))
      (if (/= (rem index 2) 0) 		;1,3,5,7,9일때.
        (setq pt (polar p ang1 #MTR_gap))	;반대방향으로 빼기
        (setq pt (polar p ang #MTR_gap))	;2,4,6,8,10 일때 같은 방향으로빼기
      );if
      (setq chplist (append chplist (list pt)))
      (setq index (1+ index))
    );repeat
    (setq chplist (append chplist (list (nth (1- (length hplist)) hplist)))) 	;마지막 점 추가
    (setq cahplist (append cahplist (list chplist)))				;전체 list에 추가.
  );foreach

  ;----- 수평 line 그리기.
  (setq ihs 0)
  (foreach hs cahplist	;각 수평보강재 선별로 그리기
    (setq index 0)
    (repeat (fix (/ (length hs) 2))	;전체 point 갯수 / 2 번 반복수행
      (setq p1 (nth (* 2 index) hs)
	    p2 (nth (+ (* 2 index) 1) hs))

      (push-os)
        (command "line" p1 p2 "")
      (pop-os)
      (if (and #MTR_wlen (= (rem ihs 2) 0))	;길이쓰기 옵션이 ON이고 0,2,3,5번째 h-line인경우
	(progn
          (setq dst (distance p1 p2)) ;보강재 길이.
          (setq dstxt (rto_dimtxt dst))
          (setq dstxt (strcat #MTR_pref dstxt))	;prefix삽입
          (djdg_wtxtonline p1 p2 dstxt thds tgap)
	);progn
      );if	
      (setq index (1+ index))      
    );repeat
    (setq ihs (1+ ihs))				;다음 h-line으로
  );foreach  

  ;------ 수직line 그리기...
  (setq index 0)
  (repeat (fix (/ (length cahplist) 2))  ;수평보강재 선갯수 /2 만큼 반복
    (setq id (* 2 index))		;line index 0,2,4,6
    (setq id1 (1+ id))			;line index 1,,3,5,7
    (setq plist1 (nth id cahplist)
	  plist2 (nth id1 cahplist))
    (setq nv (- (length plist1) 2))
    (setq iv 1)
    (repeat nv				;잘라진 곳만큼 반복
      (setq p1 (nth iv plist1)
	    p2 (nth iv plist2))
      (push-os)
        (command "LINE" p1 p2 "")
      (pop-os)
      (setq iv (1+ iv))
    );repeat  
    (setq index (1+ index))		;다음 line으로
  );repeat
  
  (setq i 0)
  (repeat (sslength ssh)
    (entdel (ssname ssh i))
    (setq i (1+ i))
  );repeat  
);defun



(defun MTR-DIALOG( /  
                  dcl_id lay )  

  (defun mtrset_val(key / val )
    (setq val (atof (get_tile key)))
     (cond
       ((= key "mtrth") (setq #MTR_th val))
       ((= key "mtrpref") (setq #MTR_Pref (get_tile key)))
       ((= key "mtrgap") (setq #MTR_gap val))
       ((= key "mtrwlen") (setq #MTR_wlen val))
       ((= key "mtrvlaylst") (setq #MTR_vlayer (nth (fix val)  #laylst)));sub cond
       ((= key "mtrhlaylst") (setq #MTR_hlayer (nth (fix val)  #laylst)));sub cond        
     );cond
  );defun

  (defun mtrdo-dialog()    ;모든 값 광역변수에 넣기
    (mtrset_val "mtrth")
    (mtrset_val "mtrpref")
    (mtrset_val "mtrgap")
    (mtrset_val "mtrwlen")
    (mtrset_val "mtrvlaylst")
    (mtrset_val "mtrhlaylst")
    (done_dialog) ;dialog종료
    (unload_dialog dcl_id)
  );defun

  (defun mtrdo-cancel( )
    (unload_dialog dcl_id)
    (exit)
  );defun

  ;--------------------------
  ;--- MTR-Dialog main ----
  ;--------------------------
  
  (setq dcl_id (load_dialog "DJDG"))                 ;ddscl.dcl load  
  (if (not (new_dialog "MTRIMB" dcl_id)) (exit))         ;ddscl.dcl 안의 WCPL
  
  ; 초기화
  ;; list다이얼로그에 layer list 뿌려주기
  (setq #laylst nil)
  (setq #laylst (append #laylst (list "ALL")))
  (setq lay (tblnext "layer" T))
  (setq #laylst (append #laylst (list (cdr (assoc 2 lay)))))
  (while (setq lay (tblnext "layer"))
    (setq #laylst (append #laylst (list (cdr (assoc 2 lay)))))
  );while
  
  ; 수직보강재 layer선택 list에 추가  
  (start_list "mtrvlaylst")
  (mapcar 'add_list #laylst)
  (end_list)
  ; 수평보강재 layer선택 list에 추가  
  (start_list "mtrhlaylst")
  (mapcar 'add_list #laylst)
  (end_list)

  (if (= nil #MTR_th) (set_tile "mtrth" (rtos (getvar "dimtxt") 2 1))
    		     (set_tile "mtrth" (rtos #MTR_TH 2 1))) ;text높이 초기화
  (if (= nil #MTR_Pref) (set_tile "mtrpref" "")
    		     (set_tile "mtrpref" #MTR_Pref)) ;Prefix 초기화
  
  (if (= #MTR_vlayer nil) (set_tile "mtrvlaylst" "0")
      (set_tile "mtrvlaylst" (itoa (vl-position #MTR_vlayer #laylst)))); layer list초기화
  (if (= #MTR_hlayer nil) (set_tile "mtrhlaylst" "0")
      (set_tile "mtrhlaylst" (itoa (vl-position #MTR_hlayer #laylst)))); layer list초기화
    
  (if (= nil #MTR_gap) (set_tile "mtrgap" "35")
    		     (set_tile "mtrgap" (rtos #MTR_gap 2 3))) ;gap 초기화  
  (if (= nil #MTR_wlen) (set_tile "mtrwlen" "0")
    		     (set_tile "mtrwlen" (if #MTR_wlen "1" "0"))) ;길이쓰기 초기화  
  
  (action_tile "mtrth" "(mtrset_val $key)")               
  (action_tile "mtrpref" "(mtrset_val $key)")           
  (action_tile "mtrgap" "(mtrset_val $key)")           

  (action_tile "wc2laylst" "(mtrset_val $key)") ;layer setting
  (action_tile "wc2laylst" "(mtrset_val $key)") ;layer setting

  (action_tile "mtrwlen" "(mtrset_val $key)")
  
  (action_tile "accept" "(mtrdo-dialog)")                
  (action_tile "cancel" "(mtrdo-cancel)")                
  
  (mode_tile "mtrgap" 2)  
  (start_dialog)  
  (unload_dialog dcl_id)  
  (princ)
  
) ;of defun TRIMB2-dialog



;--------------------------
; function : entity를 특성 dxf코드(좌표), 특성 기준점으로부터의 거리를 기준으로 sort
; 		06/08/11
;--------------------------
; enlist : entity name list
; ass : assoc기준 코드..(ex: 시점이면 10)
; ipt : 기준점	
;--------------------------
(defun entity-sort(enlist ass ipt
		   / ne plist en farpnt ipt pnt dstenlist dst sdstenlist)
  (setq ne (length enlist))
  (setq plist nil)
  (foreach en enlist
    (setq plist (append plist (list (cdr (assoc ass (entget en))))))
  );foreach
  (setq farpnt (farest plist))		;가장 먼 점 찾기

  (if (= ipt nil)			;만일 기준점이 주어지지 않으면.
    (setq pnt (car farpnt))		;가장먼 점 두개중 앞에것을 기준점으로...
    (setq pnt ipt)			
  );  
  (setq dstenlist nil)
  (foreach en enlist
    (setq dst (distance pnt (cdr (assoc ass (entget en)))))
    (setq dstenlist (append dstenlist (list (list dst en))))
  );foreach

  (setq sdstenlist (vl-sort dstenlist '(lambda (s1 s2)	;기준점에서부터 거리로 sort
    				(< (car  s1) (car s2)))))
  
);

;--------------------------
; function : point list를 특정 성분(x,y,z,거리)을 기준으로 sort
; 		06/08/11
;--------------------------
; ptlist : point list
; idx : assoc기준 코드..(ex: 시점이면 0:x, 1:y, 2:z 3:거리)
; ipt : 기준점(idx가 3일때만 사용된다) nil이면 가장먼점중의 한점으로부터 거리로 sort
;--------------------------
(defun point-sort(plist idx ipt
		   / return farpnt ipt pnt dstlist p dst dstplist sdstplist dp )
;  (setq ne (length plist))

  (cond
    ((< idx 3)
     (setq return (vl-sort plist '(lambda (s1 s2)
				    (< (nth idx  s1) (nth idx s2)))))
				    
    );subcond
    ((= idx 3)
      (setq farpnt (farest plist))		;가장 먼 점 찾기
      (if (= ipt nil)			;만일 기준점이 주어지지 않으면.
        (setq pnt (car farpnt))		;가장먼 점 두개중 앞에것을 기준점으로...
        (setq pnt ipt)			
      );  
      (setq dstplist nil)
      (foreach p plist
        (setq dst (distance pnt p))
        (setq dstplist (append dstplist (list (list dst p))))
      );foreach

      (setq sdstplist (vl-sort dstplist '(lambda (s1 s2)	;기준점에서부터 거리로 sort
    				(< (car  s1) (car s2)))))
      (setq return nil)
      (foreach dp sdstplist
	(setq return (append return (list (cadr dp))))
      );foreach	
    );subcond
    
  );cond 
);