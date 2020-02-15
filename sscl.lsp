;----------------------
; Program : sscl
;           Sapdo Scale
;           Yi Suk Jong
;           04/10/26
;   삽도출력시 크기를 입력하고 길이를찍어주면 텍스트 크기를 얼마로 해야될지 알려준다.
; text와 크기를 입혁하면 출력시 원하는 텍스트 크기가 되도록 text높이(text인 경우)와
; dimscale(dimension인 경우)을 고쳐준다.
; 05/08/07 xdata가 없는 dim에도 적용가능하도록 수정함.
;----------------------
;
(defun c:sscl(
	       /  point1 point2 hlen vlen indexstar wsize hsize bbox hbox hratio vratio
	          scale ntexth2 txt ss1 nent i
	         ent etype ass40 newass40 fnd dimtxt txth newdimscl newent)
  
  (setq prnth (getrealold prnth "\nFont size: "))

  (setq point1 (getpoint "\nPick first point: "))
  (setq point2 (getcorner point1 "\nPick Second point: "))

;  (setq dist (abs (- (car point2 ) (car point1))))	

  (setq hlen (abs (- (car point2 ) (car point1))))   ;수평길이
  (setq vlen (abs (- (cadr point2 ) (cadr point1)))) ;수직길이
  
  ;출력후 box size를 입력받음.
  (setq sizetxt (getstringold sizetxt "Enter size(B*H, Unit mm): "))  

    
  (setq indexstar (vl-string-search "*" sizetxt))  ; *위치
  (setq wsize (substr sizetxt 1 indexstar))
  (setq hsize (substr sizetxt (+ 2 indexstar) (- (strlen sizetxt) 1 indexstar)))  
  (setq bbox (atof wsize)	;폭
	hbox (atof hsize))      ;높이
  
  ; hscale과 vscale을 비교해서 최종 scale을 정함
  (setq hratio (/ bbox hlen)
	vratio (/ hbox vlen))

  (if (<= hratio vratio)
    (setq scale (/ 1 hratio))		;출력시크기/도면크기 의 값이 작은 scale값을 최종 scale값으로..
    (setq scale (/ 1 vratio))
  );of if


  ;새로운 text크기 구함.

  (setq ntexth2 (* scale prnth))  ;새로운 text크기 scale * 2.0

  (setq txt (strcat "Print scale= 1:" (rtos scale 2 3) ",Printed Text Height"
		    (rtos prnth 2 1) ": Drawing Height" (rtos ntexth2 2 3) ))
  (print txt)
  (princ "\n")
  
  (setq ss1 (ssget '((-4 .   "<OR")   ;text, mtext, dimension 선택
		     ( 0 .  "TEXT")
		     ( 0 . "MTEXT")
		     ( 0 . "DIMENSION")
		     (-4 .   "OR>"))))  
  
  (setq nent (sslength ss1))   ;엔티티 갯수
  (setq i 0)
  (repeat nent    ;엔티티 갯수만틈 반복
    (setq ent (entget (ssname ss1 i)))
    (setq etype (cdr (assoc 0 ent)))  ;entity type축출
    (cond
       ((or (= etype "TEXT") (= etype "MTEXT"))
	  (princ "TEXT")
           (setq ass40 (assoc 40 ent))
;           (setq newTH (* Hscale (cdr ass1)))
           (setq newass40 (cons 40 ntexth2))
           (setq nent (subst newass40 ass40 ent))
           (entmod nent)                       ;새로운 크기로 업데이트
       );sub cond
       ((= etype "DIMENSION")

	  (setq ent (entget (ssname ss1 i) '("ACAD")))   ;xdata포함 entity 추출
	  (if (> (length (cdr (cadr (assoc -3 ent)))) 0) (setq hasxd T) (setq hasxd nil)) ;x-data가 있는지 확인
	  (if hasxd
	    (progn			;-- x-data가를 가지고 있는 경우
          	(setq fnd (djdg_findxdata ent 1070 40))        ;findxdata 1070 40 : dimscale
	  	(if (= fnd nil) (setq dimscl 1) (setq dimscl (cdr (nth 2 fnd))))  ;dimscale 값이 nil이면 1.0으로
	  	(setq dimtxt (cdr (nth 2 (djdg_findxdata ent 1070 140))))         ;dimtxt값 추출
          	(setq txth (* dimscl dimtxt))  ;text높이 = dimscale x dimtxt
	  	(setq newdimscl (/ (* scale prnth) dimtxt))   ; newdimscale = print_text_height x scale / dimtxt
	  	(setq newent (djdg_chxdata ent 1070 40 newdimscl))  ;새로운 dimscale값이 적용된 entity정보 받음
	  	(entmod newent)
	  	(princ "DIM")
	    );progn
	    (progn			;-- x-data가 없는 경우
                ;dimstyle table에서 dimtxt값 찾기
                (setq dimtxt (cdr (assoc 140 (tblsearch "dimstyle" (cdr (assoc 3 ent))))))
	        (setq newdimscale (/ (* scale prnth) dimtxt))
	    	(setq newent (djdg_chxdata ent 1070 40 newdimscale))  ;dimscale정보 추가하기
	    	(entmod newent)

	    );progn
	  );if  
       );sub cond

    );cond  
    (setq i (1+ i))
  );  
);defun

;Command: (assoc -3 (entget (car (entsel)) '("ACAD")))
;Select object: (-3 ("ACAD" (1000 . "DSTYLE") (1002 . "{") (1070 . 40) (1040 . 1.6)
;(1070 . 41) (1040 . 0.8) (1070 . 42) (1040 . 10.0) (1070 . 43) (1040 . 10.0)
;(1070 . 44) (1040 . 2.0) (1070 . 45) (1040 . 0.0005) (1070 . 73) (1070 . 0)
;(1070 . 74) (1070 . 0) (1070 . 77) (1070 . 1) (1070 . 140) (1040 . 2.5) 
;(1070 . 144) (1040 . 0.001) (1070 . 146) (1040 . 0.072) (1070 . 147) (1040 . 1.25)
;(1070 . 172) (1070 . 1) (1070 . 173) (1070 . 1) (1070 . 174) (1070 . 1) 
;(1070 . 176) (1070 . 1) (1070 . 177) (1070 . 1) (1070 . 178) (1070 . 7) (1070 . 179)
;(1070 . 3) (1070 . 271) (1070 . 3) (1070 . 272) (1070 . 3) (1070 . 279) 
;(1070 . 2) (1070 . 340) (1005 . "1D") (1070 . 341) (1005 . "1C") (1070 . 343) 
;(1005 . "1C") (1070 . 344) (1005 . "1C") (1002 . "}")))

; -------------------------
; fundtion : djdg_chxdata
;            change xdata
;            Yi Suk Jong
;--------------------------
; Argument
;   ent : xdata를 포함한 entity정보
;   idx1 : index 바꾸고자하는 index
;   idx2 : index 바꾸고자하는 index
;   v : 찾는 item의 다음 item의 새로 값
;   만일 찾는 item이 없으면 (idx1 idx2) (1040 v) 를 (1002 . "{")뒤에 추가한다.
; Return
;   entity data
(defun djdg_chxdata(ent idx1 idx2 v / xd finallst l return foundflag i idf idf1 newxd oldxd ent)
   (setq xd (cdr (cadr (assoc -3 ent))))
   (setq finallst nil
         l (length xd)  ;xdata갯수
         return nil 
	 foundflag nil  ;찾기 깃발 nil로 설정
	 i 0 )  ;첫 item부터
   (setq idf (car (djdg_findxdata ent idx1 idx2)))   ;찾은 item의 nth
   (if (= idf nil)    
     (progn    ;xdata에 찾는 data (1070 . 40)이 없을때.
       ; x-data가 없는 경우확인 x-data가 없는 경우에는 빈 xdata추가 후 진행
       ; 추가내용은 (-3 ("ACAD" (1000 . "DSTYLE") (1002 . "{")  (1002 . "}")))
       (if (= xd nil)
	 (progn
	     (setq ent (append ent '((-3 ("ACAD" (1000 . "DSTYLE") (1002 . "{")  (1002 . "}"))))))  ;빈 x-data추가
	     (setq xd (cdr (cadr (assoc -3 ent))))  ;x-data축출
	     (setq l (length xd))   ;x-data의 길이
	 );progn    
       );if
       (setq idf1 (car (djdg_findxdata ent 1002 "{")))  ;(1002 . "{")위치찾기
       (setq i 0)
       (repeat  (1+ idf1)     ;(1002 . "{")까지 추가
         (setq finallst (append finallst (list (nth i xd))))  ;frontlist에 추가
         (setq i (1+ i))  ;다음 i로
       );repeat
       (setq finallst (append finallst (list (cons idx1 idx2)) (list (cons 1040 v))))
       (setq i (+ idf1 1))                 ;찾은 item 다음 다음 것부터 더하기
       (repeat (- (1- l) idf1)
         (setq finallst (append finallst (list (nth i xd))))
         (setq i (1+ i))  ;다음 item으로
       );repeat
       
     );progn
     (progn	;xdata에 찾는 data (1070 . 40) 가 있을 때.
       (setq i 0)
       (repeat (1+ idf)
         (setq finallst (append finallst (list (nth i xd))))  ;frontlist에 추가
         (setq i (1+ i))  ;다음 i로
       );repeat

       (setq finallst (append finallst (list (cons (car (nth (1+ idf) xd)) v)))) ;찾은 item다음 item 앞값에 v추가
       (setq i (+ idf 2))                 ;찾은 item 다음 다음 것부터 더하기
       (repeat (- (1- l) idf 1)
         (setq finallst (append finallst (list (nth i xd))))
         (setq i (1+ i))  ;다음 item으로
       );repeat
     );progn
   );if
  
   (setq newxd (list -3 (append '("ACAD") finallst)))
   (setq oldxd (assoc -3 ent))
   (setq ent (subst newxd oldxd ent))
;  newxd
);defun


; -------------------------
; fundtion : djdg_findxdata
;            change xdata
;            Yi Suk Jong
;--------------------------
; Argument
;   ent : xdata가 포함된 entity정보
;   inx1,idx2 : index 바꾸고자하는 index1, 2 (indx1 . indx2)
; Retrun
;   (i (indx1 . indx2) (   .   )) ; 몇번째item인가와 해당 item과 그 뒤 item 넘겨줌
; 사용예:
;   (djdg_findxdata ent 1070 40)  ; xdata에서 (1070 . 40)이 있는 위치를 찾아준다.
(defun djdg_findxdata(ent idx1 idx2 / xd l return foundflag i nthv )
   (setq xd (cdr (cadr (assoc -3 ent))))
   (setq l (length xd)  ;xdata갯수
         return nil 
	 foundflag nil  ;찾기 깃발 nil로 설정
	 i 0 )  ;첫 item부터
   (while (and (<= i (1- l)) (= foundflag nil))   ;i가 전체개수보다 작고, 아직 찾지 못했을때만 반복
     (setq nthv (nth i xd))  ;i번째 item 값.
     (if (and (= (car nthv) idx1) (= (cdr nthv) idx2)) ; 첫번째 item이 idx1과 같고 두번째 item이 idx2와 같으면
       (progn
	 (setq return (list i (nth i xd) (nth (1+ i) xd))) ;돌려줄 값
         (setq foundflag T) ;찾은걸로 표시
       );progn  
     );if
     (setq i (1+ i))  ;다음 item으로
   );while
  return
);defun


;;;(defun te2()
;;;  (djdg_findxdata (entget (car (entsel)) '("acad")) 1070 42)
;;;);defun
;;;
;;;(defun te3()
;;;  (djdg_chxdata (entget (car (entsel)) '("acad")) 1070 40 10.0)
;;;);defun  