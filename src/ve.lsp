;---------------------
; program : VE graph
; 	07/06/21
;---------------------
(defun c:ve( /
	    alts fn dlist nline dllist i dllist il head a temp items wf alt valt
	    alts j jl nalt ipnt vegap cpnt ialt altname lcc vs tpnt )

  (setq alts nil) 	;alts list비우기

  (setq fn (getfiled "Open VE data file" "" "ve" 1))      ;file이름 입력받음
  (setq dlist (djdg_readdatafile fn))  ;data file읽어서 저장
  (setq nline (length dlist))  ;data 갯수 ";" 줄 빼고..

  (setq dllist nil)		; 공백으로 분리한 list (("A=1.200" "B=4.500") ("C=3.4" "D=4.5"))
  
  (setq i 0)
  (repeat nline			; 모두 list로 변경
    (setq dllist (append dllist (list (djdg_splitstr (nth i dlist) " "))))  ;공백으로 분리 -> ("A=1.200" "B=4.500")
    (setq i (1+ i))	;다음 line
  );repeat

  (setq i 0)
  (repeat nline
    (setq il (nth i dllist))  ;i번째 라인 list
    (setq head (car (djdg_splitstr (car il) "=")))      ;첫번째 atom
    (cond
      ((= (strcase head) "ITEMS")
        (setq a (apply 'strcat il))
        (setq temp (cadr (djdg_splitstr a "=")))
        (setq items (djdg_splitstr temp ","))
      );sub cond
      ((= (strcase head) "WF")
        (setq a (apply 'strcat il))
        (setq temp (cadr (djdg_splitstr a "=")))
        (setq wf (djdg_splitstr temp ","))
        (setq wf (mapcar 'atof wf))
      );sub cond
      ((= (strcase head) "ALT")
        (setq a (apply 'strcat il))
        (setq temp (cadr (djdg_splitstr a "=")))
        (setq alt (djdg_splitstr temp ","))
        (setq valt (mapcar '(lambda (a) (if (is-num a) (atof a) a)) alt))
        (setq alts (append alts (list valt)))  ;alts list에 alt추가
       
      );sub cond
      (T
        (setq j 0)
        (repeat (length il)
	  (setq jl (djdg_splitstr (nth j il) "="))
	  (set (read (car jl)) (if (is-num (cadr jl)) (atof (cadr jl)) (cadr jl)))
	  (setq j (1+ j))
	);repeat
      );subcond 
    );cond
    
    (setq i (1+ i))		;다음 i로
  );repeat


  (setq nalt (length alts))   ;alt 갯수
  
  (setq ipnt (getpoint "\nPick insert point: "))
  (command "text" "j" "c" ipnt vth 0 title)  ;title쓰기
  (setq vegap (+ (* cr 2) (* 6 th)))		;각 그래프 사이 간격
  (setq cpnt (polar ipnt (* -0.5 pi) vegap))
  (setq i 0)
  (repeat nalt
    (setq ialt (nth i alts))			;i번재 alt정보
    (setq altname (car ialt))			;alt name
    (setq lcc (car (reverse ialt)))		;마지막 데이터
    (setq vs (reverse (cdr (reverse (cdr ialt)))))	;앞뒤 빼고 나머지가 점수
    ; alt name쓰기
    (setq tpnt (polar cpnt (* 0.5 pi) (+ cr (* 4 th)))) ;alt name text 좌표
    (command "text" "j" "c" tpnt vth 0 altname)          ;alt name 쓰기
    
    ;그래프 그리기	  
    (draw_ve_graph cpnt cr irratio th vth lth items vs wf lcc)
    
    (setq cpnt (polar cpnt 0 vegap))	        ;다음그래프 중심 좌표
    (setq i (1+ i))	     
  );repeat   
);defun


;------------------------
; function : draw_ve_graph
;        07/06/22
;------------------------
; arguments
;  ip : 삽입정(중심점)
;  cr : 원의 반지름
;  irratio: 안족 반지름의 비율
;  th ; item text높이
;  vth : 가치점수 text높이
;  lth : level text높이
; vitem : item별점수 '(1 2 3 4 5)
; wfitem: item별 weight factor
; lcc : 상대 lcc점수
;-------------------------
(defun draw_ve_graph(ip cr irratio th vth lth titem vitem wfitem lcc /
	       gap tgap ir dr crr vv sf icent crpnt i sumang anglist nitem hang
	       tang tpnt sp v ang1 ang2 vr pnt1 pnt2 iang vpent shent)

;  (setq ip '(0 0))  
;  (setq cr 100 ;circle radius
;	irratio 0.35  ;inner radius ratio
;	th 15   ;text height
;	vth 20  ;text height of 점수
;	nitem 6
;	titem '("계획성" "환경성" "시공성" "안전성" "유지관리" "경제성")
;	vitem '(10 10 8 6 8 6)
;	wfitem '(0.4 0.2 0.1 0.1 0.1 0.1)
;	LCC 1.0
;  );setq

  

  (setq gap 5)   ;line 갭
  (setq tgap 5) ;text갭
  (setq ir (* irratio cr)) ;inner radius
  
  (setq dr (/ (- cr ir) 5))  	;delta r
  (setq crr cr)

  ;------ 점수 계산 및 쓰기
  (setq vv (/ (* 10 (apply '+ (mapcar '* vitem wfitem))) LCC))
  (setvar "dimzin" 0)
  (command "text" "j" "m" ip vth "0" (rtos vv 2 1) "")

  ;------ 원그리기
  (setvar "cecolor" "1")
  
  (setq sf 1)  		;scale factor
  (command "circle" ip cr)
  
  
  (repeat 5
    (setq crr (- crr dr)) ;current radius)    
    (command "circle" ip crr)
  );repeat
  (setq icent (entlast))  ;안쪽 원(hatch시 사용)

  ;------ level 쓰기
  (setq crpnt (polar ip (* 0.5 pi) (- ir (* 0.5 dr))))
  (setq i 0)
  (repeat 5
    (setq crpnt (polar  crpnt (* 0.5 pi) dr)) ;current radius
    (command "text" "j" "ml" crpnt lth 0 (itoa (+ i 6)))
    (setq i (1+ i))
  );repeat


  ;------ 선그리기
  (setq i 0)
  (setq sumang (* 0.5 pi))
  (setq anglist (list sumang))
  (repeat nitem
    (setq sumang (- sumang (* 2 pi (nth i wfitem))))
    (setq anglist (append anglist (list sumang)))
    (command "line" (polar ip sumang (- ir gap))
	            (polar ip sumang (+ cr gap)) "")
    (setq i (1+ i))
  );repeat

  (setvar "cecolor" "7")
  
  ;------- item title 쓰기
  (setq i 1)
  (repeat nitem
    (setq hang (* 0.5 (+ (nth (1- i) anglist) (nth i anglist))))  ;전각과의 중간각
    ;text쓰기
    (setq tang (- hang (* 0.5 pi)))
    (setq tpnt (polar ip hang (+ cr tgap)))
    (command "text" "j" "c" tpnt  th (rtod tang)  (nth (1- i) titem))
    (setq i (1+ i))
  );repeat
  

  ;--------- 점수선 그리기
  (setq i 0)
  (setq sp (polar ip (* pi 0.5) (- cr (* (- 10 (nth 0 vitem)) dr))))
  (command "Pline" sp "a")
  (repeat nitem
    (setq v (nth i vitem)) ;item의 값
    (setq ang1 (nth i  anglist)		;이전 각
	  ang2 (nth (1+ i) anglist))  ;이번 각
    (setq vr (- cr (* dr (- 10 v))))
    (setq pnt1 (polar ip ang1 vr)
	  pnt2 (polar ip ang2 vr))
    
    (setq iang (* (nth i wfitem) 2 pi -1)) ;item angle
    (command "CE" ip "a" (rtod iang))
    (if (= i (1- nitem))
      (command "L" "cl")		;close
      (command "L" (polar ip ang2 (- cr (* (- 10 (nth i vitem)) dr))) "a")
    );  
    (setq i (1+ i))	
  );repeat  
    (command "")
  (setq vpent (entlast)) 	;값 외곽선 entity
  
  ;----- hatch하기
  (command "hatch" "p" "s" icent vpent "" "")
  (setq shent (entlast))	;hatch entity

  ;----- hatch색 바꾸기
  (command "change" shent "" "P" "C" "254" "")

  ;----- hatch 뒤로 보내기
  (command "draworder" shent "" "B")

  
  
);defun  