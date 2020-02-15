; Program : SAREA;      Sum of AREA   여러개의 구역으로 이루어진 도형의 면적구하기.
; program : cala;           calulate Area 산출근거 보여주는 면적구하기


;*************************************            
;     Program : SAREA
;               Sum of AREA
;               By Suk-Jong Yi            
;               03/07/18
;*************************************
; 05/08/13 : 단위를 설정하고 기록할 수 있도록 수정
; 점을 찍어 영역들을 설정해주고 그 영역들의 면적 합을 알려줌
;*************************************            
(defun c:sarea(/
	        th unit sumar plines e  ar  narea var scaledarea scaledareastr 
              )
  (princ "\n[다정다감] SAREA : 면적구하기")
  (setq th (* (getvar "dimtxt") (getvar "dimscale")))  ;text크기는 dimension크기 * dimscale

  (if (not unit) (setq unit 1.0))
  (setq unit (getrealold unit "\n표시할 길이 스케일을 입력하세요: "))

  
  (print "Select polyline defining area to be calculated: ")
 
;(setq e (entsel))
  (setq sumar 0)
  (setq plines nil)
  (setq e (bpoly (getpoint "\nPick Point: ")))
  (while e
    (setq plines (append plines (list e)))
    (foreach var plines
      (redraw var 3)
    ); foreach  
    (command ".area" "e" e)
    (setq ar (getvar "area"))
    (setq sumar (+ sumar ar))
    (setq e (bpoly (getpoint "\Pick Point: ")))  
  ); while
;  (foreach var plines
;    (redraw var 4)
;  ); foreach
  (foreach var plines
    (entdel var)
  ); foreach    
  (setq narea (length plines))
  (princ "\n Number of Area: ")
  (princ narea)
  (princ "\n Total Area: " )
  (setq scaledarea (* sumar unit unit))    ;scale이 적용된 면적
  (setq scaledareastr (rtos scaledarea 2 3))  ;text로 변환
  (princ scaledareastr)

  (setq ip (getpoint "\n면적을 표시할 위치를 찍으세요: "))  
  (push-os)
  (command "text" "j" "m" ip th "0" (strcat "A=" scaledareastr))
  (pop-os)  
);defun


;----------------------------
; program : cala
;           calulate Area
;		Yi Suk Jong
;		06/08/12
;----------------------------
;구역을 정해주고 그 구역의 면적산출근거를 보여준다.
; 구역은 사각형이나 삼각형으로 이루어져 있어야 한다.
;----------------------------
(defun c:cala( / th ds linegapf thds linegapds icount pp e results plst result1 icountxt ip
		 i sum ipnt sum bdeq bdv maxeq maxv eqx vx ipnt1 y lpt1 lpt2 y1 sumpt sumtxt
	      )
  
  (setq th (getvar "dimtxt")
	ds (getvar "dimscale"))
  (setq linegapf 1.5)	;line gap factor
  
  (setq thds (* th ds)			;scale적용된 
  	linegapds (* thds linegapf))


  (setq icount 1)
  (setq pp (getpoint "\nPick Point(종료=Enter): "))
  (setq e (bpoly pp))        
  (if (= e nil)
    (progn
      (alert "선택한 구역은 삼각형이나 사각형이 아닙니다.")
	(exit)
    );progn
  );if  
  (setq results nil)
  (while e
    (setq plst (car (mk_vertlist e)))
    (if (= (cdr (assoc 70 (entget e))) 1) (setq plst (append plst (list (nth 0 plst))))) ;만일 닫혀있으면 처점을 마지막점으로 추가
    (setq result (plarea plst 0.001))
    (if (/= result "No rectangle")
      (progn
        (setq result (strcat (itoa icount) " : " result))
        (setq result1 (divide_str result "="))	;산출근거와 결과로 나눔
        (setq results (append results (list result1)))
       );progn
      (progn
	(alert "선택한 구역은 삼각형이나 사각형이 아닙니다.")
	(exit)
      );
    );if  
      
   
    (redraw e 3)
    (setq icountxt (itoa icount))
    (command "text" pp thds "0" icountxt )
    (setq pp (getpoint "\nPick Point(종료=Enter): "))
    (entdel e)					;바로전 pline지우기 지우지 않으면 만나는 점에서 node생김    
    (if (/= pp nil)
      (progn
        (setq e (bpoly pp))		;입력받은 점으로 polyline만들기
        (if (= e nil)			;poly line이 만들어지지 않은 경우
          (progn
            (alert "선택한 구역은 삼각형이나 사각형이 아닙니다.")
	    (exit)
          );progn
        );if  
        (setq icount (1+ icount))
      );progn
      (setq e nil)			;pp가 nil인경우 while문  종료하기 위해 e=nil대입
    );if	
  ); while
  
  (setq ip (getpoint "\n계산결과 출력위치를 찍으세요: "))

  (setq i 0)

;  (foreach plst plines
;    (setq result (plarea plst 0.001))
;    (setq result (strcat (itoa (1+ i)) " : " result))
;    (setq result1 (divide_str result "="))	;산출근거와 결과로 나눔
;    (setq results (append results (list result1)))
;    (setq i (1+ i))
;  );foreach
  
  (setq sum 0)
  (setq i 0)
  (foreach result results
    (setq ipnt (list (car ip) (- (cadr ip) (* linegapds i)) 0))
    (push-os)
    (command "text" ipnt thds "0" (car result))
    (pop-os)
    ;합계 계산
    (setq sum (+ sum (atof (cadr result))))		;합산
      
    (setq bdeq (car (nth 1 (textbox (list (cons 1 (car result))))))		;산출근거 text크기 판단
          bdv  (car (nth 1 (textbox (list (cons 1 (cadr result)))))))		;산출결과 text크기 판단
      
    (if (= i 0)
      (setq maxeq bdeq maxv bdv)  		;첫text면 최대값으로 기억
      (progn
        (if (> bdeq maxeq) (setq maxeq bdeq)) ;최대값이면 기억
	(if (> bdv maxv) (setq maxv bdv)) ;최대값이면 기억
      );progn	
    );if  
    (setq i (1+ i))
  );foreach

  (setq eqx (+ (car ip) maxeq (* thds 2)))			; =표시 x좌표
  (setq vx (+  eqx maxv (* thds 3)))
  (setq i 0)
  (foreach result results
    (setq ipnt (list eqx (- (cadr ip) (* linegapds i)) 0))   ; "=" 표시 좌표
    (setq ipnt1 (list vx (- (cadr ip) (* linegapds i)) 0))  ; 결과 표시 좌표오른정렬
    (push-os)
      (command "text" ipnt thds "0" "=")
      (command "text" "J" "R" ipnt1 thds "0" (cadr result))
    (pop-os)
    (setq i (1+ i))
  );foreach

  ;---합계 출력
  
  (setq y (- (cadr ip) (* linegapds (- i 0.5)))
        lpt1 (list (car ip) y)
	lpt2 (list (car ipnt1) y)
	y1 (- y (* linegapds 1.0))
	sumpt (list (car lpt2) y1))
  (setq sumtxt (rtos sum 2 3))
  (push-os)
    (command "line" lpt1 lpt2 "")
    (command "text" "J" "R" sumpt thds "0" sumtxt)
  (pop-os)	     
  
);defun

;-----------------------
; function : plarea
;	     poly line의 vertext로 이루어진 면적을 구해준다. 삼각형, 4각형만 지원
;		Yi Suk Jong
;		06/08/12
;-----------------------
;argument
; plist : point list
; lfac : 길이 factor
;결과 : 사각형 -> "1.000x2.000=2.000"
;	삼각형 -> "1/2x1.000x2.000=1.000"
;-----------------------

(defun plarea(plist lfac
	              / np p0 p1 p2 p3 p4 ang01 ang23 ang12 ang34 da13 da24 return
	      		l1 l2 a l1s l2s as b01 b12 b23 mini angv cp h bs hs as
	      
	      )
  (setq np (length plist))		;point 수 ;마주보는 변의 각도 구하기.
  (setq p0 (nth 0 plist)
	p1 (nth 1 plist)
	p2 (nth 2 plist)
	p3 (nth 3 plist))
  
  (if (> np 4) (setq p4 (nth 4 plist))) ;4각형인경우
  
  (setq ang01 (angle p0 p1)  
	ang23 (angle p2 p3)
	ang12 (angle p1 p2))
  (if (> np 4)  (setq ang34 (angle p3 p4))) ;4각형인 경우
  (cond
    ((= np 5)	;사각형인경우
  	(setq da13 (dang ang01 ang23)		;변1,3의 각차
	     da24 (dang ang12 ang34))	;변2,4의 각차
  	(if (or (> (abs (- (abs da13) pi)) 0.001) (> (abs (- (abs da24) pi)) 0.001))
    	  (setq return "No rectangle")
    	  (progn
      	    (setq l1 (distance p0 p1)
	          l2 (distance p1 p2))
      	    (setq a (* l1 l2))
      	    (setq l1s (* l1 lfac)
	    	  l2s (* l2 lfac)
	    	  as (* a lfac lfac))
      	    (setq return (strcat (rtos l1s 2 3) "x" (rtos l2s 2 3) "=" (rtos as 2 3)))
    	  );progn
  	);if
     );subcond
    ((= np 4)	;삼각형인경우
      (setq b01 (distance p0 p1)
	    b12 (distance p1 p2)
	    b23 (distance p2 p3))
      (setq mini (car (vl-sort-i (list b01 b12 b23) '<)))  ;제일 작은변 찾기
      (cond
	((= mini 0) (setq b1 p0 b2 p1 b3 p2 b b01))	;basepoint 및 꼭지점
	((= mini 1) (setq b1 p1 b2 p2 b3 p3 b b12))
	((= mini 2) (setq b1 p2 b2 p3 b3 p1 b b23))
      );cond	
      (setq angv (+ (angle b1 b2) (* 0.5 pi)))
      (setq cp (inters b3 (polar b3 angv 100) b1 b2 nil))
      (setq h (distance cp b3))
      (setq bs (* b lfac)
	    hs (* h lfac)
	    as (* 0.5 bs hs))
      (setq return (strcat "1/2x" (rtos bs 2 3) "x" (rtos hs 2 3) "=" (rtos as 2 3)))
    );subcond
  );cond  
  return
)  ;defun