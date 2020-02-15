;-------------------------------
; function : djdg_divdim
;            divide dimension
;            Yi Suk Jong
;            04/09/02
;-------------------------------
;> argument :
;    dst   : total distance
;    pitch : pitch
;> return
; case-1 : 2@200
; case-2 : 155+9@200
; case-3 : 9@200+155
; case-4 : 5@200+155+4@200
; case-5 : 4@200+155+5@200
; 2-div case-6 : 77+9@200+78
;       case-7 : 78+9@200+77
; 3-div case-8 : 52+52+53+9@200
; 4-div case-9 : 38+38+38+39+9@200

(defun djdg_divdim( dst pitch div rediv / )
  
  (setq re (rem dst pitch))    ;remain
  (setq mok (fix (/ dst pitch)))  ;mok

  (if (< div mok)  ;강제로주어진 나누는 갯수가 실제 몫보다 작을 경우 mok을 div로 설정
    (setq mok div
	  re (- dst (* mok pitch)))
  );if
  
  (setq dimlst nil)   ;initailize dim list
  
  (cond
    ((= re 0)    ;remain = zero  나머지가 없는 경우 3@100=3000 
      (setq dimlst (append dimlst (list (strcat (rtos mok 2 0) "@" (rtos pitch 2 0)))))
    ); re = 0
    ((/= re 0)   ; remain /= zero  나머지가 있는 경우
     			; 155+8@200=1955
      (setq rmlst (djdg_divint re rediv))  ;나머지를 rediv로 나누기 --> 155/2 --> ((1 78) (1 77))
      ; 나눠진 나머지를 1개 혹은 2개의 텍스트로 만들어줌.
      ; 한개의 텍스트로 만들기.
      (setq rtxt1 (djdg_retxt1 rmlst 1))  ; 한개로 만들기 "78+77"
      (if (>= rediv 2)
        (setq rtxt2 (djdg_retxt1 rmlst 2))  ; 두개로 만들기 "78" "77"
      );if	
     

;     (if (= (length rmlst) 2)   ; 총나머지 개수구하기;
;	(setq nrem1 (car (nth 0 rmlst))
;	      nrem2 (car (nth 1 rmlst))
;	      nrem  (+ nrem1 nrem2))
;	(setq nrem1 (car (nth 0 rmlst))
;	      nrem nrem1)
;     );if
      
      (setq mktxt (strcat (rtos mok 2 0) "@" (rtos pitch 2 0)))  ;몫x간격 8@200

     ;      (print mktxt)
     ;      (print rmlst)
     ;나머지 텍스트를 한개로 처리하는 경우 즉 나머지가 앞에 오거나 뒤에 오거나 중간에 오는 경우
      (setq dimlst (append dimlst (list (strcat  rtxt1 "+" mktxt))))  ; 155+8@200=1955  ;나머지가 앞에 오는 경우
               		
      (setq dimlst (append dimlst (list (strcat mktxt "+" rtxt1))))   ; 8@200+155=1955  ;나머지가 뒤에 오는 경우
     
      (setq mokr1 (fix (/ mok 2)) ; 나머지가 중간에 오는 경우 --> 몫 갯수를 둘로 나눔   ;앞쪽 몫 갯수
	    mokr2 (- mok mokr1))                                  ;뒷쪽 몫갯수.
     								;4@200+155+4@200=1755 or 4@200+155+5@200=1955
      (setq dimlst (append dimlst (list (strcat (rtos mokr1 2 0) "@" (rtos pitch 2 0) "+"      ;앞 몫
					        rtxt1 "+"				;나머지	
						(rtos mokr2 2 0) "@" (rtos pitch 2 0)))))  ;뒷몫

      ;나머지 텍스트를 두개로 처리하는 경우
      (if (>= rediv 2)  ;나머지를 2개이상으로 나눳을 때만 실행
;       (setq re2 (/ re 2))
       (setq dimlst (append dimlst (list (strcat (nth 0 rtxt2) "+"        ;앞 나머지
						 (rtos mok 2 0) "@" (rtos pitch 2 0) "+"   ;중간 몫
      						 (nth 1 rtxt2)))))       ;뒷 나머지.
      );if	
    ); re /= 0 
  );cond
  dimlst
);defun


;--------------------
; function : djdg_divint
;            Divide integer
;            Yi Suk Jong
;            04/10/23
;--------------------------
; v : value값
; div : 나누는 수
; >> return
; ex) 100 5 --> ((5 20))
; ex) 86 3  --> ((2 29) (1 28))
(defun djdg_divint(v div / v div mk rm vlst)
  (setq mk (fix (/ v div)))  ;몫 
  (setq rm (fix (rem v div)))      ;나머지
  (setq vlst nil)
  (if (> rm 0)
    (setq vlst (append vlst (list (list rm (1+ mk)))))
  );if
  (if (> (- div rm) 0)
    (setq vlst (append vlst (list (list (- div rm) mk))))
  );if
);defun


;-----------------
; function : djdg_retxt1
;            remain text1
;            나머지 list를 가지고 1개 또는 2개로 나눠주는 함수
;            나머지 list ((2 250) (2 251))    
;            1개로 만드는 경우 : ("2@250+2@251")
;            2개로 만드는 경우 : ("2@250" "2@251")
;               나머지 갯수를 나눠서 둘로 쪼갠다. 쪼개는 방법은 나머지 list를 일렬로 나열하고
;               두개또는 한개로 만든다. 이때 여러번 나오는 간격은 @로 묶어준다.
;               1렬로 나열한 후 잘라내는 것이 djdg_retxt와 다른 점이다.
;            Yi Suk Jong
;--------------------------------------
; Argument :
;    rmlst : ((2 300) (2 500))
;    div   : 1 2
; Return
;   1일 때: ("2@300+2@500")
;   2일 때: ("2@200" "2@500")
(defun djdg_retxt1(rmlst div / dlist n1 n v n1 v1 num regol return ndlist nn1 nn2 splitlst regol1 regol2 )
  (setq dlist nil)     ; distance list비우기
  (setq n1 0)
  (setq n (car (nth 0 rmlst))  ;첫번재 나머지 list
        v (cadr (nth 0 rmlst)))
  (if (= (length rmlst) 2)   ;'((2 200) (2 500))  2개인 경우
      (setq n1 (car (nth 1 rmlst))
  	    v1 (cadr (nth 1 rmlst)))
  );if
  
  ; ('(2 200) (2 500))  --> (200 200 500 500) : dlist
  (repeat n  (setq dlist (append dlist (list v))))   ; 첫번째 나머지 list
  (if (= (length rmlst) 2)                           ;두번째 나머지 list가 있으면 dlist에 추가
    (repeat n1  (setq dlist (append dlist (list v1)))) 
  );if
  
  (setq num (+ n n1))   ;전체 갯수

  ;;;; 
  ;;;; 한개 또는 두개로 잘라주는 부분.
  ;;;; 한개로 만드는 경우 checkgol만 수행 후 return함
  (cond
    ((= div 1)
        (setq regol (djdg_checkgol dlist))   ;checkgol의 결과.
	(setq return (djdg_makegoltext regol))  ;gollist를 이용하여 text화 함.
    );sub cond

    
    ;; 두개로 나눠주는 부분 
    ((= div 2)
      (setq ndist (length dlist))     ; dlist의 길이 간격의 갯수 .
      (setq nn1 (fix (/ ndist 2.0)))  ;첫번째 묶음 갯수는 전체 갯수를 반으로 나눈 갯수로.
      (setq nn2 (- ndist nn1))        ;두번째 묶음 갯수는 전체 갯수에서 첫번째 묶음 갯수 뺀값.
      (setq splitlst (djdg_splitlist dlist nn1))   ;두개로 난눔  나누는 위치는 첫번째 묶음 갯수.
      (setq regol1 (djdg_checkgol (car splitlst))) ;첫번째 묶음을 골뱅이로 만듦.
      (setq regol2 (djdg_checkgol (cadr splitlst))) ;두번째 묶음을 골뱅이로 만듦.
      (setq return (list (djdg_makegoltext regol1)
			 (djdg_makegoltext regol2))) ;첫번째와 두번째 묶음을 text화 함.
  );sub cond 
 );cond   
  return
);defun


;----------------------------
; function : djdg_makegoltext
;            골뱅이 text를 만들어준다.
;             ex) '((2 200) (2 500))  --> 2@200+2@500
;            Yi Suk Jong
;            05/02/24
;----------------------------
(defun djdg_makegoltext(gol / return n i ni v vtxt )
  (setq return "")
  (setq n (length gol))  ;전체 요소의 갯수
  (setq i 0)  ;첫 요소부터
  (repeat n   ;요소갯수만큼 반복.
    (setq ni (car (nth i gol)))  ;반복갯수.
    (setq v (cadr (nth i gol)))  ;값.
    (setq vtxt (rto_dimtxt v))   ;값을 텍스트로 바꿈. rto_dimtxt함수적용(djdgfun.lsp)
    (setq return (strcat return (if (>= i 1) "+" "")  ;2번째요소 이후엔 "+"를 추가
			 (if (>= ni 2) (strcat (rtos ni 2 0) "@") "")    ;갯수가 두개이상이면 2@  방식으로 표현 1개인 경우는 아무 표시 안함.
			 vtxt))   ;값텍스트  .
    (setq i (1+ i))   ;다음 요소로...
  );repeat
  return
);defun


(defun djdg_retxt(rmlst div)
  (setq n1 0)
  (setq n (car (nth 0 rmlst))
        v (cadr (nth 0 rmlst)))
  (if (= (length rmlst) 2)
    (setq n1 (car (nth 1 rmlst))
	  v1 (cadr (nth 1 rmlst)))
  );if
  
  (setq num (+ n n1))

  
  (cond
    ((or (= div 1) (= num 1))
      (if (= (length rmlst) 1)
	(progn  ; ((1 100))  ;한개인 경우
	    (if (= n 1)      ;n=1인경우
            (setq return (strcat (rtos v 2 0)))
	    (setq return (strcat (rtos n 2 0) "@" (rtos v 2 0)))
	  );if  
	);progn
	(progn  ; ( (1 100) (1 101)) 두개인경우
	  (setq return (strcat (if (= n 1) (rtos v 2 0) (strcat (rtos n 2 0) "@" (rtos v 2 0))) "+"
			       (if (= n1 1) (rtos v1 2 0) (strcat (rtos n1 2 0) "@" (rtos v1 2 0)))))
	  
	);progn  
    );if
  );sub cond
  ((and (= div 2) (/= num 1))
    (setq vlst nil)
    (repeat n (setq vlst (append vlst (list (rtos v 2 0)))))
    (repeat n1 (setq vlst (append vlst (list (rtos v1 2 0)))))
    (cond
      ((= (rem num 2) 0)   ;짝수일때
        (setq div1 (fix (/ num 2)))
        (setq i 0
	      return1 "")
        (repeat div1
	  (setq return1 (strcat return1 (nth i vlst) (if (< i (1- div1)) "+" "")))
	  (setq i (1+ i))
	);repeat
        (setq return return1)
      );subcond	  
    );cond	  
  );sub cond 
 );cond   
  return
);defun

;----------------------------------------
; function : djdg_checkgol
;            주어진 간격들로 골뱅이(@)를 만들어준다.
;            ex) '(100 100 200 200)  --> ((2 100) (2 200))
;            05/02/24
;            Yi Suk Jong
;----------------------------------------
; argument : dlist = '(100 100 200 200)  간격 list
(defun djdg_checkgol(dlist / dlist golist n i snum cv)
    (setq n (length dlist))   ;간격갯수 
    (setq golist nil)   ;골뱅이 list --> return value
    (setq i 1       ;두번째 값부터 체크하기 시작함
	  snum 1)   ;같은 값 갯수  처음에 첫번째 값이 있으므로 1
    (repeat n
      (setq cv (nth i dlist));현재 값. i가 dlist의 갯수를 초과한 경우에 nil을 돌려주므로 아래 if문 유효
      (if (/= cv  (nth (1- i) dlist)) ;현재 값이 이전 값과 같지않거나 마지막 값이면 golist에 넣음.
	(progn
	  (setq golist (append golist (list (list snum (nth (1- i) dlist)))));새로운 gol을 추가함. 현태는 '(snum 이전값).
	  (setq snum 1)  ;같은 값 갯수는 1로 reset
	);progn
	(setq snum (1+ snum))  ;현재 값이 앞값과 같은경우 snum을 1증가
      );if	
	
      (setq i (1+ i)) ;다음 값으로..	    
    );repeat
  golist
);  defun



;----------------------------
; function : djdg_splitlist
;            list를 주어진 위치에서 잘라서 두개의 list로 만들어줌.
;            ex) '( 1 2 3 4) 2 -->  (1 2) (3 4)  : 두개의 list로 나누어짐.
;            Yi Suk Jong
;            05/02/25
;----------------------------
(defun djdg_splitlist(lst idx / len n1 n2 i lst1 lst2)
 (setq len (length lst))   ;전체 요소 갯수.
 (setq n2 (- len idx))     ;두번째 묶음 갯수.
 (setq n1 (- len n2))      ;첫번재 묶음 갯수.
 (setq i 0)			;첫번째 요소부터
 (repeat n1			;첫번재 묶음 구성
   (setq lst1 (append lst1 (list (nth i lst))))
   (setq i (1+ i))
 );repeat
 (setq i idx)			;idx번째요소부터.
 (repeat n2			;두번째묶음 구성
   (setq lst2 (append lst2 (list (nth i lst))))
   (setq i (1+ i))
 );repeat
 (list lst1 lst2)
);defun 