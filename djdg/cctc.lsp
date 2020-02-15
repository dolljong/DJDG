;-------------------------------------
; function : cctc_dia
;            dialog box를 통해서 ctc를 변경하도록 함.
;--------------------
; function : djdg_divint
;            Divide integer
;            정수를 주어진 갯수로 나누어서 요소들로 만들어준
;-------------------------------
; function : djdg_divdim
;            divide dimension
;            주어진 거리/간격/갯수/나머지 분할 을 이용하여 여러가지 경우의 치수텍스트를 돌려줌.
;            ex) 거리=1250 간격=125 갯수=8 나머지분할=2
;               -->  2@125+8@125, 8@125+2@125, 4@125+2@125+4@125, 125+8@125+125
;                  (나머지앞으로) (나머지뒤로) (나머지가운데로)   (나머지 둘로나누기)
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
;----------------------------
; function : djdg_makegoltext
;            골뱅이 text를 만들어준다.
;             ex) '((2 200) (2 500))  --> 2@200+2@500
;----------------------------------------
; function : djdg_checkgol
;            주어진 간격들로 골뱅이(@)를 만들어준다.
;            ex) '(100 100 200 200)  --> ((2 100) (2 200))
;----------------------------
; function : djdg_splitlist
;            list를 주어진 위치에서 잘라서 두개의 list로 만들어줌.
;            ex) '( 1 2 3 4) 2 -->  (1 2) (3 4)  : 두개의 list로 나누어짐.

;--------------------------------
; Program : CCTC
;           Change CTC
;           Yi Suk Jong
;           05/02/21
; Dim의 ctc를 바꿔줌
; ex) 10@125=1250  --> ctc 150으로 변경  --> 8@150+50=1250
; -------------------------------

(defun c:cctc()
  (setq desel (entsel "\nSelect Dimension: "))
  (setq de (entget (car desel)))

  (setq as1 (cdr (assoc 1 de))) ;text
  (setq as10 (cdr (assoc 10 de))) ; dimpoint

  (setq as13 (cdr (assoc 13 de))) ; def point 13
  
  (setq as14 (cdr (assoc 14 de))) ; def point 14
  
  (grdraw as13 (mapcar '+ as13 '(10 10 0)) 1 1)  ;grdraw test
  
  (setq sptxt (djdg_splitdimtxt as1))

  (setq spnum (djdg_splitdimtxtnum as1))  ; dimtxt를 잘라서 숫자로 만듦
  (if (= (nth 0 spnum) nil)		; 첫번째 값(mok)이 nil인경우 --> 길이만 있음 (nil nil 1.250)
    (setq totl (nth 2 spnum))           ; 세번째값이 total length임
    (setq totl (* (nth 0 spnum) (nth 1 spnum)))  ; 그렇지 않은 경우 mok@divl값이 total length임
  );

  (if (= (nth 1 spnum) nil)  ; 두번재 값이 nil인 경우 즉 골뱅이가 없고 치수만 있는 경우
     (setq divl totl)   ;divl을 치수로 설정.
     (setq divl (nth 1 spnum)) ;골뱅이가 있는 경우는 divl을 두번째 값으로 설정.
  );if

  (setq divlist (djdg_divlen totl divl))  ;몫과 나머지.

  (setq mok (nth 0 divlist)
	re (nth 1 divlist))

  ;초기치를 설정하고 dialogn박스를 띄움.
  (setq #dst totl)    ;총길이.
  (setq #num mok)  ;몫.
  (setq #divl divl)    ;간격값.
  (setq #re (rem #dst #divl))  ;나머지

  (cctc_dia)		;다이얼로그박스로 #dimstr(최종 dim구성) 받기.
  
  ;----- dialog box로 구해진 #dimstr대로 치수선 그리기.
  (setq spdstr (divide_str #dimstr "+"))	;+로 split된 #dimstr
  (setq idx 0)
  (repeat (length spdstr)
    (setq dstr (nth idx spdstr))	;처리한 dimstring
    (setq spgol (divide_str dstr "@"))
    (if (> (length spgol) 1)     	;@가 있는 경우.
      (setq mok (atoi (car spgol))
	    divl (djdg_dimtxtoreal (cadr spgol)))
      (setq mok 1			;@가 없는 경우.
	    divl (djdg_dimtxtoreal (car spgol)))
    );if
    
    (if (= idx 0)
      (setq stpnt as13)
      (setq stpnt ipnt)
    );if
    (setq angdl (djdg_angofdimline (car desel)))  ;angle of dim line
    (cond			;Hor, Ver, Align 의 경우 처리 note:05/12/30 Align의 경우 처리 남았음.
      ((= angdl 0)  		;horizontal인 경우
        (if (minusp (- (car as14) (car as13))) (setq mok (* mok -1)))
        (setq ipnt (f_dh stpnt divl mok as10 nil))	;첫 dim인경우 기존 치수선의 시작점에서 시작
      );sub cond
      ((= angdl (/ pi 2.0))	;vertcial인 경우
        (if (minusp (- (cadr as14) (cadr as13))) (setq mok (* mok -1)))
	(setq ipnt (f_dv stpnt divl mok as10 nil))  
      );sub cond
      (T			;Aligned인 경우
        (setq ipnt (f_da stpnt angdl divl mok as10 nil))
      );sub cond else 
    );cond  
    
    (setq idx (1+ idx))  ;다음 치수 string으로..
  );repeat
  (if (= #delold 1) 
    (command "erase" (car desel) "")		;기존치수선 지우기.
    (djdg_cdimdan (car desel) 1)		;기존치수선 1단 올리기.
  );if  

);defun


;-------------------------------------
; function : cctc_dia
;            dialog box를 통해서 ctc를 변경하도록 함.
;            Yi Suk Jong
;            05/02/25
;-------------------------------------
(defun cctc_dia()

  (defun done-dialog()
    (if (= (get_tile "delold") "1")
      (setq #delold 1)
      (setq #delold 0)
    );if
    (done_dialog)
  );defun
  
    (setq dcl_id (load_dialog "DJDG.DCL"))                  ;dialog호출
    (if (not (new_dialog "CCTC" dcl_id)) (exit))

;-------------------
; 초기값설정
;-------------------
  
  (set_tile "length" (strcat "Distance: " (rtos #dst 2 0)))
  (if (/= #num nil) (set_tile "num" (rtos #num 2 0)))
  (if (/= #divl nil) (set_tile "divl" (rtos #divl 2 0)))
  (if (/= #rnum nil) (set_tile "rnum" (rtos #rnum 2 0)) (set_tile "rnum" "1"))  ;나머지분할 초기값=1
  (if (/= #re nil) (set_tile "re" (rtos #re 2 0)) (set_tile "re" "0")) ;나머지 초기값=0
  (if (or (= #delold nil) (= #delold "1")) (set_tile "delold" "1") (set_tile "plus1dan" "1")) ;나머지 초기값=0
  
  (set_tile "ntd" (rtos (* #num #divl) 2 0))

  (checkex)
;  (if (/= #lstiff nil) (set_tile "brib" (rtos #lstiff 2 0)))  
;  (if (/= #tstiff nil) (set_tile "thick" (rtos #tstiff 2 0)))
    
;---------------------------
; dialog box 초기화
;---------------------------
;   (action_tile "num" "(setq at \"num\")(checkex)")
   (action_tile "num" "(checkex)")  
   (action_tile "divl" "(checkex)")
   (action_tile "rnum" "(checkex)")
   (action_tile "lst" "(selectdimcase)")
   (action_tile "final" "(flen)")
  
   (action_tile "brib" "(checkex)")
   (action_tile "thick" "(checkex)")
   (action_tile "check" "(checkex)") 
    
   (action_tile "accept" "(done-dialog)")

   (action_tile "cancel"  "(exit)")

    ;   (mode_tile "fl_weld" 2)

    (start_dialog)

    (unload_dialog dcl_id)
) ;of sub defun

;(defun do-accept();
;
;);defun  

(defun selectdimcase()
  (setq #lstidx (atoi (get_tile "lst")))
  (setq #dimstr (nth #lstidx #caselst))
  (setq #flen (djdg_getdimlen #dimstr))  ;총길이 구하기.
  (set_tile "final" #dimstr)    ;최종선택 text를 다이얼로그 박스에 표시
  (set_tile "flen" (strcat "최종길이: " (rtos #flen 2 0)))  ;길이를 다이얼로그박스에 표시
  
);defun

(defun flen()
  (setq #dimstr (get_tile "final"))		;총길이 string  -> 전역변수로 넘겨줌.
  (setq #flen (djdg_getdimlen #dimstr))  ;총길이 구하기.
  (set_tile "flen" (strcat "최종길이: " (rtos #flen 2 0)))  ;길이를 다이얼로그박스에 표시
);defun

(defun checkex()
  (setq #num (atof (get_tile "num")))
  (setq #divl (atof (get_tile "divl")))
  (setq #rnum (atof (get_tile "rnum")))
  
  (setq #ntd (* #num #divl))
  
  (if (> #ntd #dst) (setq #num (fix (/ #dst #divl))))  ;현재 입력된 값의 계산치가 총 길이보다 큰경우 몫을 divl기준으로 계산함

  (setq #ntd (* #num #divl))
  (setq #re (- #dst (* #divl #num)))

  (setq ren (djdg_divint #re #rnum))   ;나머지분할 결과.

  (setq #caselst (djdg_divdim #dst #divl #num #rnum)) ;나머지 분할 결과와 주치수의 결과를 합해서 case구성.

  ;; 여기까지 했음...  05/02/26  am 7:38

  ;; list다이얼로그에 case분할결과 뿌려주기
  (start_list "lst")
  (mapcar 'add_list #caselst)
  (end_list)

  (set_tile "num" (rtos #num 2 0))
  (set_tile "ntd" (rtos #ntd 2 0))
  (set_tile "re" (rtos #re 2 0))
)  ; defun



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
    (setq vlst (append vlst (list (list (fix (- div rm)) mk))))
  );if
);defun



;-------------------------------
; function : djdg_divdim
;            divide dimension
;            주어진 거리/간격/갯수/나머지 분할 을 이용하여 여러가지 경우의 치수텍스트를 돌려줌.
;            ex) 거리=1250 간격=125 갯수=8 나머지분할=2
;               -->  2@125+8@125, 8@125+2@125, 4@125+2@125+4@125, 125+8@125+125
;                  (나머지앞으로) (나머지뒤로) (나머지가운데로)   (나머지 둘로나누기)
;            Yi Suk Jong
;            04/09/02
;-------------------------------
;> argument :
;    dst   : total distance
;    pitch : pitch
;    div : 나누는 갯수 이 값이 실제 몫보다 작을 경우 이 값으로 작업함.
;    rediv : 나머지를 나누는 갯수 예) 1250 = 8@125 + 250 --> 나머지는 250임 이 나머지를 몇개로 나눌것인가?
;> return (djdg_divdim 1955 200 9 1)
; case-1 : 2@200
; case-1 : 155+9@200  나머지가 앞으로
; case-2 : 9@200+155  나머지가 뒤로
; case-3 : 5@200+155+4@200 나머지가 가운데로
; case-4 : 77+9@200+78 나머지를 둘로 나누기

(defun djdg_divdim( dst pitch div rediv / re mok dimlst rmlst rtxt1 rtxt2 mktxt mokr1 mokr2)
  
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


;-----------------------------
; function : djdg_strdim
;            string dim
;            string을 이용하여 dimension을 만들어줌.
;            ex) 500+5@200+250
;            Yi Suk Jong
;            05/02/28
;-----------------------------
;>>> argument
; spnt :시작점.
; ang : 각도.
; ud : up/down
; dstr : sring ; Dimension string
;>>> return
; 마지막 점.
(defun djdg_strdim(spnt ang ud dstr / spnt ang ud dstr a i l ipnt)
  (setq dlst (divide_str dstr "+"))
;  (setq n (length str)) ;; 치수갯수.
  (setq ipnt spnt)
  
  (foreach a dlst
	(setq dlst1 (divide_str a "@"))
    (if (= (length dlst1) 2)
      (setq i (atoi (nth 0  dlst1))
	    l (atof (nth 1 dlst1)))
      (setq i 1
	    l (atof (car dlst1)))
    );if  
    (setq ipnt (f_da ipnt ang l i ud nil))
  );foreach  
  
);