;--------------------
;  program : ldd
;            load distributed
;            분포하중 그림을 그려줌.
;            Yi Suk Jong
;            5/04/16
;--------------------
(defun c:ldd()

  (setq arwlf 0.25   ;화살표의 길이/하중의 크기(길이)
	arwwf 0.25   ; 화살표의 폭/하중의 크기(길이)
	arwpf 1.0)   ;화살표간격/하중의 크기(길이)

  
  (setq pnt1 (getpoint "\n첫점: "))		; 처점 입력받기.
  (setq pnt2 (getpoint pnt1 "\n두번째점: "))	; 둘째점 입력받기.
  (setq pnt3 (getpoint pnt2 "\n하중 방향 및 크기 지정: "))  ;하중크기 및 방향 지정.

  (initget "X Y V")                ;x,y,vertical
  (setq kword (getkword "\nX-direction/Y-direction/Vertical: "))
  (setq text (getstring "\nText: "))

  (setq ang0 (angle pnt1 pnt2))			;첫점에서의 끝점으로의 각.
  
  (cond
    ((= kword "X")    ;X방향인 우
     (setq crsp (inters pnt3 (mapcar '+ pnt3 '(100 0 0)) pnt1 pnt2 nil))
     (setq ang (angle crsp pnt3))
    )
    ((= kword "Y")	;Y방향인 경우.
     (setq crsp (inters pnt3 (mapcar '+ pnt3 '(0 100 0)) pnt1 pnt2 nil))
     (setq ang (angle crsp pnt3))
    )
    ((= kword "V")	;Z방향인 경우.
     (setq ang (v_angle pnt1 pnt2 pnt3))
     (setq crsp (inters pnt1 pnt2 pnt3 (polar pnt3 ang 100) nil))
    )         
  );cond
  
  (setq ldlen (distance pnt3 crsp)) ;하중의 크기(길이).
  
  (setq arwl (* ldlen arwlf)		;화살표길이.
	arww (* ldlen arwwf)		;화살표폭.
	arwp (* ldlen arwpf))		;화살표간격

  ; 외곽선 그리기(pline)
  (setq opnt1 (polar pnt1 ang ldlen)   ;하중 첫점
	opnt2 (polar pnt2 ang ldlen))  ;하중 끝점.
  (command "pline" pnt1 opnt1 opnt2 pnt2 "")  ;외곽선 그리기.


  ;안쪽선 갯수 구하기.
  (cond
    ((= kword "X") (setq totl (abs (- (cadr pnt2) (cadr pnt1))))) ;X인 경우y차가 total 길이.
    ((= kword "Y") (setq totl (abs (- (car pnt2) (car pnt1))))) ;y인 경우x차가 total 길이.
    ((= kword "V") (setq totl (distance pnt1 pnt2)))            ;v인경우  두점의 거리.
  );cond  
  (setq n (atoi (rtos (/ totl arwp) 3 0)))[		;간격갯수(반올림)
  (setq narwp  (/ totl n))		;새로운 arwp

  ;line상의 거리 구하기
  (cond
    ((= kword "X") (setq diap (abs (/ narwp (sin ang0))))) 	;
    ((= kword "Y") (setq diap (abs (/ narwp (cos ang0))))) 	;
    ((= kword "V") (setq diap narwp))       			;
  );cond  
  

  ;첫화살표 마지막 화살표 그리기.
  (setq scl (/ arwl 2.5))
  (setq angd (rtod ang))  ;degree
  (push-os)
  (command "INSERT" (strcat (prefix) "blocks/" "ARW3") pnt1 scl scl angd)  ;첫번째 화살표.
  (command "INSERT" "ARW3" pnt2 scl scl angd)				;마지막 화살표.
  (pop-os)

  ;중간화살표/line 그리기.
  (setq idx 1)
  (repeat (1- n)
    (setq pnti (polar pnt1 ang0 (* idx diap))	;insert point
	  pntl (polar pnti ang ldlen))		;load line끝점.
    (push-os)
    (command "INSERT" "ARW3" pnti scl scl angd) ;중간화살표 삽입.
    (command "LINE" pnti pntl "")		;draw load line.
    (pop-os)
    (setq idx (1+ idx))				;다음 화살표로...
  );repeat
  
  ; text쓰기.
  (setq th (getvar "textsize")
	gap (* th 1.25))
  (setq angv (v_angle pnt1 pnt2 pnt3))  ;직각 Angle
;  (setq lenv (length pnt3 (inters pnt1 pnt2 pnt3 (polar pnt3 angv 100) nil)))   ;직각 길이.
  (setq txtp (polar (mid-point opnt1 opnt2) angv gap))
  (setq ta (ang4text opnt1 opnt2))    ;text angle

  (push-os)
  (command "TEXT" "J" "M" txtp th (rtod ta) text)
  (pop-os)
  
  ;(djdg_wtxtonline opnt1 opnt2 text th (* th 0.5))
  ;(setq pnt (polar crsp ang ldlen))
  ;(command "line" crsp pnt "")
);defun  
