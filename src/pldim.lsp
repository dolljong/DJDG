;-----------------------
; pldim : Pline dimension
;         polyline 치수선 만들기.
;         Yi Suk Jong
;         05/03/10
;----------------------
; function : djdg_dimtext2pvert : 주어진 두점으로부터 치수 text 삽입점과 각도를 구하고 text를 만들어줌.

(defun c:pldim(
	       / ds donutsz dimexe dimexo djdg_objtodimline djdg_objtodimlineds exlinelen
	         sel ent selp objpl defp defponpline deflen offsetdst dimpl objdimpl sp spdst spdp
	         spang spel1 epel1 ep epdst mp dst epdp mpdp epang spel2 epel2 dtxt
	       )

  ;(setq acobject (vla-get-object )

  (vl-load-com)

  (setq ds (getvar "dimscale")
	donutsz (* ds (getvar "dimasz"))   	;도넛 직경.
        dimexe (* ds (getvar "dimexe"))		;치수보조선이 치수위로 나간거리.
	dimexo (* ds (getvar "dimexo"))
	)	;치수보조선이 물체와 떨어진 거리.
  
  (setq djdg_objtodimline 20   ;object 에서 치수선까지 거리 mm단위.
        djdg_objtodimlineds (* djdg_objtodimline ds)   ;dimscale적용
	exlinelen (- djdg_objtodimlineds dimexo))	;치수보조선의 길이.
  
 
  (setq sel (entsel "\nSelect Pline: "))
  
  (setq ent (entget (car sel ))     ;선택 pline
	selp (cadr sel))            ;선택점.
  (setq objpl (vlax-ename->vla-object (car sel)))   ; pline의 activex object
  
  (setq defp (getpoint "\nSpecify first extension line origin or <Select Object>: "))
  (if defp
    (progn						;기준점을 입력했을 때
;      (setq sdp (getpoint defp "\nPick Side: "))
      (setq defponpline (vlax-curve-getClosestPointTo objpl defp ))	;def-point와 제일 가까운 pline상의 점.
      (setq deflen (distance defponpline defp))                         ;def-point와 pline의 거리.
      (setq offsetdst (+ djdg_objtodimlineds deflen))
      (command "offset"   offsetdst sel defp "")
      (setq dimpl (entlast)) 	;치수선 entity
    );progn
    (progn						;치수선이 이미 만들어져 있을 때
      (setq dimpl (car (entsel "\nSelect Dimension Pline: ")))
    );progn
  );if  
  
  (setq objdimpl (vlax-ename->vla-object dimpl))     ;dimension line의 activex object
  
  (setq sp (getpoint "\nPick Start Point: "))
  (setq spdst (vlax-curve-getDistAtPoint objpl sp)) ;시작점의 distance from start point
  (setq spdp (vlax-curve-getClosestPointTo objdimpl sp ))	;start point dim point
  (setq spang (angle sp spdp))					;시작점에서 시작점 치수점까지 도	
  (setq spel1 (polar spdp (+ spang pi) exlinelen))		;첫치수보조선의 시작점(dimexo적용)
  (setq epel1 (polar spdp spang dimexe))			;첫치수보조선의 끝점(dimexe적용).
  (push-os)
  (cecolor (if (numberp (getvar "dimclre")) (itoa (getvar "dimclre")) (getvar "dimclre") ))
  (command "line" spel1 epel1 "")		;첫 치수보조선 그리기.
  (popcolor)
  (command "donut" "0"  donutsz spdp "")	;도넛그리기(arrow)
  (push-os)

  ;두번째점부터 반복구간
  (while   (setq ep (getpoint "\nPick End point: "))
    (setq epdst (vlax-curve-getDistAtPoint objpl ep))  			;끝점의 거리.
    (if (= epdst nil)					;선택점이 pline상에 없을 때.
      (progn
	(setq ep (vlax-curve-getClosestPointTo objpl ep ))  ;선택점에서 가장 가까운 pline상의 점을 ep점으로..
	(setq epdst (vlax-curve-getDistAtPoint objpl ep))
      );progn	 
    );if  
    (setq mp (vlax-curve-getPointAtDist objpl (/ (+ spdst epdst) 2.0)))  ;pline상의 두점의 가운데점.
    (setq dst (abs (- epdst spdst)))   				;시작점과 마지막점 사이 길이.

    (setq epdp (vlax-curve-getClosestPointTo objdimpl ep )	;end point dim point
	  mpdp (vlax-curve-getclosestpointTo objdimpl mp))      ;mid point dim point

    (setq epang (angle ep epdp))					;끝점에서 끝점 치수점까지 각도.
  
    (setq spel2 (polar epdp (+ epang pi) exlinelen)		;두번째치수보조선의 시작점(dimexo적용)
	  epel2 (polar epdp epang dimexe))	;두번째치수보조선의 끝점(dimexo적용).
    (setq dtxt (rto_dimtxt dst))                  ;치수 string구하기.
  
    (push-os)
    (cecolor (if (numberp (getvar "dimclre")) (itoa (getvar "dimclre")) (getvar "dimclre") ))
    (command "line" spel2 epel2 "")		;두번째 치수보조선 그리기.
    (command "donut" "0"  donutsz epdp "")	;도넛그리기(arrow)
    (popcolor)	
;  (command "donut" "0"  donutsz mp mpdp "")	;도넛그리기(arrow).
    (cecolor (if (numberp (getvar "dimclrt")) (itoa (getvar "dimclrt")) (getvar "dimclrt") ))    
    (djdg_dimtext2pvert mp mpdp dtxt)		;치수쓰기.
    (popcolor)
    (pop-os)

    ;방금 그린 점의 정보를 첫점 정보로 저장. 
    ;저장내용 커브상의 거리.
    (setq spdst epdst)				;방금그린 점의 커브길이를 첫점 커브길이로 저장
  ); while
);defun


;------------------------------------
; function : djdg_dimtext2pvert
;            get dim text point
;            주어진 두점으로부터 치수 text 삽입점과 각도를 구하고 text를 만들어줌.
; arguments:
;     sp  > 시작점.
;     ep  > 끝점.(text삽입기준점) 치수 아래쪽.
;    txt  > text
; Returns:
;  
;------------------------------------
(defun djdg_dimtext2pvert(sp ep txt
			 / th ang wh4 ang2 tang txtpnt
			 )
  (setq th (* (getvar "dimtxt") (getvar "dimscale")))

  (setq ang (rem (+ (angle sp ep) (/ pi 2)) (* 2 pi))) 	;두점이 이루는 각.
  

  (setq wh4 (which4 ang))                              	;몇사분면에 있는가?

  (cond                                                	;1~4사분면에 있을 때
     ((= wh4 1)
       (setq ang2 ang)
       (setq tang (+ ang (* pi 0.5)))      
     )
     ((= wh4 2)
       (setq ang2 (- ang pi))
       (setq tang (- ang (* pi 0.5)))            
     )
     ((= wh4 3)
       (setq ang2 (- ang pi))
       (setq tang (- ang (* pi 0.5)))            
     )
     ((= wh4 4)
       (setq ang2 (- ang (* 2 pi)))
       (setq tang (+ ang (* pi 0.5)))            
     )
  );of cond
  
  (setq txtpnt (polar ep tang (* th 0.5)))  		;text point

  (command "TEXT" "J" "C" txtpnt th (rtod ang2) txt "")

);defun


;---------------------------
; program : PLN (Pline Length)
;           Pline 상의 두점의 거리를 돌려준다.
;           Yi Suk Jong
;           05/03/013
;---------------------------

(defun c:pln(
	    / plent plobj sp ep sp1 ep1 dstsp dstep dst
	    )
  (vl-load-com)
  (setq plent (car (entsel "\nSelect Pline or Arc: ")))   ;entity 입력받음
  (setq plobj (vlax-ename->vla-object plent))             ;object name으로 변환
  (setq sp (getpoint "\nPick start point: "))              ;시작점 입력.
  (setq ep (getpoint "\nPick end Point: "))		;끝점입력.
  (setq sp1 (vlax-curve-getclosestpointto plobj sp)	;첫점에서 가까운 pline상의 점
	ep1 (vlax-curve-getclosestpointto plobj ep))	;끝점에서 가까운 pline상의 점.
  (setq dstsp (vlax-curve-getdistatpoint plobj sp1)     ;첫점의 거리
	dstep (vlax-curve-getdistatpoint plobj ep1))    ;끝점의 거리
  (setq dst (abs (- dstep dstsp)))			;두점의 거리
  (princ "\nLength : ")(princ dst)(princ)
);defun  