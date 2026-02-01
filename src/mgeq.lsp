;**************************************************
; Program : MGEQ
;           Merge Equal and Value
;           Yi Suk-Jong
;           3/10/15
;**************************************************
; 'L=' text와 뒤에 따라오는 '1.000'과 같은 숫자 text를 합해서 하나의 텍스트로 만들어준다. 
;  Attribute를 사용한 경우 explode후 사용   
;**************************************************

(defun c:mgeq( / delentlst
	       )
  (setq ssdelent (ssadd))				;지워질 ent ss만들기
  (setq sslst (ssget '((0 . "TEXT"))))                  ;text entity만 선택
  (setq numss (sslength sslst))                         ;ss-list의 갯수
  (princ numss)
  (princ " text found\n")                               ;선택된 text갯수

  (setq count 0                                         ;첫 text부터
        tlst nil)

  (repeat numss                                             ;text갯수만큼
    (setq ent (entget (ssname sslst count)))        	;text entity정보
    (setq txt (cdr (assoc 1 ent)))                      ;text내용
    (setq ipnt (cdr (assoc 10 ent)))    			;삽입점
    (if (= (substr txt (strlen txt) 1) "=")
      (progn
        (setq th (cdr (assoc 40 ent)))    			;text 높이
        (setq tbp1 (mapcar '+ (car (textbox ent)) ipnt)
              tbp2 (mapcar '+ (cadr (textbox ent)) ipnt))
	(setq tbpv1 (mapcar '+ tbp2 (list th 0 0))
	      tbpv2 (mapcar '+ tbpv1 (list (* th 2) (* -1 th) 0)))
	(setq ssvtxt (ssget "C" tbpv1 tbpv2))                ; 값텍스트 잡아내기
        (setq vent (entget (ssname ssvtxt 0)))
	(if (= (cdr (assoc 0 vent)) "TEXT")
	  (progn
	    (setq vtxt (cdr (assoc 1 vent)))
            (setq old1 (assoc 1 ent))                 ;old text정보
            (setq new1 (cons 1 (strcat txt vtxt)))			;new text정보
            (setq ent (subst new1 old1 ent))
            (entmod ent)          
	    (ssadd  (ssname ssvtxt 0) ssdelent)                         ;지워질 ent이름을 ssdelent에 포함
	    
	  );progn  
	)  ;if
	      
      );progn	
    );if  
    (setq count (1+ count))                                 ;다음 text로
  ) ;of repeat

  (setq count 0)
  (repeat (sslength ssdelent)
    (entdel (ssname ssdelent count))
    (setq count (1+ count))
  );repeat  

  
  
);defun       
