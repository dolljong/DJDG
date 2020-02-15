;**************************************************
; Program : MGEQ
;           Merge Equal and Value
;           Yi Suk-Jong
;           3/10/15
;**************************************************
; 'L=' text�� �ڿ� ������� '1.000'�� ���� ���� text�� ���ؼ� �ϳ��� �ؽ�Ʈ�� ������ش�. 
;  Attribute�� ����� ��� explode�� ���   
;**************************************************

(defun c:mgeq( / delentlst
	       )
  (setq ssdelent (ssadd))				;������ ent ss�����
  (setq sslst (ssget '((0 . "TEXT"))))                  ;text entity�� ����
  (setq numss (sslength sslst))                         ;ss-list�� ����
  (princ numss)
  (princ " text found\n")                               ;���õ� text����

  (setq count 0                                         ;ù text����
        tlst nil)

  (repeat numss                                             ;text������ŭ
    (setq ent (entget (ssname sslst count)))        	;text entity����
    (setq txt (cdr (assoc 1 ent)))                      ;text����
    (setq ipnt (cdr (assoc 10 ent)))    			;������
    (if (= (substr txt (strlen txt) 1) "=")
      (progn
        (setq th (cdr (assoc 40 ent)))    			;text ����
        (setq tbp1 (mapcar '+ (car (textbox ent)) ipnt)
              tbp2 (mapcar '+ (cadr (textbox ent)) ipnt))
	(setq tbpv1 (mapcar '+ tbp2 (list th 0 0))
	      tbpv2 (mapcar '+ tbpv1 (list (* th 2) (* -1 th) 0)))
	(setq ssvtxt (ssget "C" tbpv1 tbpv2))                ; ���ؽ�Ʈ ��Ƴ���
        (setq vent (entget (ssname ssvtxt 0)))
	(if (= (cdr (assoc 0 vent)) "TEXT")
	  (progn
	    (setq vtxt (cdr (assoc 1 vent)))
            (setq old1 (assoc 1 ent))                 ;old text����
            (setq new1 (cons 1 (strcat txt vtxt)))			;new text����
            (setq ent (subst new1 old1 ent))
            (entmod ent)          
	    (ssadd  (ssname ssvtxt 0) ssdelent)                         ;������ ent�̸��� ssdelent�� ����
	    
	  );progn  
	)  ;if
	      
      );progn	
    );if  
    (setq count (1+ count))                                 ;���� text��
  ) ;of repeat

  (setq count 0)
  (repeat (sslength ssdelent)
    (entdel (ssname ssdelent count))
    (setq count (1+ count))
  );repeat  

  
  
);defun       
