;****************************************************************************
; Program : CHBDRTXT
;           Change Border TeXT
;           By Suk-Jong Yi
;           2006/09/23
;****************************************************************************
; Border�� ǥ������ text�� �־��� text�� �����Ѵ�.
; �ַ� �����ȣ �������� ������ �� �����.
; text��ġ�� insert scale�� ����Ǳ� ���� �ΰ��� ��ġ point �� �̿��� window box�� ���Ѵ�.
; �Է�:
; bordername	: border Block Name
; point1 	: text��ġ point1
; point2 	: text��ġ point2
; Newtext	: ���ο� text����(�� �տ� +,-�� ���� ��� ������ ������)
;****************************************************************************

(defun C:CHBDRTXT( /
                    bn pnt1 pnt2 newtxt xg1 yg1 xg2 yg2 f_list ss_list
		  ss_num bdr_ent ipnt i_scale low_left up_right sstxt
		  txt_ent ass1 oldtxt str1 co entl1 
)

  
  (setq bn (getstring "\nBlock name: "))	;block �̸�
  (push-os)
  (setq pnt1 (getpoint "\nPoint1: "))		;point1
  (setq pnt2 (getpoint "\nPoint2: "))		;point2
  (pop-os)
  (setq newtxt (getstring "\nNew Text: "))	;new text

  (setq xg1 (car pnt1)
	yg1 (cadr pnt1)
	xg2 (car pnt2)
	yg2 (cadr pnt2))

  (setq f_list (list (cons 0 "INSERT") (cons 2 bn)))    ;filter list
  
  (setq ss_lst (ssget "X" f_list))                      ;entity ����
  (setq ss_num (sslength ss_lst))                       ;���õ� entity����

  (if (> ss_num 1)
    (progn
      (alert "1���� block�� �ִ� ��츸 ���밡���մϴ�")
      (exit)
    );progn  
    (progn
      (setq bdr_ent (entget (ssname ss_lst 0)))     ;border entity����
      (setq ipnt (cdr (assoc 10 bdr_ent)))            ;border�� insert point
      (setq i_scale (cdr (assoc 41 bdr_ent)))         ;border�� scale factor
      (setq low_left (list (+ (car ipnt) (* xg1 i_scale))    ;border�� ���� �Ʒ�
                           (+ (cadr ipnt) (* yg1 i_scale)))
            up_right (list (+ (car ipnt) (* xg2 i_scale))    ;border�� ���� ��
                           (+ (cadr ipnt) (* yg2 i_scale)))) ; true
      
      (setq sstxt (ssget "W" low_left up_right))	;text entity
      (setq txt_ent (entget (ssname sstxt 0)))     ;border entity����
      

      (setq ass1 (assoc 1 txt_ent))         ;text���� 
      (setq oldtxt (cdr ass1))		;text����
      
      (setq str1 (substr newtxt 1 1))	;ù��° string
      
      (if (= str1  "+") (setq newtxt (itoa (+ (atoi oldtxt)
					      (atoi (substr newtxt 2 (1- (strlen newtxt))))))))
      (if (= str1  "-") (setq newtxt (itoa (- (atoi oldtxt)
					      (atoi (substr newtxt 2 (1- (strlen newtxt))))))))
      
      (setq co (cons (car ass1) newtxt))
      (setq entl1 (subst co ass1 txt_ent))
      (entmod entl1)                       ;���ο� ũ��� ������Ʈ
	
      
    );progn
  );if
  (princ "\nOK CHBDRTXT")
); defun

