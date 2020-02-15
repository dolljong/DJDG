;****************************************************************************
; Program : CHBDRTXT
;           Change Border TeXT
;           By Suk-Jong Yi
;           2006/09/23
;****************************************************************************
; Border의 표제란의 text를 주어진 text로 수정한다.
; 주로 도면번호 도면명등을 수정할 때 사용함.
; text위치는 insert scale이 적용되기 전의 두개의 위치 point 를 이용해 window box로 구한다.
; 입력:
; bordername	: border Block Name
; point1 	: text위치 point1
; point2 	: text위치 point2
; Newtext	: 새로운 text내용(맨 앞에 +,-가 붙은 경우 연산을 수행함)
;****************************************************************************

(defun C:CHBDRTXT( /
                    bn pnt1 pnt2 newtxt xg1 yg1 xg2 yg2 f_list ss_list
		  ss_num bdr_ent ipnt i_scale low_left up_right sstxt
		  txt_ent ass1 oldtxt str1 co entl1 
)

  
  (setq bn (getstring "\nBlock name: "))	;block 이름
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
  
  (setq ss_lst (ssget "X" f_list))                      ;entity 선택
  (setq ss_num (sslength ss_lst))                       ;선택된 entity갯수

  (if (> ss_num 1)
    (progn
      (alert "1개의 block이 있는 경우만 적용가능합니다")
      (exit)
    );progn  
    (progn
      (setq bdr_ent (entget (ssname ss_lst 0)))     ;border entity정보
      (setq ipnt (cdr (assoc 10 bdr_ent)))            ;border의 insert point
      (setq i_scale (cdr (assoc 41 bdr_ent)))         ;border의 scale factor
      (setq low_left (list (+ (car ipnt) (* xg1 i_scale))    ;border의 좌측 아래
                           (+ (cadr ipnt) (* yg1 i_scale)))
            up_right (list (+ (car ipnt) (* xg2 i_scale))    ;border의 우측 위
                           (+ (cadr ipnt) (* yg2 i_scale)))) ; true
      
      (setq sstxt (ssget "W" low_left up_right))	;text entity
      (setq txt_ent (entget (ssname sstxt 0)))     ;border entity정보
      

      (setq ass1 (assoc 1 txt_ent))         ;text내용 
      (setq oldtxt (cdr ass1))		;text내용
      
      (setq str1 (substr newtxt 1 1))	;첫번째 string
      
      (if (= str1  "+") (setq newtxt (itoa (+ (atoi oldtxt)
					      (atoi (substr newtxt 2 (1- (strlen newtxt))))))))
      (if (= str1  "-") (setq newtxt (itoa (- (atoi oldtxt)
					      (atoi (substr newtxt 2 (1- (strlen newtxt))))))))
      
      (setq co (cons (car ass1) newtxt))
      (setq entl1 (subst co ass1 txt_ent))
      (entmod entl1)                       ;새로운 크기로 업데이트
	
      
    );progn
  );if
  (princ "\nOK CHBDRTXT")
); defun

