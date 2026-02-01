;****************************************************************************
; Program : IBLK
;           Insert BLocK using block that was already inserted
;           By Suk-Jong Yi
;           2004/07/26
;****************************************************************************
; 주어진 블럭을 원하는 block을 insert(1파일당 1도면)
; 주로 도곽의 일정부분에 block을 insert시키는데 사함
;   ex) 도곽에 sign을 insert 시킬때
; bname : "borderch"    -- border명
; offset : "688,4"     -- insert위치
; ibname : "c:/program files/autocad 2002/blocks/sign"  -- insert될 block이름
;****************************************************************************

(defun C:IBLK( /
                    ds    dwgn    f_list   ss_lst   ss_num    index
                    pltn  pltn1   bdr_ent  ipnt     i_scale   up_left  low_right
)

  ;(push-env)                                        ;환경변수 대피


;;;(setq bname "borderbpo"
;;;      offset "688,4"
;;;      ibname "c:/program files/autocad 2002/blocks/sign1")

  (setq bname (getstring "\nInserted Block name: "))
  (setq offset (getstring "\nOffset(x,y): "))
  (setq ibname (getstring "\nBlock name to be inserted: "))

  (setq ibname (strcat (getvar "dwgprefix") ibname))
  
  (setq bn bname)
  (setq bnstr (divide_str bname ","))			;block namestr
  (setq nbn 	(length bnstr))				;number of block name
  (if (> nbn 1)
    (progn
  	(setq index 0
	      bnlst (list '(-4 . "<OR")))
  	(repeat nbn
    	  (setq bnlst (append bnlst (list (cons 2 (nth index bnstr)))))
    	  (setq index (1+ index))
  	);repeat
  	(setq bnlst (append bnlst (list '(-4 . "OR>"))))
        (setq f_list (append (list (cons 0 "INSERT")) bnlst))    ;filter list
      );True
    (progn
      (setq f_list (list (cons 0 "INSERT")  (cons 2 bn)))    ;filter list  
    );FLASE
  );if  
  (setq ss_lst (ssget "X" f_list))                      ;entity 선택
  (setq ss_num (sslength ss_lst))                       ;선택된 entity갯수


  ;--------- 첫번째 border부터 insert하기
  (setq index 0)                        ;첫번째 border부터
  (repeat ss_num                        ;선택된 border 갯수만큼 반복
    (setq bdr_ent (entget (ssname ss_lst index)))     ;border entity정보
    (setq ipnt (cdr (assoc 10 bdr_ent)))            ;border의 insert point
;    (setq bdrname (cdr (assoc 2 bdr_ent)))         ;border이름
    (setq i_scale (cdr (assoc 41 bdr_ent)))         ;border의 scale factor

    (if (> ss_num 0)  ;block이 있을 때
      (setq
             offsetv (divide_str offset ",")
	     xg1 (atof (car offsetv))
	     yg1 (atof (cadr offsetv))

	    ibpnt (list (+ (car ipnt) (* xg1 i_scale))        ;insertion point
                           (+ (cadr ipnt) (* yg1 i_scale)))
      );setq 	     
      (progn
	(princ "Block Not Found")
	(exit)
      );of false	
    );if


  (push-os)(command "INSERT" ibname ibpnt i_scale i_scale 0)(pop-os)

;    (setvar "CMDDIA" 1)                                     ;command echo ON
;    (princ pltn) (princ " is Plotted") (terpri)
    (setq index (1+ index))                                 ;다음 border로
  ) ;of repeat

;  (pop-env)                                                 ;환경변수 복귀

  (princ)
) ;of defun

  



;********************************************
; function : ipnt_nblk
;            insert point of nested block
;            Yi Suk-Jong
;            1999/7/15
;********************************************
; 기능 : 부모block의 table data를 뒤져 아들블록들의
;        삽입점들을 돌려준다. 도각의 프린트영역, 도면명영역,
;        도면변호영역 등을 찾기위한 함수이다.
; 넘어오는 값
;   pblkname : parent block name ;부모블록이름
;   sblkname : son block name    ;아들블록이름
; 넘어가는 값
;   ipnt_list : insert point list;아들블록의 삽입점(insert point기준 좌표)
; 예)
; (ipnt_nblk "BORDERKK" "$PUL")
;     -> borderkk라는 이름의 block안의 $PUL이란 블럭이
;        삽입된 점들을 리턴해준다.
;     -> 사용자는 위에서 얻어진 insert point값에 블록의 scale
;        값을 곱한 후 insert점좌표값에다 더하면 $PUL들의 절대좌표를
;        구할 수 있다.
;******************************

(defun ipnt_nblk(pblkname sblkname
/ tlbh bname t-1 t-list ipnt_list
)


  
  (setq tblh (tblsearch "BLOCK" pblkname))   ;table data head
  (setq base_point (cdr (assoc 10 tblh)))  ;block의 base point

  (setq t-1 (cdr (assoc -2 tblh)))           ;block내 첫 entity명

  (if (= (cdr (assoc 70 tblh)) 0)
    (setq bname sblkname)
    (setq bname (strcat pblkname "|" sblkname))
  );if  
  
  (setq ipnt_list nil)                      ;insert point list를 만듦

  (setq t-list (entget t-1))                ;첫번째 entity

  (if (and (= "INSERT" (cdr (assoc 0 t-list)))
;           (= (strcase (strcat pblkname "|" sblkname))
           (= (strcase bname)	   
              (cdr (assoc 2 t-list))))
      (setq ipnt_list
            (append ipnt_list
                    (list (mapcar '- (cdr (assoc 10 t-list)) base_point))))
  );if

  (while (setq t-1 (entnext t-1))           ;다음 entity
    (setq t-list (entget t-1))
    (if (and (= "INSERT" (cdr (assoc 0 t-list)))
;             (= (strcase (strcat pblkname "|" sblkname))
             (= (strcase bname)
	     (strcase (cdr (assoc 2 t-list)))))
      (setq ipnt_list
            (append ipnt_list
                    (list (mapcar '- (cdr (assoc 10 t-list)) base_point))))
    );if

  );while

  (setq ipnt_list ipnt_list)                    ;Return insert point list

);defun
