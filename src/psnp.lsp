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

(defun C:aa1( /
                    ds    dwgn    f_list   ss_lst   ss_num    index
                    pltn  pltn1   bdr_ent  ipnt     i_scale   up_left  low_right
)

  ;(push-env)                                        ;환경변수 대피


;;;(setq bname "borderbpo"
;;;      offset "688,4"
;;;      ibname "c:/program files/autocad 2002/blocks/sign1")

  (setq bdname "도곽(모델용)")
;  (setq bdname "도곽(우대현)")
;  (setq bdname "도곽(김유신)")
;  (setq bdname "도곽(김인순)")
;  (setq bdname "도곽(조근호)")
;  (setq bdname "도곽(선종구)")
;  (setq bdname "도곽(임바로)")
;  (setq bdname "도곽(유영민)")
;  (setq bdname "도곽(이광모)")
;  (setq bdname "도곽(최재익)")
;  (setq bdname "도곽(정창현)")                  

  (setq mbname (strcat (prefix) "blocks/" bdname ".dwg"))
  
  (setq ibpnt (getpoint "\nPick insert point: "))

  (setq ds (getvar "dimscale"))
  
  (push-os)(command "INSERT" mbname ibpnt ds ds 0)(pop-os)

  (setq bdr (entlast))

  
;;;  (setq ibname (getstring "\nBlock name to be inserted: "))
;;;
;;;  (setq ibname (strcat (getvar "dwgprefix") ibname))
;;;  
;;;  (setq bn bname)
;;;  (setq bnstr (divide_str bname ","))			;block namestr
;;;  (setq nbn 	(length bnstr))				;number of block name
;;;  (if (> nbn 1)
;;;    (progn
;;;  	(setq index 0
;;;	      bnlst (list '(-4 . "<OR")))
;;;  	(repeat nbn
;;;    	  (setq bnlst (append bnlst (list (cons 2 (nth index bnstr)))))
;;;    	  (setq index (1+ index))
;;;  	);repeat
;;;  	(setq bnlst (append bnlst (list '(-4 . "OR>"))))
;;;        (setq f_list (append (list (cons 0 "INSERT")) bnlst))    ;filter list
;;;      );True
;;;    (progn
;;;      (setq f_list (list (cons 0 "INSERT")  (cons 2 bn)))    ;filter list  
;;;    );FLASE
;;;  );if  
;;;  (setq ss_lst (ssget "X" f_list))                      ;entity 선택
;;;  (setq ss_num (sslength ss_lst))                       ;선택된 entity갯수
;;;
;;;
;;;  ;--------- 첫번째 border부터 insert하기
;;;  (setq index 0)                        ;첫번째 border부터
;;;  (repeat ss_num                        ;선택된 border 갯수만큼 반복
;;;    (setq bdr_ent (entget (ssname ss_lst index)))     ;border entity정보
;;;    (setq ipnt (cdr (assoc 10 bdr_ent)))            ;border의 insert point
;;;;    (setq bdrname (cdr (assoc 2 bdr_ent)))         ;border이름
;;;    (setq i_scale (cdr (assoc 41 bdr_ent)))         ;border의 scale factor
;;;
;;;    (if (> ss_num 0)  ;block이 있을 때
;;;      (setq
;;;             offsetv (divide_str offset ",")
;;;	     xg1 (atof (car offsetv))
;;;	     yg1 (atof (cadr offsetv))
;;;
;;;	    ibpnt (list (+ (car ipnt) (* xg1 i_scale))        ;insertion point
;;;                           (+ (cadr ipnt) (* yg1 i_scale)))
;;;      );setq 	     
;;;      (progn
;;;	(princ "Block Not Found")
;;;	(exit)
;;;      );of false	
;;;    );if
;;;
;;;
;;;  (push-os)(command "INSERT" ibname ibpnt i_scale i_scale 0)(pop-os)
;;;
;    (setvar "CMDDIA" 1)                                     ;command echo ON
;    (princ pltn) (princ " is Plotted") (terpri)
;;;    (setq index (1+ index))                                 ;다음 border로
;;;  ) ;of repeat

;  (pop-env)                                                 ;환경변수 복귀

  (princ)
) ;of defun

  

(defun c:aa2()

  (if (= llist nil) (get_filist))  ;list불러오기

  (setq fn (strcase (getvar "dwgname")))
  (setq fn (substr fn 1 (- (strlen fn) 4)))  
  (setq name (nth 4 (assoc fn llist)))

  (setq bdname (strcat "도곽(" name ")"))  
;  (setq bdname "도곽(우대현)")
;  (setq bdname "도곽(김유신)")
;  (setq bdname "도곽(김인순)")
;  (setq bdname "도곽(조근호)")
;  (setq bdname "도곽(선종구)")
;  (setq bdname "도곽(임바로)")
;  (setq bdname "도곽(유영민)")
;  (setq bdname "도곽(이광모)")
;  (setq bdname "도곽(최재익)")
;  (setq bdname "도곽(정창현)")                  
  (if (= bdr nil) (setq bdr (ssname (ssget "X" '((0 . "INSERT") (2 . "도곽(모델용)"))) 0)))
  (setq pbname (strcat (prefix) "blocks/" bdname ".dwg"))
  (setq ds (getvar "dimscale"))
  (setq dstxt (strcat "1/" (rtos ds 2 0) "XP"))
  (command "zoom" "e")  
  (command "layout" "s" "")
  (command "erase" "all" "")
  (command "insert" pbname "0,0" "1" "1" "0")
  (setq b (entlast))
  (command "explode" b) ; "")
  (command "mview" "0,0" "841,594")
  (command "mspace")
  (command "zoom" dstxt)
  (command "pspace")
  (command "ltscale" "10")
  (command "mspace")
  (command "erase"  bdr "")
  (command "layout" "s" "")
  (command "pspace")
);defun  

(defun c:aa3()

  (command "layout" "s" "")
  
  (if (= llist nil) (get_filist))  ;list불러오기

  (setq fn (strcase (getvar "dwgname")))
  (setq fn (substr fn 1 (- (strlen fn) 4)))  
  (setq name (nth 4 (assoc fn llist)))
;  

;  (setq bdname "도곽(우대현)")
;  (setq bdname "도곽(김유신)")
;  (setq bdname "도곽(김인순)")
;  (setq bdname "도곽(조근호)")
;  (setq bdname "도곽(선종구)")
;  (setq bdname "도곽(임바로)")
;  (setq bdname "도곽(유영민)")
;  (setq bdname "도곽(이광모)")
;  (setq bdname "도곽(최재익)")
;  (setq bdname "도곽(정창현)")                  

  (setq pbname (strcat (prefix) "blocks/도곽(" name ").dwg"))
  (setq ip (cdr (assoc 10 (entget (ssname (ssget "X" '((0 . "INSERT") (2 . "W")))0)))))
  
  (setq	ss (ssget "X"))  ;all
  (setq	ssv (ssget "X" '((0 . "VIEWPORT"))))
  (setq vs (ssname ss 0 ))
  (setq ss (ssdel vs ss))
;  (setq ip (cdr (assoc 10 (entget vs))))
  
  (command "erase" ss "")

;  (setq ds (getvar "dimscale"))
;  (setq dstxt (strcat "1/" (rtos ds 2 0) "XP"))
;  (command "zoom" "e")  
;  (command "layout" "s" "")
;  (command "erase" "all" "")
  
;  (command "insert" pbname "0,0" "1" "1" "0")
  (command "insert" pbname ip "1" "1" "0")  
  (setq b (entlast))
  (command "explode" b ) ;"")
;  (command "mview" "0,0" "841,594")
;  (command "mspace")
;  (command "zoom" dstxt)
;  (command "pspace")
  (command "ltscale" "10")
;  (command "mspace")
;  (command "erase"  bdr "")
;  (command "layout" "s" "")
;  (command "pspace")
);defun  


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


(defun c:setatt()
  
  (command "layout" "s" "")
  
  (if (= llist nil) (get_filist))
  
  ;(0 filename , 1 scale , 2 page , 3 subject)
  (setq fname_full (strcase (getvar "dwgname")))   ;파일명
  (setq fname (substr fname_full 1 (- (strlen fname_full) 4)))
  (setq finfo (assoc fname llist))

  (setq ndwgnum (strcat (substr fname 1 4) "-"
			 (substr fname 5 6) "-"
			 (substr fname 11 6) "-"
			 (substr fname 17 3) "-"
			 (substr fname 20 3)))

  (setq nscale (nth 1 finfo))
  
  (setq strdimscl (strcat "1 : " (rtos (getvar "dimscale") 2 0)))
  
  (setq npage (nth 2 finfo))
  (setq nsubject (nth 3 finfo))
  
  (setq	main_nb
	 (ssname (ssget "X" '((0 . "INSERT") (2 . "001"))) 0)
  )
  (setq ne (entnext main_nb))  ;next entity
  (repeat 23 (setq ne (entnext ne)))                 ;도면번호좌상단
  (djdg_entmod 1 ndwgnum ne)
  (repeat 10 (setq ne (entnext ne)))   ;scale
;  (djdg_entmod 1 nscale ne)
  (djdg_entmod 1 strdimscl ne)  
  (repeat 1 (setq ne (entnext ne)))   ;페이지
  (djdg_entmod 1 npage ne)
  (repeat 2 (setq ne (entnext ne)))   ;도면번호(우하단)
  (djdg_entmod 1 ndwgnum ne)
  (repeat 1 (setq ne (entnext ne)))   ;CAD파일명
  (djdg_entmod 1 fname ne)
  (repeat 1 (setq ne (entnext ne)))   ;도면명칭
  (djdg_entmod 1 nsubject ne)

;  (repeat 23 (setq ne (entnext ne)))                 ;도면번호좌상단  
);defun



(defun get_filist()
  (setq fn (strcat (prefix) "djdg/list.csv"))      ;file name입력  
;  (setq fn (strcat (prefix) "djdg/att.dat"))      ;file name입력
  (setq opf (open fn "r"))                          ;file open
      (if opf                                           ;file이 없는 경우
        (progn
           (setq count 1)
	   (setq llist nil)
           (while (setq ch (read-line opf))             ;한줄을 읽는다
;              (princ (chr 13))                          ;입력중 메세지 출력
;              (princ count)
;              (princ " Line Processing...")
              (setq inline (data-in ch))
;              (setq lst (cons                           ;문자 data를 숫자 data로
;			  (strcase (nth 0 inline))   ;subject
;			  inline)) ;filename (number)
;              (setq llist (append llist (list lst)))                   ;llist에 추가
              (setq llist (append llist (list inline)))                   ;llist에 추가
	      (setq count (1+ count))                   ;line번호 증가
           ) ;of while
        ) ;of progn
        (princ "\nFile not found")                          ;file이 없는 경우
      ) ;of if
      (close opf)                                           ;file close
);defun


(defun djdg_entmod(ass nvalue entname)
  (setq ent (entget entname))
  (setq ovalue (cdr (assoc ass ent)))
  (entmod (subst (cons ass nvalue) (cons ass ovalue)  ent))
  (entupd entname)
);defun  